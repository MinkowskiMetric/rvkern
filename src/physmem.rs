use core::convert::TryFrom;
use core::fmt;
use core::mem::MaybeUninit;
use spin::Mutex;

// This module is responsible for maintaining the list of physical pages. Currently we have two
// states for physical pages - in use, and available. Since all physical pages are mapped in RAM
// we can use the available pages as a linked list. We do not use this mechanism to handle IO memory. That is mapped
// through a separate system.

const MIN_LEGAL_PAGE_FRAME: u32 = 0;
const INVALID_PAGE_FRAME: u32 = (0x200000000_u64 >> 12) as u32;
const MAX_LEGAL_PAGE_FRAME: u32 = INVALID_PAGE_FRAME - 1;

extern "C" {
    static __kernel_start: u8;
    static __kernel_end: u8;
    static __memory_start: u8;
    static __memory_end: u8;
}

fn kernel_start() -> PhysicalAddress {
    unsafe { PhysicalAddress::new_unchecked(&__kernel_start as *const u8 as u64) }
}

fn kernel_end() -> PhysicalAddress {
    unsafe { PhysicalAddress::new_unchecked(&__kernel_end as *const u8 as u64) }
}

fn ram_start() -> PhysicalAddress {
    unsafe { PhysicalAddress::new_unchecked(&__memory_start as *const u8 as u64) }
}

fn ram_end() -> PhysicalAddress {
    unsafe { PhysicalAddress::new_unchecked(&__memory_end as *const u8 as u64) }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PageFrameIndex(u32);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PhysicalAddress(u64);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RamPhysicalAddress(PhysicalAddress);

impl RamPhysicalAddress {
    pub fn try_new(addr: PhysicalAddress) -> Option<Self> {
        if addr >= ram_start() && addr < ram_end() {
            Some(Self(addr))
        } else {
            None
        }
    }

    pub fn new(addr: PhysicalAddress) -> Self {
        Self::try_new(addr).expect("Physical address is not in RAM")
    }

    pub const fn addr(&self) -> u64 {
        self.0.addr()
    }

    pub const fn physical_address(&self) -> PhysicalAddress {
        self.0
    }

    pub const fn page_frame_index(&self) -> PageFrameIndex {
        self.0.page_frame_index()
    }
}

impl PhysicalAddress {
    pub const MIN: Self = PageFrameIndex::MIN.physical_address();
    pub const MAX: Self = PageFrameIndex::MAX.physical_address();
    pub const INVALID: Self = PageFrameIndex::INVALID.physical_address();

    pub const fn try_new(addr: u64) -> Option<Self> {
        const MIN_ADDR: u64 = PhysicalAddress::MIN.addr();
        const MAX_ADDR: u64 = PhysicalAddress::MAX.addr();
        match addr {
            MIN_ADDR..=MAX_ADDR => Some(Self(addr)),
            _ => None,
        }
    }

    pub fn new(addr: u64) -> Self {
        Self::try_new(addr).expect("Invalid physical address")
    }

    pub const unsafe fn new_unchecked(addr: u64) -> Self {
        Self(addr)
    }

    pub const fn addr(&self) -> u64 {
        self.0
    }

    pub const fn page_frame_index(&self) -> PageFrameIndex {
        // Safety - this is safe because we know that a valid physical address must also be
        // a valid page frame index
        unsafe { PageFrameIndex::new_unchecked((self.0 >> 12) as u32) }
    }
}

impl PageFrameIndex {
    pub const MIN: Self = unsafe { Self::new_unchecked(MIN_LEGAL_PAGE_FRAME) };
    pub const INVALID: Self = unsafe { Self::new_unchecked(INVALID_PAGE_FRAME) };
    pub const MAX: Self = unsafe { Self::new_unchecked(MAX_LEGAL_PAGE_FRAME) };

    pub const unsafe fn new_unchecked(pfi: u32) -> Self {
        Self(pfi)
    }

    pub fn new(pfi: u32) -> Self {
        match pfi {
            MIN_LEGAL_PAGE_FRAME..=MAX_LEGAL_PAGE_FRAME => Self(pfi),
            _ => panic!("Invalid page frame index"),
        }
    }

    pub const fn physical_address(&self) -> PhysicalAddress {
        // Safety - this is safe because we know that a valid page frame must also be a valid
        // physical address
        unsafe { PhysicalAddress::new_unchecked((self.0 as u64) << 12) }
    }

    pub const fn pfi(&self) -> u32 {
        self.0
    }
}

impl From<PageFrameIndex> for PhysicalAddress {
    fn from(pfi: PageFrameIndex) -> Self {
        pfi.physical_address()
    }
}

impl From<PhysicalAddress> for PageFrameIndex {
    fn from(addr: PhysicalAddress) -> Self {
        addr.page_frame_index()
    }
}

impl From<PageFrameIndex> for RamPhysicalAddress {
    fn from(pfi: PageFrameIndex) -> Self {
        RamPhysicalAddress::new(pfi.physical_address())
    }
}

impl From<RamPhysicalAddress> for PhysicalAddress {
    fn from(rpa: RamPhysicalAddress) -> Self {
        rpa.physical_address()
    }
}

impl TryFrom<PhysicalAddress> for RamPhysicalAddress {
    type Error = &'static str;
    fn try_from(pa: PhysicalAddress) -> Result<Self, Self::Error> {
        Self::try_new(pa).ok_or("Physical address is not in RAM")
    }
}

impl<T> TryFrom<*mut T> for PhysicalAddress {
    type Error = &'static str;
    fn try_from(p: *mut T) -> Result<Self, Self::Error> {
        Self::try_new(p as u64).ok_or("Invalid physical address")
    }
}

impl fmt::Debug for PhysicalAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_fmt(format_args!("PhysicalAddress({:#x})", self.0))
    }
}

impl fmt::Debug for RamPhysicalAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_fmt(format_args!("RamPhysicalAddress({:#x})", self.addr()))
    }
}

impl fmt::Debug for PageFrameIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_fmt(format_args!("PageFrameIndex({:#x})", self.0))
    }
}

impl core::ops::Add<u32> for PageFrameIndex {
    type Output = Self;

    fn add(self, i: u32) -> Self::Output {
        Self::new(self.0 + i)
    }
}

struct PageFrameRange {
    pos: PageFrameIndex,
    end: PageFrameIndex,
}

impl Iterator for PageFrameRange {
    type Item = PageFrameIndex;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.end {
            None
        } else {
            let ret = self.pos;
            self.pos = ret + 1;
            Some(ret)
        }
    }
}

pub fn page_frame_index_range<Page: Into<PageFrameIndex>>(
    start: Page,
    end: Page,
) -> impl Iterator<Item = PageFrameIndex> {
    PageFrameRange {
        pos: start.into(),
        end: end.into(),
    }
}

struct FreePageEntry {
    next: Option<&'static mut FreePageEntry>,
}

struct FreePageList {
    head: Option<&'static mut FreePageEntry>,
    free_pages: usize,
}

static FREE_PAGE_LIST: Mutex<FreePageList> = Mutex::new(FreePageList {
    head: None,
    free_pages: 0,
});

pub fn free_pages() -> usize {
    FREE_PAGE_LIST.lock().free_pages
}

pub fn allocate_page() -> Option<PhysicalAddress> {
    let mut free_list = unsafe { FREE_PAGE_LIST.lock() };

    if let Some(page_head) = free_list.head.take() {
        // Reattach the rest of the list
        free_list.head = page_head.next.take();
        free_list.free_pages -= 1;

        // Now we need to get the pointer to the page
        let page_head = page_head as *mut FreePageEntry;
        let page_addr = PhysicalAddress::try_from(page_head).unwrap();

        unsafe {
            core::ptr::drop_in_place(page_head);
        }

        Some(page_addr)
    } else {
        None
    }
}

pub unsafe fn free_page(page: impl Into<PhysicalAddress>) {
    let mut free_list = FREE_PAGE_LIST.lock();

    let page = page.into();
    let page = page.addr() as *mut MaybeUninit<FreePageEntry>;

    let new_list = page.as_mut().unwrap().write(FreePageEntry {
        next: free_list.head.take(),
    });

    free_list.head = Some(new_list);
    free_list.free_pages += 1;
}

pub unsafe fn init() {
    kprintln!("RAM START: {:?} END: {:?}", ram_start(), ram_end());
    kprintln!("KERNEL START: {:?} END: {:?}", kernel_start(), kernel_end());

    assert!(
        kernel_start() == ram_start(),
        "Kernel should start at start of RAM"
    );
    assert!(
        kernel_end() < ram_end(),
        "Kernel should leave some RAM available"
    );

    for pfi in page_frame_index_range(ram_start(), ram_end()) {
        if pfi < kernel_start().page_frame_index() || pfi >= kernel_end().page_frame_index() {
            free_page(pfi);
        }
    }

    kprintln!(
        "Initialized physical memory with {} pages available",
        free_pages()
    );
    let (p1, p2) = (allocate_page(), allocate_page());
    kprintln!(
        "Allocated pages {:?} and {:?} (which comes after kernel_end {:?}",
        p1,
        p2,
        kernel_end()
    );
    kprintln!("{} pages available", free_pages());
    free_page(p1.unwrap());
    kprintln!("{} pages available", free_pages());
}
