use crate::{kernel_vm, MappingMode, utils::*};
use core::convert::TryFrom;
use core::fmt;
use core::mem::MaybeUninit;
use spin::{Mutex, MutexGuard};

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

#[derive(Debug, Clone, Copy)]
enum PageFrameDatabaseEntry {
    Available { next: Option<usize> },
    InUse,
    Unknown,
}

fn get_initial_page_entry(addr: PhysicalAddress) -> PageFrameDatabaseEntry {
    if addr >= kernel_start() && addr < kernel_end() {
        PageFrameDatabaseEntry::InUse
    } else if addr >= ram_start() && addr < ram_end() {
        PageFrameDatabaseEntry::Available { next: None }
    } else {
        PageFrameDatabaseEntry::Unknown
    }
}

impl Default for PageFrameDatabaseEntry {
    fn default() -> Self {
        Self::Unknown
    }
}

struct PageFrameDatabaseZone {
    entries: &'static mut [PageFrameDatabaseEntry],
    available: usize,
    free_list_head: Option<usize>,
    base_address: PhysicalAddress,
}

trait PageZone {
    fn total_pages(&self) -> usize;
    fn available_pages(&self) -> usize;
    fn base(&self) -> PhysicalAddress;
    fn limit(&self) -> PhysicalAddress;

    fn contains_page(&self, page: PhysicalAddress) -> bool {
        page >= self.base() && page < self.limit()
    }
}

trait PageZoneMut: PageZone {
    fn allocate_page(&mut self) -> Option<PhysicalAddress>;
    fn free_page(&mut self, page: PhysicalAddress);

    fn try_free_page(&mut self, page: PhysicalAddress) -> bool {
        if self.contains_page(page) {
            self.free_page(page);
            true
        } else {
            false
        }
    }
}

impl<T: PageZone> PageZone for Option<T> {
    fn total_pages(&self) -> usize {
        self.as_ref().map(|zone| zone.total_pages()).unwrap_or(0)
    }

    fn available_pages(&self) -> usize {
        self.as_ref().map(|zone| zone.available_pages()).unwrap_or(0)
    }

    fn base(&self) -> PhysicalAddress {
        self.as_ref().map(|zone| zone.base()).unwrap_or(PhysicalAddress::new(0))
    }

    fn limit(&self) -> PhysicalAddress {
        self.as_ref().map(|zone| zone.limit()).unwrap_or(PhysicalAddress::new(0))
    }
}

impl<T: PageZoneMut> PageZoneMut for Option<T> {
    fn allocate_page(&mut self) -> Option<PhysicalAddress> {
        self.as_mut().and_then(|z| z.allocate_page())
    }
    fn free_page(&mut self, page: PhysicalAddress) {
        self.as_mut().unwrap().free_page(page)
    }
}

impl PageZone for PageFrameDatabaseZone {
    fn total_pages(&self) -> usize {
        self.entries.len()
    }

    fn available_pages(&self) -> usize {
        self.available
    }

    fn base(&self) -> PhysicalAddress {
        self.base_address
    }

    fn limit(&self) -> PhysicalAddress {
        PhysicalAddress(self.base().addr() + (self.total_pages() * PAGE_SIZE) as u64)
    }
}

impl PageZoneMut for PageFrameDatabaseZone {
    fn allocate_page(&mut self) -> Option<PhysicalAddress> {
        if let Some(free_page_index) = self.free_list_head {
            match self.entries[free_page_index] {
                PageFrameDatabaseEntry::Available { next } => {
                    self.free_list_head = next;
                    self.available -= 1;

                    Some(self.page_index_to_physical_address(free_page_index))
                }

                entry => panic!(
                    "Expected available page in PFDB entry {}, found {:?}",
                    free_page_index, entry
                ),
            }
        } else {
            None
        }
    }

    fn free_page(&mut self, addr: PhysicalAddress) {
        unimplemented!()
    }
}

impl PageFrameDatabaseZone {
    fn page_index_to_physical_address(&self, idx: usize) -> PhysicalAddress {
        PhysicalAddress::new(self.base_address.addr() + (idx * PAGE_SIZE) as u64)
    }
}

struct PageFrameDatabaseLock<'a>(MutexGuard<'a, Option<PageFrameDatabaseZone>>);

impl<'a> core::ops::Deref for PageFrameDatabaseLock<'_> {
    type Target = PageFrameDatabaseZone;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}

impl<'a> core::ops::DerefMut for PageFrameDatabaseLock<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().unwrap()
    }
}

fn initial_zone<'a>() -> PageFrameDatabaseLock<'a> {
    static ZONE: Mutex<Option<PageFrameDatabaseZone>> = Mutex::new(None);

    let mut lock = ZONE.lock();
    lock.get_or_insert_with(|| {
        const INITIAL_ZONE_PAGES: usize = 1024; // Enough for 4MB
        static mut INITIAL_ZONE_ENTRIES: [core::mem::MaybeUninit<PageFrameDatabaseEntry>;
            INITIAL_ZONE_PAGES] = core::mem::MaybeUninit::uninit_array();

        let base_address = ram_start();

        let entries = unsafe { &mut INITIAL_ZONE_ENTRIES[0..INITIAL_ZONE_PAGES] };
        let mut available = 0;
        let mut free_list_head = None;
        for (idx, entry) in entries.iter_mut().enumerate() {
            let addr = PhysicalAddress(base_address.addr() + (idx * PAGE_SIZE) as u64);
            *entry = MaybeUninit::new(match get_initial_page_entry(addr) {
                PageFrameDatabaseEntry::Available { .. } => {
                    available += 1;
                    PageFrameDatabaseEntry::Available {
                        next: free_list_head.replace(idx),
                    }
                }

                c => c,
            });
        }

        let entries: &mut [PageFrameDatabaseEntry] = unsafe { core::mem::transmute(entries) };

        PageFrameDatabaseZone {
            entries,
            available,
            free_list_head,
            base_address,
        }
    });
    PageFrameDatabaseLock(lock)
}

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

    pub const fn round_down_to_page(&self) -> PhysicalAddress {
        self.page_frame_index().physical_address()
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

unsafe fn create_ram_range(base: PhysicalAddress, limit: PhysicalAddress) -> PageFrameDatabaseZone {
    let (base, limit) = (round_up_to_page(base.addr() as usize), round_down_to_page(limit.addr() as usize));
    let pages = (limit - base) / PAGE_SIZE;
    let entry_bytes = round_up_to_page(core::mem::size_of::<PageFrameDatabaseEntry>() * pages);
    let vm_reservation = kernel_vm().allocate(entry_bytes, MappingMode::ReadWrite).expect("Failed to allocate memory for page frame database");

    let entries: &mut [core::mem::MaybeUninit<PageFrameDatabaseEntry>] = core::slice::from_raw_parts_mut(vm_reservation.as_mut_ptr(), pages);
    let mut available = 0;
    let mut free_list_head = None;

    for (idx, entry) in entries.iter_mut().enumerate() {
        let addr = PhysicalAddress((base + (idx * PAGE_SIZE)) as u64);
        *entry = MaybeUninit::new(match get_initial_page_entry(addr) {
            PageFrameDatabaseEntry::Available { .. } => {
                available += 1;
                PageFrameDatabaseEntry::Available {
                    next: free_list_head.replace(idx),
                }
            }

            c => c,
        });
    }

    let entries = core::mem::transmute(entries);

    PageFrameDatabaseZone {
        entries,
        available,
        free_list_head,
        base_address: PhysicalAddress::new(base as u64),
    }
}

static EXTRA_RANGE_ZONE: Mutex<Option<PageFrameDatabaseZone>> = Mutex::new(None);

pub fn total_pages() -> usize {
    initial_zone().total_pages() + EXTRA_RANGE_ZONE.lock().total_pages()
}
pub fn available_pages() -> usize {
    initial_zone().available_pages() + EXTRA_RANGE_ZONE.lock().available_pages()
}

pub fn allocate_page() -> Option<PhysicalAddress> {
    { EXTRA_RANGE_ZONE.lock().allocate_page() }.or_else(|| initial_zone().allocate_page())
}

pub unsafe fn free_page(page: impl Into<PhysicalAddress>) {
    let page = page.into();
    
    if !EXTRA_RANGE_ZONE.lock().try_free_page(page) {
        initial_zone().free_page(page);
    }
}

pub unsafe fn init() {
    // The initial zone is "self initializing", but as soon as we're able to allocate memory, we run into a need to
    // initialize the rest of physical memory
    let extra_range_base = initial_zone().limit();
    let extra_range = create_ram_range(extra_range_base, ram_end());
    *EXTRA_RANGE_ZONE.lock() = Some(extra_range);
}
