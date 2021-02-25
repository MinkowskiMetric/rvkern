use crate::utils::*;
use crate::{PageFrameIndex, PhysicalAddress, RamPhysicalAddress};
use core::borrow::BorrowMut;
use core::convert::{TryFrom, TryInto};
use core::fmt;
use core::mem::MaybeUninit;
use core::ops::{Index, IndexMut};
use core::sync::atomic::{AtomicBool, Ordering};
use riscv::register::satp;
use spin::{Mutex, MutexGuard};

const HYPERSPACE_VA_START: VirtualAddress = VirtualAddress(0xc0000000);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MappingMode {
    Read,
    ReadWrite,
    ReadExecute,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemoryError {
    PageTableAllocationError,
    OutOfHyperspace,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct VirtualAddress(usize);

impl VirtualAddress {
    pub fn from_pointer<T>(ptr: *const T) -> Self {
        Self(ptr as usize)
    }

    pub fn from_ref<T>(rf: &T) -> Self {
        Self::from_pointer(rf as *const T)
    }

    pub fn addr(&self) -> usize {
        self.0
    }

    pub fn pd_index(&self) -> usize {
        self.0 >> 22
    }

    pub fn pt_index(&self) -> usize {
        (self.0 >> 12) & (1024 - 1)
    }
}

impl fmt::Debug for VirtualAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_fmt(format_args!("VirtualAddress({:#x})", self.addr()))
    }
}

impl<T> From<*const T> for VirtualAddress {
    fn from(ptr: *const T) -> Self {
        Self::from_pointer(ptr)
    }
}

impl<T> From<&T> for VirtualAddress {
    fn from(ptr: &T) -> Self {
        Self::from_ref(ptr)
    }
}

impl<T> From<&mut T> for VirtualAddress {
    fn from(ptr: &mut T) -> Self {
        Self::from_ref(ptr)
    }
}

impl TryFrom<usize> for VirtualAddress {
    type Error = core::convert::Infallible;
    fn try_from(u: usize) -> Result<Self, Self::Error> {
        Ok(Self(u))
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RawPageTableEntry(u32);

impl RawPageTableEntry {
    const PRESENT: u32 = 1 << 0;
    const READABLE: u32 = 1 << 1;
    const WRITEABLE: u32 = 1 << 2;
    const EXECUTABLE: u32 = 1 << 3;

    pub const fn zero() -> Self {
        Self(0)
    }

    pub const fn is_present(&self) -> bool {
        0 != self.0 & Self::PRESENT
    }

    pub const fn is_readable(&self) -> bool {
        0 != self.0 & Self::READABLE
    }

    pub const fn is_writeable(&self) -> bool {
        0 != self.0 & Self::WRITEABLE
    }

    pub const fn is_executable(&self) -> bool {
        0 != self.0 & Self::EXECUTABLE
    }

    pub const fn is_leaf(&self) -> bool {
        self.is_readable() || self.is_writeable() || self.is_executable()
    }
}

impl fmt::Debug for RawPageTableEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_fmt(format_args!("RawPageTableEntry({:#x})", self.0))
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PresentPageTableEntry(RawPageTableEntry);

impl PresentPageTableEntry {
    pub fn new_page_table_reference(pfi: PageFrameIndex) -> Self {
        Self(RawPageTableEntry(
            (pfi.pfi() << 10) | RawPageTableEntry::PRESENT,
        ))
    }

    pub fn new_page_reference(pfi: PageFrameIndex, mode: MappingMode) -> Self {
        let mode_bits = match mode {
            MappingMode::Read => RawPageTableEntry::PRESENT | RawPageTableEntry::READABLE,
            MappingMode::ReadWrite => {
                RawPageTableEntry::PRESENT
                    | RawPageTableEntry::READABLE
                    | RawPageTableEntry::WRITEABLE
            }
            MappingMode::ReadExecute => {
                RawPageTableEntry::PRESENT
                    | RawPageTableEntry::READABLE
                    | RawPageTableEntry::EXECUTABLE
            }
        };
        Self(RawPageTableEntry((pfi.pfi() << 10) | mode_bits))
    }

    pub fn physical_address(&self) -> PhysicalAddress {
        PageFrameIndex::new(self.0 .0 >> 10).physical_address()
    }
}

impl From<PresentPageTableEntry> for RawPageTableEntry {
    fn from(ppte: PresentPageTableEntry) -> Self {
        ppte.0
    }
}

impl TryFrom<RawPageTableEntry> for PresentPageTableEntry {
    type Error = &'static str;
    fn try_from(rpte: RawPageTableEntry) -> Result<Self, Self::Error> {
        if rpte.is_present() {
            Ok(Self(rpte))
        } else {
            Err("Page not present")
        }
    }
}

#[repr(C, align(4096))]
pub struct RawPageTable {
    entries: [RawPageTableEntry; 1024],
}

impl RawPageTable {
    pub const fn empty() -> Self {
        Self {
            entries: [RawPageTableEntry::zero(); 1024],
        }
    }

    fn mapper<'a>(&'a mut self, hyperspace: &'a mut Hyperspace) -> PageMapper<'a> {
        PageMapper {
            page_table: self,
            hyperspace,
        }
    }
}

impl Index<usize> for RawPageTable {
    type Output = RawPageTableEntry;

    fn index(&self, index: usize) -> &Self::Output {
        &self.entries[index]
    }
}

impl IndexMut<usize> for RawPageTable {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.entries[index]
    }
}

struct Hyperspace {
    page_table: &'static mut RawPageTable,
}

struct HyperspacePageMapping<'a> {
    parent: &'a mut Hyperspace,
    address: VirtualAddress,
}

impl<'a> HyperspacePageMapping<'a> {
    pub fn as_ptr<T>(&self) -> *const T {
        self.address.addr() as *const T
    }
    pub fn as_mut_ptr<T>(&mut self) -> *mut T {
        self.address.addr() as *mut T
    }
    pub unsafe fn as_ref<T>(&self) -> &T {
        &*self.as_ptr()
    }
    pub unsafe fn as_mut_ref<T>(&mut self) -> &mut T {
        &mut *self.as_mut_ptr()
    }
}

impl<'a> Drop for HyperspacePageMapping<'a> {
    fn drop(&mut self) {
        unsafe {
            self.parent.unmap_page(self.address);
        }
    }
}

impl Hyperspace {
    pub fn new(page_table: &'static mut RawPageTable) -> Self {
        Self { page_table }
    }

    pub unsafe fn map_page<'a>(
        &'a mut self,
        paddr: PhysicalAddress,
    ) -> Result<HyperspacePageMapping<'a>, MemoryError> {
        for (idx, page_entry) in self.page_table.entries.iter_mut().enumerate() {
            if !page_entry.is_present() {
                kprintln!("Found available hyperspace slot {}", idx);

                *page_entry = PresentPageTableEntry::new_page_reference(
                    paddr.page_frame_index(),
                    MappingMode::ReadWrite,
                )
                .into();

                return Ok(HyperspacePageMapping {
                    parent: self,
                    address: (HYPERSPACE_VA_START.0 + (idx * PAGE_SIZE))
                        .try_into()
                        .unwrap(),
                });
            }
        }

        Err(MemoryError::OutOfHyperspace)
    }

    unsafe fn unmap_page(&mut self, vaddr: VirtualAddress) {
        assert!(round_down_to_page(vaddr.addr()) == vaddr.addr());
        assert!(vaddr.pd_index() == HYPERSPACE_VA_START.pd_index());
        kprintln!("{:?} {:#x}", vaddr, vaddr.pt_index());
        assert!(vaddr.pt_index() < 1024);

        let pte = &mut self.page_table.entries[vaddr.pt_index()];
        assert!(pte.is_present());

        *pte = RawPageTableEntry::zero();
        riscv::asm::sfence_vma(0, vaddr.addr() as usize);
    }
}

struct PageMapper<'a> {
    page_table: &'a mut RawPageTable,
    hyperspace: &'a mut Hyperspace,
}

struct PageTableMapping<'a> {
    hyperspace_mapping: HyperspacePageMapping<'a>,
}

impl<'a> core::ops::Deref for PageTableMapping<'a> {
    type Target = RawPageTable;

    fn deref(&self) -> &Self::Target {
        unsafe { self.hyperspace_mapping.as_ref() }
    }
}

impl<'a> core::ops::DerefMut for PageTableMapping<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.hyperspace_mapping.as_mut_ref() }
    }
}

impl<'a> PageMapper<'a> {
    pub unsafe fn identity_map_physical_region(
        &mut self,
        start: *const u8,
        end: *const u8,
        mode: MappingMode,
    ) -> Result<(), MemoryError> {
        let start = VirtualAddress::from_pointer(start);
        let start_pa = PhysicalAddress::try_new(start.addr() as u64)
            .expect("Page must be in identity mapped memory");
        let end = VirtualAddress::from_pointer(end);
        let length = end.addr() - start.addr();

        self.identity_map_physical_region_by_address(start_pa, length, mode)
    }

    pub unsafe fn identity_map_physical_region_by_address(
        &mut self,
        start: PhysicalAddress,
        length: usize,
        mode: MappingMode,
    ) -> Result<(), MemoryError> {
        let (start, end) = (
            round_down_to_page(start.addr() as usize),
            round_up_to_page(start.addr() as usize + length),
        );

        for page in (start..end).step_by(PAGE_SIZE) {
            let page = VirtualAddress::try_from(page).expect("invalid address");
            kprintln!("IDENTITY MAP PAGE {:#x?} MODE {:?}", page, mode);

            let mut pt = self.ensure_pd_entry(page)?;

            let pt_phys = PhysicalAddress::try_new(page.addr() as u64)
                .expect("Page must be in identity mapped memory");
            pt.entries[page.pt_index()] =
                PresentPageTableEntry::new_page_reference(pt_phys.page_frame_index(), mode).into();
        }

        Ok(())
    }

    unsafe fn ensure_pd_entry<'b>(
        &'b mut self,
        page: VirtualAddress,
    ) -> Result<PageTableMapping<'b>, MemoryError> {
        let pde = &mut self.page_table[page.pd_index().into()];

        let hyperspace_mapping = if let Ok(pde_val) = PresentPageTableEntry::try_from(*pde) {
            self.hyperspace.map_page(pde_val.physical_address())?
        } else {
            let page_directory =
                crate::allocate_page().ok_or(MemoryError::PageTableAllocationError)?;

            // Currently we do not support this mapping failing. It is pretty straightforward to support it, I just
            // didn't because running out of hyperspace mapping should be a critical error anyway. All you need to do
            // is to fix the leak above
            let mut mapping = self
                .hyperspace
                .map_page(page_directory)
                .expect("Hyperspace mapping failed");

            // We need to ensure that the mapping is zeroed out
            let new_mapping_entries: &mut [MaybeUninit<RawPageTableEntry>; 1024] =
                mapping.as_mut_ref();
            for entry in new_mapping_entries.iter_mut() {
                entry.write(RawPageTableEntry::zero());
            }

            // Insert it into the page table
            *pde =
                PresentPageTableEntry::new_page_table_reference(page_directory.page_frame_index())
                    .into();

            // It is now safe to treat the page as if it contains a valid rust page table
            mapping
        };

        Ok(PageTableMapping { hyperspace_mapping })
    }
}

struct KernelVMImpl {
    root_page_table: &'static mut RawPageTable,
    hyperspace: Hyperspace,
}

static KERNEL_VM: Mutex<Option<KernelVMImpl>> = Mutex::new(None);

struct KernelVMLock<'a>(MutexGuard<'a, Option<KernelVMImpl>>);

impl<'a> core::ops::Deref for KernelVMLock<'_> {
    type Target = KernelVMImpl;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}

pub trait KernelVM {}

impl KernelVM for KernelVMImpl {}

pub unsafe fn init() {
    let mut lock = KERNEL_VM.lock();
    assert!(lock.is_none());

    extern "C" {
        static __text_start: u8;
        static __text_end: u8;
        static __rodata_start: u8;
        static __rodata_end: u8;
        static __data_start: u8;
        static __data_end: u8;
        static __bss_start: u8;
        static __bss_end: u8;
        static mut _s_boot_page_table: RawPageTable;
    }

    // So, we need to start by initializing our hyperspace page table
    static mut _G_HYPERSPACE_PAGE_TABLE: RawPageTable = RawPageTable::empty();
    let mut hyperspace = Hyperspace::new(&mut _G_HYPERSPACE_PAGE_TABLE);

    static mut _G_ROOT_PAGE_TABLE: RawPageTable = RawPageTable::empty();
    let root_page_table = &mut _G_ROOT_PAGE_TABLE;

    let hyperspace_page_table_phys =
        PhysicalAddress::try_new(&_G_HYPERSPACE_PAGE_TABLE as *const RawPageTable as u64)
            .expect("Hyperspace page table must be in identity mapped memory");

    // Map hyperspace into the boot page table, and the root page table
    _s_boot_page_table.entries[HYPERSPACE_VA_START.pd_index()] =
        PresentPageTableEntry::new_page_table_reference(
            hyperspace_page_table_phys.page_frame_index(),
        )
        .into();
    root_page_table.entries[HYPERSPACE_VA_START.pd_index()] =
        PresentPageTableEntry::new_page_table_reference(
            hyperspace_page_table_phys.page_frame_index(),
        )
        .into();

    {
        let mut mapper = root_page_table.mapper(&mut hyperspace);
        kprintln!("MAP TEXT");
        mapper
            .identity_map_physical_region(
                &__text_start as *const u8,
                &__text_end as *const u8,
                MappingMode::ReadExecute,
            )
            .expect("Failed to map kernel");
        kprintln!("MAP RODATA");
        mapper
            .identity_map_physical_region(
                &__rodata_start as *const u8,
                &__rodata_end as *const u8,
                MappingMode::Read,
            )
            .expect("Failed to map kernel");
        kprintln!("MAP DATA");
        mapper
            .identity_map_physical_region(
                &__data_start as *const u8,
                &__data_end as *const u8,
                MappingMode::ReadWrite,
            )
            .expect("Failed to map kernel");
        kprintln!("MAP BSS");
        mapper
            .identity_map_physical_region(
                &__bss_start as *const u8,
                &__bss_end as *const u8,
                MappingMode::ReadWrite,
            )
            .expect("Failed to map kernel");

        // This is kinda wrong, but we need it - map the uart into memory - will come up with a more permanent solution to that later
        mapper
            .identity_map_physical_region_by_address(
                PhysicalAddress::new_unchecked(0x10000000),
                PAGE_SIZE,
                MappingMode::ReadWrite,
            )
            .expect("Failed to map kernel");
    }

    // Switch over the the kernel page table
    let pt_phys = PhysicalAddress::try_new(root_page_table as *const RawPageTable as u64)
        .expect("Root page table must be in identity mapped memory");
    let pt_phys = RamPhysicalAddress::try_new(pt_phys).expect("Root page table must be in RAM");
    let pt_pfi = pt_phys.page_frame_index();

    kprintln!("SATP: {:#x}", satp::read().bits());
    satp::set(satp::Mode::Sv32, 0, pt_pfi.pfi() as usize);
    kprintln!("SATP2: {:#x}", satp::read().bits());
    panic!("ksjhgdskdf");

    *lock = Some(KernelVMImpl {
        root_page_table,
        hyperspace,
    });
}

pub unsafe fn kernel_vm<'a>() -> impl core::ops::Deref<Target = impl KernelVM> {
    KernelVMLock(KERNEL_VM.lock())
}
