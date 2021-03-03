use crate::utils::*;
use crate::{PageFrameIndex, PhysicalAddress, RamPhysicalAddress};
use core::convert::{TryFrom, TryInto};
use core::fmt;
use core::mem::MaybeUninit;
use core::ops::{Index, IndexMut};
use riscv::register::satp;
use spin::{Mutex, MutexGuard};

const HYPERSPACE_VA_START: VirtualAddress = VirtualAddress(0xc000_0000);

const SYSTEM_PTE_VA_START: VirtualAddress = VirtualAddress(0xd000_0000);

// How many system PTEs do we need? 64MB of kernel memory seems like a good figure, which is
// 16 page tables, or 16384 page table entries
const SYSTEM_PTE_PAGE_TABLES: usize = 16;
const SYSTEM_PTE_COUNT: usize = SYSTEM_PTE_PAGE_TABLES * 1024;
const SYSTEM_PTE_LENGTH: usize = SYSTEM_PTE_COUNT * PAGE_SIZE;
const SYSTEM_PTE_VA_END: VirtualAddress =
    VirtualAddress(SYSTEM_PTE_VA_START.addr() + SYSTEM_PTE_LENGTH);

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
    OutOfSystemPTEs,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct VirtualAddress(usize);

impl VirtualAddress {
    pub const fn from_addr(addr: usize) -> Self {
        Self(addr)
    }

    pub fn from_pointer<T>(ptr: *const T) -> Self {
        Self(ptr as usize)
    }

    pub fn from_ref<T>(rf: &T) -> Self {
        Self::from_pointer(rf as *const T)
    }

    pub const fn addr(&self) -> usize {
        self.0
    }

    pub fn pd_index(&self) -> usize {
        self.0 >> 22
    }

    pub fn pt_index(&self) -> usize {
        (self.0 >> 12) & (1024 - 1)
    }

    pub const fn as_ptr<T>(&self) -> *const T {
        self.0 as *const T
    }

    pub const fn as_mut_ptr<T>(&self) -> *mut T {
        self.0 as *mut T
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
#[derive(Debug)]
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
    phys_addr: PhysicalAddress,
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

    pub const fn phys_addr(&self) -> PhysicalAddress {
        self.phys_addr
    }

    pub const fn virt_addr(&self) -> VirtualAddress {
        self.address
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
                *page_entry = PresentPageTableEntry::new_page_reference(
                    paddr.page_frame_index(),
                    MappingMode::ReadWrite,
                )
                .into();

                let address: VirtualAddress = (HYPERSPACE_VA_START.0 + (idx * PAGE_SIZE))
                    .try_into()
                    .unwrap();

                return Ok(HyperspacePageMapping {
                    parent: self,
                    address,
                    phys_addr: paddr,
                });
            }
        }

        Err(MemoryError::OutOfHyperspace)
    }

    unsafe fn unmap_page(&mut self, vaddr: VirtualAddress) {
        assert!(round_down_to_page(vaddr.addr()) == vaddr.addr());
        assert!(vaddr.pd_index() == HYPERSPACE_VA_START.pd_index());
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

impl<'a> PageTableMapping<'a> {
    pub fn phys_addr(&self) -> PhysicalAddress {
        self.hyperspace_mapping.phys_addr()
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

pub struct KernelVM {
    root_page_table: &'static mut RawPageTable,
    hyperspace: Hyperspace,
    system_ptes: &'static mut [RawPageTableEntry],
}

static KERNEL_VM: Mutex<Option<KernelVM>> = Mutex::new(None);

pub struct KernelVMLock<'a>(MutexGuard<'a, Option<KernelVM>>);

impl<'a> core::ops::Deref for KernelVMLock<'_> {
    type Target = KernelVM;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().unwrap()
    }
}

impl<'a> core::ops::DerefMut for KernelVMLock<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut().unwrap()
    }
}

impl KernelVM {
    fn with_mapper<T, F: FnOnce(&mut PageMapper<'_>) -> Result<T, MemoryError>>(
        &mut self,
        f: F,
    ) -> Result<T, MemoryError> {
        let mut mapper = self.root_page_table.mapper(&mut self.hyperspace);
        f(&mut mapper)
    }

    pub fn map_physical_region(
        &mut self,
        base: PhysicalAddress,
        size: usize,
        mode: MappingMode,
    ) -> Result<VirtualAddress, MemoryError> {
        let alloc_base = round_down_to_page(base.addr() as usize);
        let alloc_limit = round_up_to_page(alloc_base + size);

        let pages = (alloc_limit - alloc_base) / PAGE_SIZE;
        let (addr, ptes) = self.find_available_ptes(pages)?;

        for (idx, pte) in ptes.iter_mut().enumerate() {
            *pte = PresentPageTableEntry::new_page_reference(
                PhysicalAddress::new((alloc_base + (idx * PAGE_SIZE)) as u64).into(),
                mode,
            )
            .into();
        }

        Ok(VirtualAddress(
            addr.addr() + alloc_base - base.addr() as usize,
        ))
    }

    fn find_available_ptes(
        &mut self,
        page_count: usize,
    ) -> Result<(VirtualAddress, &mut [RawPageTableEntry]), MemoryError> {
        let total_system_ptes = self.system_ptes.len();
        let mut idx = 0;
        while idx + page_count < total_system_ptes {
            // Skip over any occupied pages
            if self.system_ptes[idx].is_present() {
                // skip the page
                idx += 1;
            } else {
                let (start_idx, end_idx) = (idx, idx + page_count);
                let mut valid_range = true;

                for check_idx in (start_idx + 1)..end_idx {
                    if self.system_ptes[check_idx].is_present() {
                        idx = check_idx + 1;
                        valid_range = false;
                    }
                }

                if valid_range {
                    return Ok((
                        self.system_pte_index_to_virtual_address(start_idx),
                        &mut self.system_ptes[start_idx..end_idx],
                    ));
                }
            }
        }

        Err(MemoryError::OutOfSystemPTEs)
    }

    fn system_pte_index_to_virtual_address(&self, idx: usize) -> VirtualAddress {
        VirtualAddress(SYSTEM_PTE_VA_START.addr() + (idx * PAGE_SIZE))
    }
}

unsafe fn init_system_ptes(mapper: &mut PageMapper<'_>) -> &'static mut [RawPageTableEntry] {
    // Start by mapping the first page directory of the system PTEs
    let mut first_pd = mapper
        .ensure_pd_entry(SYSTEM_PTE_VA_START)
        .expect("Failed to create first system PTE PD");
    first_pd[0] = PresentPageTableEntry::new_page_reference(
        first_pd.phys_addr().into(),
        MappingMode::ReadWrite,
    )
    .into();

    // Now we do a dangerous thing which is to create a slice covering the whole of the
    // system ptes range. This is a little cheeky because it isn't all mapped yet, but we will fix that shortly
    let system_ptes: &mut [MaybeUninit<RawPageTableEntry>] =
        core::slice::from_raw_parts_mut(SYSTEM_PTE_VA_START.as_mut_ptr(), SYSTEM_PTE_COUNT);

    // Now that we've got all the system PTEs in one place, then we can fill in the rest
    for idx in 1..SYSTEM_PTE_PAGE_TABLES {
        let page_table = crate::allocate_page().expect("Failed to allocate system PTE page table");
        system_ptes[idx] = MaybeUninit::new(
            PresentPageTableEntry::new_page_reference(page_table.into(), MappingMode::ReadWrite)
                .into(),
        );
    }

    // Finally, zero out the remaining entries
    for idx in SYSTEM_PTE_PAGE_TABLES..SYSTEM_PTE_COUNT {
        system_ptes[idx] = MaybeUninit::new(RawPageTableEntry::zero());
    }

    // And now they're all initialized
    core::mem::transmute(system_ptes)
}

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

    let pt_phys = PhysicalAddress::try_new(root_page_table as *const RawPageTable as u64)
        .expect("Root page table must be in identity mapped memory");
    let pt_phys = RamPhysicalAddress::try_new(pt_phys).expect("Root page table must be in RAM");
    let pt_pfi = pt_phys.page_frame_index();

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

    let mut mapper = root_page_table.mapper(&mut hyperspace);
    mapper
        .identity_map_physical_region(
            &__text_start as *const u8,
            &__text_end as *const u8,
            MappingMode::ReadExecute,
        )
        .expect("Failed to map kernel");
    mapper
        .identity_map_physical_region(
            &__rodata_start as *const u8,
            &__rodata_end as *const u8,
            MappingMode::Read,
        )
        .expect("Failed to map kernel");
    mapper
        .identity_map_physical_region(
            &__data_start as *const u8,
            &__data_end as *const u8,
            MappingMode::ReadWrite,
        )
        .expect("Failed to map kernel");
    mapper
        .identity_map_physical_region(
            &__bss_start as *const u8,
            &__bss_end as *const u8,
            MappingMode::ReadWrite,
        )
        .expect("Failed to map kernel");

    // Switch over the the kernel page table
    satp::set(satp::Mode::Sv32, 0, pt_pfi.pfi() as usize);

    let system_ptes = init_system_ptes(&mut mapper);

    *lock = Some(KernelVM {
        root_page_table,
        hyperspace,
        system_ptes,
    });
}

pub fn kernel_vm<'a>() -> KernelVMLock<'a> {
    KernelVMLock(KERNEL_VM.lock())
}
