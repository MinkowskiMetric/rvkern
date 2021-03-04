use crate::utils::*;
use crate::{allocate_page, PageFrameIndex, PhysicalAddress, RamPhysicalAddress};
use core::convert::{TryFrom, TryInto};
use core::fmt;
use core::mem::MaybeUninit;
use core::ops::{Index, IndexMut};
use riscv::register::satp;
use spin::{Mutex, MutexGuard};

mod hyperspace;
mod page_tables;
mod virtual_address;

pub(super) use hyperspace::{Hyperspace, HyperspacePageMapping};
pub use page_tables::MappingMode;
pub(super) use page_tables::{RawPageTable, RawPageTableEntry, PresentPageTableEntry, PageMapper,};
pub use virtual_address::{VirtualAddress};

const HYPERSPACE_VA_START: VirtualAddress = VirtualAddress::from_addr(0xc000_0000);

const SYSTEM_PTE_VA_START: VirtualAddress = VirtualAddress::from_addr(0xd000_0000);

// How many system PTEs do we need? 64MB of kernel memory seems like a good figure, which is
// 16 page tables, or 16384 page table entries
const SYSTEM_PTE_PAGE_TABLES: usize = 16;
const SYSTEM_PTE_COUNT: usize = SYSTEM_PTE_PAGE_TABLES * 1024;
const SYSTEM_PTE_LENGTH: usize = SYSTEM_PTE_COUNT * PAGE_SIZE;
const SYSTEM_PTE_VA_END: VirtualAddress =
    VirtualAddress::from_addr(SYSTEM_PTE_VA_START.addr() + SYSTEM_PTE_LENGTH);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemoryError {
    PageTableAllocationError,
    OutOfHyperspace,
    OutOfSystemPTEs,
    OutOfMemory,
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

        Ok(VirtualAddress::from_addr(
            addr.addr() + alloc_base - base.addr() as usize,
        ))
    }

    pub fn allocate(&mut self, bytes: usize, mode: MappingMode) -> Result<VirtualAddress, MemoryError> {
        let pages = round_up_to_page(bytes) / PAGE_SIZE;
        let (addr, ptes) = self.find_available_ptes(pages)?;

        for (idx, pte) in ptes.iter_mut().enumerate() {
            if let Some(new_page) = allocate_page() {
                *pte = PresentPageTableEntry::new_page_reference(new_page.into(), mode).into();
            } else {
                todo!("Handle memory allocation failure in page {}", idx);
            }
        }

        Ok(addr)
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
        VirtualAddress::from_addr(SYSTEM_PTE_VA_START.addr() + (idx * PAGE_SIZE))
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
    let mut hyperspace = Hyperspace::new(HYPERSPACE_VA_START, &mut _G_HYPERSPACE_PAGE_TABLE);

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
    _s_boot_page_table[HYPERSPACE_VA_START.pd_index()] =
        PresentPageTableEntry::new_page_table_reference(
            hyperspace_page_table_phys.page_frame_index(),
        )
        .into();
    root_page_table[HYPERSPACE_VA_START.pd_index()] =
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
