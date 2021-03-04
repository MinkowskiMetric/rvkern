use core::{fmt, mem::MaybeUninit, ops::{Index, IndexMut}, convert::TryFrom};
use crate::{allocate_page, PhysicalAddress, PageFrameIndex, utils::*, };
use super::{VirtualAddress, Hyperspace, HyperspacePageMapping, MemoryError};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MappingMode {
    Read,
    ReadWrite,
    ReadExecute,
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

    pub fn mapper<'a>(&'a mut self, hyperspace: &'a mut Hyperspace) -> PageMapper<'a> {
        PageMapper {
            page_table: self,
            hyperspace,
        }
    }

    pub fn iter<'a>(&'a self) -> core::slice::Iter<'a, RawPageTableEntry> {
        self.entries.iter()
    }

    pub fn iter_mut<'a>(&'a mut self) -> core::slice::IterMut<'a, RawPageTableEntry> {
        self.entries.iter_mut()
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

impl<'a> IntoIterator for &'a RawPageTable {
    type Item = &'a RawPageTableEntry;
    type IntoIter = core::slice::Iter<'a, RawPageTableEntry>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.iter()
    }
}

impl<'a> IntoIterator for &'a mut RawPageTable {
    type Item = &'a mut RawPageTableEntry;
    type IntoIter = core::slice::IterMut<'a, RawPageTableEntry>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.iter_mut()
    }
}

pub struct PageMapper<'a> {
    page_table: &'a mut RawPageTable,
    hyperspace: &'a mut Hyperspace,
}

pub struct PageTableMapping<'a> {
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

    pub unsafe fn ensure_pd_entry<'b>(
        &'b mut self,
        page: VirtualAddress,
    ) -> Result<PageTableMapping<'b>, MemoryError> {
        let pde = &mut self.page_table[page.pd_index().into()];

        let hyperspace_mapping = if let Ok(pde_val) = PresentPageTableEntry::try_from(*pde) {
            self.hyperspace.map_page(pde_val.physical_address())?
        } else {
            let page_directory =
                allocate_page().ok_or(MemoryError::PageTableAllocationError)?;

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