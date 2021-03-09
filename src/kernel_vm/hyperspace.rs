use super::{
    MappingMode, MemoryError, NotPresentPageTableEntry, PresentPageTableEntry, RawPageTable,
    RawPageTableEntry, VirtualAddress,
};
use crate::{utils::*, PhysicalAddress};
use core::convert::TryInto;

pub struct Hyperspace {
    base_address: VirtualAddress,
    page_table: &'static mut RawPageTable,
}

pub struct HyperspacePageMapping<'a> {
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
    pub fn new(base_address: VirtualAddress, page_table: &'static mut RawPageTable) -> Self {
        Self {
            base_address,
            page_table,
        }
    }

    pub unsafe fn map_page<'a>(
        &'a mut self,
        paddr: PhysicalAddress,
    ) -> Result<HyperspacePageMapping<'a>, MemoryError> {
        for (idx, page_entry) in self.page_table.iter_mut().enumerate() {
            if !page_entry.is_present() {
                *page_entry = PresentPageTableEntry::new_page_reference(
                    paddr.page_frame_index(),
                    MappingMode::ReadWrite,
                )
                .into();

                let address: VirtualAddress = (self.base_address.addr() + (idx * PAGE_SIZE))
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
        assert!(vaddr.pd_index() == self.base_address.pd_index());
        assert!(vaddr.pt_index() < 1024);

        let pte = &mut self.page_table[vaddr.pt_index()];
        assert!(pte.is_present());

        *pte = NotPresentPageTableEntry::zero().into();
        riscv::asm::sfence_vma(0, vaddr.addr() as usize);
    }
}
