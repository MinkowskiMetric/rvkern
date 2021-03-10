use super::kernel_vm;
use crate::{utils::*, VirtualAddress};
use core::alloc::Layout;
use core::ptr::NonNull;

trait StackTrampoline {
    fn stack_top(&self) -> VirtualAddress;
    fn call_on_stack(self) -> !;
}

struct CallStackTrampoline<F: FnOnce() -> !> {
    stack: VirtualAddress,
    f: F,
}

impl<F: FnOnce() -> !> StackTrampoline for CallStackTrampoline<F> {
    fn stack_top(&self) -> VirtualAddress {
        self.stack
    }

    fn call_on_stack(self) -> ! {
        (self.f)()
    }
}

pub struct KernelStack {
    alloc_addr: VirtualAddress,
    pages: usize,
    top: VirtualAddress,
}

impl KernelStack {
    pub(super) fn from_allocation(alloc_addr: VirtualAddress, pages: usize) -> Self {
        assert!(pages > 1, "page count should include guard page");
        Self {
            alloc_addr,
            pages,
            top: VirtualAddress::from_addr(alloc_addr.addr() + (pages * PAGE_SIZE)),
        }
    }

    pub fn top(&self) -> VirtualAddress {
        self.top
    }

    pub fn bottom(&self) -> VirtualAddress {
        VirtualAddress::from_addr(self.alloc_addr.addr() + PAGE_SIZE)
    }

    pub fn available_space(&self) -> usize {
        self.top().addr() - self.bottom().addr()
    }

    pub unsafe fn alloc(&mut self, layout: Layout) -> NonNull<u8> {
        // Make space
        let top_addr = align_down(self.top().addr() - layout.size(), layout.align());

        // Ensure there is actually space and account for it
        assert!(
            top_addr >= self.bottom().addr(),
            "Not enough space on stack for allocation"
        );
        self.top = VirtualAddress::from_addr(top_addr);

        // return it
        NonNull::new_unchecked(self.top.addr() as *mut u8)
    }

    pub unsafe fn call_on_stack(&mut self, f: unsafe extern "C" fn() -> !) -> ! {
        asm! {
            "add sp, {}, zero",
            "jalr zero, {}, 0",
            in(reg) self.top().addr(),
            in(reg) f,
            options(noreturn),
        }
    }
}

impl Drop for KernelStack {
    fn drop(&mut self) {
        todo!("Haven't done kernel stack dropping yet")
    }
}
