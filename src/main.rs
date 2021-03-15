#![no_std]
#![no_main]
#![feature(asm, global_asm)]
#![feature(const_maybe_uninit_assume_init)]
#![feature(const_fn_transmute)]
#![feature(maybe_uninit_extra)]
#![feature(maybe_uninit_uninit_array)]
#![feature(never_type)]
#![feature(ptr_as_uninit)]
#![feature(thread_local)]
#![feature(try_blocks)]

#[macro_use]
mod kprint;

mod cpu;
mod kernel_vm;
mod panic;
mod physmem;
mod uart;
pub mod utils;

pub use kernel_vm::{kernel_vm, KernelStack, MappingMode, VirtualAddress};
pub use physmem::{
    allocate_page, available_pages, free_page, total_pages, PageFrameIndex, PhysicalAddress,
    RamPhysicalAddress,
};

global_asm! {include_str!("entry.S")}

#[no_mangle]
pub unsafe extern "C" fn start() -> ! {
    kernel_vm::init();
    kprint::init();
    physmem::init();

    cpu::init_bsp(continue_on_bsp)
}

unsafe extern "C" fn continue_on_bsp() -> ! {
    kernel_vm().allocate(1, MappingMode::ReadWrite).unwrap();

    kprintln!(
        "Kernel running - sort of - {} pages available out of {}",
        physmem::available_pages(),
        physmem::total_pages()
    );

    fn recurse(i: u32) -> u32 {
        recurse(i + 1)
    }

    recurse(0);

    unimplemented!("I haven't written any more kernel yet");
}
