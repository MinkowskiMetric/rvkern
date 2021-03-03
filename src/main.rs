#![no_std]
#![no_main]
#![feature(asm, global_asm)]
#![feature(const_maybe_uninit_assume_init)]
#![feature(const_fn_transmute)]
#![feature(maybe_uninit_extra)]
#![feature(maybe_uninit_uninit_array)]
#![feature(ptr_as_uninit)]
#![feature(thread_local)]
#![feature(try_blocks)]
#[macro_use]
mod kprint;

mod kernel_vm;
mod panic;
mod physmem;
mod uart;
pub mod utils;

pub use kernel_vm::{kernel_vm, MappingMode, VirtualAddress};
pub use physmem::{
    allocate_page, free_page, free_pages, PageFrameIndex, PhysicalAddress, RamPhysicalAddress,
};

global_asm! {include_str!("entry.S")}

#[no_mangle]
pub unsafe extern "C" fn start() -> ! {
    physmem::init();
    kernel_vm::init();
    kprint::init();

    unimplemented!("I haven't written any more kernel yet");
}
