#![no_std]
#![no_main]
#![feature(global_asm)]

global_asm!{include_str!("entry.S")}

extern crate panic_halt;

#[no_mangle]
pub static mut STACK0: [u8;4096] = [0; 4096];

#[no_mangle]
pub unsafe extern "C" fn start() -> ! {
    loop { }
}
