#![no_std]
#![no_main]
#![feature(global_asm)]

#[macro_use]
mod kprint;
mod panic;
mod uart;

global_asm! {include_str!("entry.S")}

#[no_mangle]
pub static mut STACK0: [u8; 4096] = [0; 4096];

#[no_mangle]
pub unsafe extern "C" fn start() -> ! {
    static UART0_PHYSICAL_ADDRESS: u32 = 0x10000000;
    let uart0 = uart::uart_init(UART0_PHYSICAL_ADDRESS);
    kprint::init(uart0);

    kprintln!("Starting rvkern...");

    unimplemented!("I haven't written any more kernel yet");
}
