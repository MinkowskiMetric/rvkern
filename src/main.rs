#![no_std]
#![no_main]
#![feature(global_asm)]
#![feature(thread_local)]

#[macro_use]
mod kprint;
mod panic;
mod uart;

global_asm! {include_str!("entry.S")}

#[repr(align(16))]
pub struct StackHolder {
    stack: [u8; 16384],
}

#[no_mangle]
pub static mut STACK0: StackHolder = StackHolder { stack: [0; 16384], };

#[no_mangle]
pub unsafe extern "C" fn start() -> ! {
    static UART0_PHYSICAL_ADDRESS: u32 = 0x10000000;
    let uart0 = uart::uart_init(UART0_PHYSICAL_ADDRESS);
    kprint::init(uart0);

    kprintln!("Starting rvkern... {}", 0);

    unimplemented!("I haven't written any more kernel yet");
}
