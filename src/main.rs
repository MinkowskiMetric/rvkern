#![no_std]
#![no_main]
#![feature(asm, global_asm)]
#![feature(const_fn_transmute)]
#![feature(maybe_uninit_extra)]
#![feature(ptr_as_uninit)]
#![feature(thread_local)]

#[macro_use]
mod kprint;
mod panic;
mod physmem;
mod uart;

global_asm! {include_str!("entry.S")}

#[no_mangle]
pub unsafe extern "C" fn start() -> ! {

    static UART0_PHYSICAL_ADDRESS: u32 = 0x10000000;
    let uart0 = uart::uart_init(UART0_PHYSICAL_ADDRESS);
    kprint::init(uart0);

    kprintln!("Starting rvkern... {}", 0);
    physmem::init();

    unimplemented!("I haven't written any more kernel yet");
}
