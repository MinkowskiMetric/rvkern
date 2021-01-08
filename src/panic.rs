use core::panic::PanicInfo;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    // TODOTODOTODO - this needs to be more complicated.
    kprintln!("{}", info);

    loop {
        unsafe {
            asm!("wfi", options(nomem));
        }
    }
}
