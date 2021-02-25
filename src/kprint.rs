use super::uart;
use core::fmt;
use spin::Mutex;

// Damn. Can't do Box<dyn> if we don't have a memory manager. So we're going to have to
// hard code the concrete type here to make it global. Ah well.
struct Console {
    uart: uart::UART16650,
}

impl fmt::Write for Console {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        use uart::Uart;
        for byte in s.bytes() {
            match byte {
                b'\n' | 0x20..=0x7e => self.uart.write_byte(byte),
                _ => self.uart.write_byte(0xfe),
            }
        }

        Ok(())
    }
}

static CONSOLE: Mutex<Option<Console>> = Mutex::new(None);

pub fn init_pre_vm() {
    // The bootloader identity maps the UART into virtual memory for us, but that's silly
    // for various reasons, mostly because the identity mapping it puts the UART right in the
    // middle of user mode VA, so that will have to change. Because we think it would be useful
    // to be able to print things during vmem bringup though, we have this early initializer that
    // uses that identity mapping.
    static UART0_PHYSICAL_ADDRESS: u32 = 0x10000000;
    let uart = uart::uart_init(UART0_PHYSICAL_ADDRESS);

    *CONSOLE.lock() = Some(Console { uart });
}

pub fn init_post_vm() {
    unimplemented!()
}

#[doc(hidden)]
pub fn _kprint(args: fmt::Arguments) {
    use core::fmt::Write;
    CONSOLE
        .lock()
        .as_mut()
        .map(|console| console.write_fmt(args).unwrap());
}

#[macro_export]
macro_rules! kprint {
    ($($arg:tt)*) => ($crate::kprint::_kprint(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! kprintln {
    () => ($crate::kprint!("\n"));
    ($fmt:expr) => ($crate::kprint!(concat!($fmt, "\n")));
    ($fmt:expr, $($arg:tt)*) => ($crate::kprint!(
        concat!($fmt, "\n"), $($arg)*));
}
