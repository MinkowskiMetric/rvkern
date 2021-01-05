use super::uart;
use core::fmt;
use lazy_static::lazy_static;
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
                0x20..=0x7e => self.uart.write_byte(byte),
                b'\n' => self.uart.write_byte(b'\n'),
                _ => self.uart.write_byte(0xfe),
            }
        }

        Ok(())
    }
}

static CONSOLE: Mutex<Option<Console>> = Mutex::new(None);

pub fn init(uart: uart::UART16650) {
    *CONSOLE.lock() = Some(Console { uart });
}

#[doc(hidden)]
pub fn _kprint(args: fmt::Arguments) {
    use core::fmt::Write;
    CONSOLE.lock().as_mut().unwrap().write_fmt(args).unwrap()
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
