//const RHR: u32 = 0;
const THR: u32 = 0;
const IER: u32 = 1;
//const IER_RX_ENABLE: u8 = 0b00000001;
//const IER_TX_ENABLE: u8 = 0b00000010;
const FCR: u32 = 2;
const FCR_FIFO_ENABLE: u8 = 0b00000001;
const FCR_FIFO_CLEAR: u8 = 0b00000110;
//const ISR: u32 = 2;
const LCR: u32 = 3;
const LCR_EIGHT_BITS: u8 = 0b00000011;
const LCR_BAUD_LATCH: u8 = 0b10000000;
const LSR: u32 = 5;
//const LSR_RX_READY: u8 = 0b00000001;
const LSR_TX_IDLE: u8 = 0b00100000;

pub struct UART16650 {
    base_address: u32,
}

pub trait Uart {
    fn write_byte(&mut self, byte: u8);
}

impl UART16650 {
    fn new(base_address: u32) -> Self {
        let mut ret = Self { base_address };
        ret.init();
        ret
    }

    fn init(&mut self) {
        // Disable interrupts
        self.write_register(IER, 0);

        // Set the baud rate to 38.4K
        self.write_register(LCR, LCR_BAUD_LATCH);
        self.write_register(0, 0x03);
        self.write_register(1, 0x00);

        // Leave set baud mode, and set word length to 8 bits no parity
        self.write_register(LCR, LCR_EIGHT_BITS);
        // Reset and enable FIFOs
        self.write_register(FCR, FCR_FIFO_ENABLE | FCR_FIFO_CLEAR);
        // Enable TX and RX interrupts
        // TODOTODOTODO - I don't think I'm ready for interrupts just now
        //self.write_register(IER, IER_TX_ENABLE | IER_RX_ENABLE);
    }

    fn read_register(&mut self, offset: u32) -> u8 {
        let register = (self.base_address + offset) as *const u8;
        unsafe { core::ptr::read_volatile(register) }
    }

    fn write_register(&mut self, offset: u32, value: u8) {
        let register = (self.base_address + offset) as *mut u8;
        unsafe { core::ptr::write_volatile(register, value) }
    }
}

impl Uart for UART16650 {
    fn write_byte(&mut self, byte: u8) {
        // TODOTODOTODO - really need to disable interrupts in here!

        while self.read_register(LSR) & LSR_TX_IDLE == 0 {
            // Waiting for the UART to indicate it is ready to receive a byte
        }

        self.write_register(THR, byte);
    }
}

pub fn uart_init(base_address: u32) -> UART16650 {
    UART16650::new(base_address)
}
