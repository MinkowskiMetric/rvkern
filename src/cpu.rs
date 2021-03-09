use crate::{kernel_vm, VirtualAddress};

global_asm! {include_str!("trap.S")}

#[thread_local]
static mut HELLO: u32 = 42;
#[thread_local]
static mut HELLO2: u32 = 0;
#[thread_local]
static mut HELLO4: [u32; 17] = [0; 17];

#[repr(C)]
struct KernelThreadControlBlock {
    kernel_stack: VirtualAddress,
}

pub unsafe fn init_bsp() {
    extern "C" {
        fn s_trap_entry();
        static __text_start: u8;
        static __text_end: u8;
        static __rodata_start: u8;
        static __rodata_end: u8;
        static __data_start: u8;
        static __data_end: u8;
        static __tdata_start: u8;
        static __tdata_end: u8;
        static __tbss_start: u8;
        static __tbss_end: u8;
        static __bss_start: u8;
        static __bss_end: u8;
    }

    // Allocate a stack for the boot CPU
    let kernel_stack = kernel_vm()
        .allocate_kernel_stack(16384)
        .expect("Failed to allocate BSP kernel stack");

    // From that stack, reserve enough memory for the TLS data
    let tls_data_size = (&__tbss_end as *const _ as usize) - (&__tdata_start as *const _ as usize);
    let tls_bss_offset =
        (&__tbss_start as *const _ as usize) - (&__tdata_start as *const _ as usize);

    let reserve_size = (tls_data_size + 15) & !15;
    let tcb_data_ptr = VirtualAddress::from_addr(kernel_stack.addr() - reserve_size);

    let tcb_data_src = &__tdata_start as *const _;
    let tcb_data_dest = tcb_data_ptr.as_mut_ptr();
    let tcb_bss_dest: *mut u8 =
        VirtualAddress::from_addr(tcb_data_ptr.addr() + tls_bss_offset).as_mut_ptr();

    core::ptr::copy(tcb_data_src, tcb_data_dest, tls_bss_offset);
    core::ptr::write_bytes(tcb_bss_dest, 0, tls_data_size - tls_bss_offset);

    // Now we can set the thread pointer
    asm! {
        "addi tp, {}, 0",
        in(reg) tcb_data_dest
    }

    // It is now safe to set the trap handler
    riscv::register::stvec::write(
        s_trap_entry as usize,
        riscv::register::stvec::TrapMode::Direct,
    );
}
