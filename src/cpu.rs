use crate::{kernel_vm, KernelStack, VirtualAddress};
use core::mem::MaybeUninit;
use core::ptr::NonNull;

global_asm! {include_str!("trap.S")}

#[thread_local]
static mut HELLO: u32 = 42;
#[thread_local]
static mut HELLO2: u32 = 0;
#[thread_local]
static mut HELLO4: [u32; 17] = [0; 17];

#[repr(C)]
struct KernelThreadControlBlock {
    stack_scratch: VirtualAddress,
    kernel_interrupt: VirtualAddress,
    kernel_fault: VirtualAddress,
}

macro_rules! exception_handler {
    ($name:ident) => {
        #[no_mangle]
        unsafe extern "C" fn $name(cpu_state: &mut CpuState) {
            todo!("Exception {}: {:#x?}", stringify!($name), cpu_state)
        }
    };
}

exception_handler!(do_trap_insn_misaligned);
exception_handler!(do_trap_insn_fault);
exception_handler!(do_trap_insn_illegal);
exception_handler!(do_trap_break);
exception_handler!(do_trap_load_misaligned);
exception_handler!(do_trap_load_fault);
exception_handler!(do_trap_store_misaligned);
exception_handler!(do_trap_store_fault);
exception_handler!(do_trap_ecall_u);
exception_handler!(do_trap_ecall_s);
exception_handler!(do_trap_unknown);
exception_handler!(do_trap_ecall_m);
exception_handler!(do_page_fault);

#[repr(C)]
#[derive(Debug)]
struct CpuState {
    ra: usize,
    gp: usize,
    t0: usize,
    t1: usize,
    t2: usize,
    s0: usize,
    s1: usize,
    a0: usize,
    a1: usize,
    a2: usize,
    a3: usize,
    a4: usize,
    a5: usize,
    a6: usize,
    a7: usize,
    s2: usize,
    s3: usize,
    s4: usize,
    s5: usize,
    s6: usize,
    s7: usize,
    s8: usize,
    s9: usize,
    s10: usize,
    s11: usize,
    t3: usize,
    t4: usize,
    t5: usize,
    t6: usize,
    sp: usize,
    sstatus: usize,
    sepc: usize,
    stval: usize,
    scause: usize,
    sscratch: usize,
}

unsafe fn initialize_tls_data(
    kernel_stack: &mut KernelStack,
    fault_stack: &KernelStack,
) -> NonNull<KernelThreadControlBlock> {
    extern "C" {
        static __tdata_start: u8;
        static __tdata_end: u8;
        static __tbss_start: u8;
        static __tbss_end: u8;
    }

    // From that stack, reserve enough memory for the TLS data
    let tls_data_size = (&__tbss_end as *const _ as usize) - (&__tdata_start as *const _ as usize);
    let tls_bss_offset =
        (&__tbss_start as *const _ as usize) - (&__tdata_start as *const _ as usize);

    const TLS_DATA_ALIGN: usize = 16; // Should be enough

    let tls_data_layout = core::alloc::Layout::from_size_align(tls_data_size, TLS_DATA_ALIGN)
        .expect("Failed to generate TLS data layout");

    let tls_data_dest = kernel_stack.alloc(tls_data_layout);
    let tcb_data_dest = tls_data_dest.as_ptr();

    let tcb_data_src = &__tdata_start as *const _;
    let tcb_bss_dest = tcb_data_dest.offset(tls_bss_offset as isize);

    core::ptr::copy(tcb_data_src, tcb_data_dest, tls_bss_offset);
    core::ptr::write_bytes(tcb_bss_dest, 0, tls_data_size - tls_bss_offset);

    let mut tcb_ptr = tls_data_dest.cast();

    tcb_ptr.as_uninit_mut().write(KernelThreadControlBlock {
        stack_scratch: VirtualAddress::zero(),
        kernel_interrupt: kernel_stack.top(),
        kernel_fault: fault_stack.top(),
    });

    tcb_ptr
}

#[no_mangle]
unsafe extern "C" fn handle_irq(cpu_state: &mut CpuState) {
    todo!("handle_irq {:#x?}", cpu_state)
}

pub unsafe fn init_bsp(f: unsafe extern "C" fn() -> !) -> ! {
    extern "C" {
        fn s_trap_entry();
    }

    // Allocate a stack for the boot CPU
    let mut kernel_stack = kernel_vm()
        .allocate_kernel_stack(16384)
        .expect("Failed to allocate BSP kernel stack");

    let mut fault_stack = kernel_vm()
        .allocate_kernel_stack(8192)
        .expect("Failed to allocate BSP fault stack");

    let kernel_tcb = initialize_tls_data(&mut kernel_stack, &fault_stack);
    asm! {
        "addi tp, {}, 0",
        in(reg) kernel_tcb.as_ptr(),
    }

    // Ensure that while we're in kernel mode the sscratch register is 0
    riscv::register::sscratch::write(0);
    // And that accessing user mode memory is disabled
    riscv::register::sstatus::clear_sum();
    // And that floating point is disabled
    riscv::register::sstatus::set_fs(riscv::register::sstatus::FS::Off);
    riscv::register::sstatus::set_sie();

    // It is now safe to set the trap handler
    riscv::register::stvec::write(
        s_trap_entry as usize,
        riscv::register::stvec::TrapMode::Direct,
    );

    kernel_stack.call_on_stack(f)
}
