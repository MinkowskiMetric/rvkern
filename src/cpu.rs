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
    kernel_stack: VirtualAddress,
}

unsafe fn initialize_tls_data(kernel_stack: &mut KernelStack) -> NonNull<KernelThreadControlBlock> {
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
        kernel_stack: kernel_stack.top(),
    });

    tcb_ptr
}

pub unsafe fn init_bsp(f: unsafe extern "C" fn() -> !) -> ! {
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
    let mut kernel_stack = kernel_vm()
        .allocate_kernel_stack(16384)
        .expect("Failed to allocate BSP kernel stack");

    let kernel_tcb = initialize_tls_data(&mut kernel_stack);
    asm! {
        "addi tp, {}, 0",
        in(reg) kernel_tcb.as_ptr(),
    }

    kernel_stack.call_on_stack(f)
}
