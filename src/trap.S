.option norvc

.section .text
.global s_trap_entry

s_trap_entry:
    # All interrupts and exceptions start out here. At this point we cannot
    # trust any of our registers because they might have come from user mode
    # and we cannot corrupt any register state. Interrupts ought to be disabled

    # Swap the sscratch register with tp
    csrrw tp, sscratch, tp
    bnez tp, _save_context

    # it is not clear how we handle a kernel mode stack overflow here. We're about to double fault in that
    # case.

    # sscratch was zero, so we need to restore the tp since we're still in kernel mode
    csrr tp, sscratch
    # Temporarily store the current stack pointer in the TLS
    sw sp, 0(tp)

_save_context:
    sw sp, 4(tp)
    lw sp, 0(tp)
    addi sp, sp, -140           # make space for the stack frame

    # x0 is the zero register, so no need to save that
    sw x1, 0(sp)                # link register
    # x2 is the stack pointer, which we already dealt with
    sw x3, 4(sp)                # global pointer
    # x4 is the thread pointer, which we already dealt with
    sw x5, 8(sp)                # t0
    sw x6, 12(sp)               # t1
    sw x7, 16(sp)               # t2
    sw x8, 20(sp)               # s0
    sw x9, 24(sp)               # s1
    sw x10, 28(sp)              # a0
    sw x11, 32(sp)              # a1
    sw x12, 36(sp)              # a2
    sw x13, 40(sp)              # a3
    sw x14, 44(sp)              # a4
    sw x15, 48(sp)              # a5
    sw x16, 52(sp)              # a6
    sw x17, 56(sp)              # a7
    sw x18, 60(sp)              # s2
    sw x19, 64(sp)              # s3
    sw x20, 68(sp)              # s4
    sw x21, 72(sp)              # s5
    sw x22, 76(sp)              # s6
    sw x23, 80(sp)              # s7
    sw x24, 84(sp)              # s8
    sw x25, 88(sp)              # s9
    sw x26, 92(sp)              # s10
    sw x27, 96(sp)              # s11
    sw x28, 100(sp)             # t3
    sw x29, 104(sp)             # t4
    sw x30, 108(sp)             # t5
    sw x31, 112(sp)             # t6

    # At this point make sure we disable user mode memory access and the FPU
    li t0, 0x08c00000

    # Reload the user stack pointer and store it in the trap frame along
    # with the status registers
    lw s0, 4(tp)
    csrrc s1, sstatus, t0
    csrr s2, sepc
    csrr s3, stval
    csrr s4, scause
    csrr s5, sscratch
    sw s0, 116(sp)
    sw s1, 120(sp)
    sw s2, 124(sp)
    sw s3, 128(sp)
    sw s4, 132(sp)
    sw s5, 136(sp)

    # Ensure the scratch register is 0 so we know that any exceptions came from kernel mode
    csrw sscratch, zero

    # Reload the global pointer
.option push
.option norelax
	la gp, __global_pointer$
.option pop

    # Since all exceptions and interrupts come back through the same path, we can
    # probably save some typing here (note that at this point it should be boldly
    # obvious that I'm copying how Linux does it even down to the symbol names)
    la ra, ret_from_exception

    # We need to check for interrupt versus exception because we want to turn interrupts
    # back on if it was an exception
    # The high bit of the cause determines whether it is an exception or an interrupt
    bge s4, zero, 1f

    # This is an interrupt, so leave interrupts disabled and go to the handler
    move a0, sp
    jal zero, handle_irq

1:
    # At this point interrupts are disabled. Since we're not handling an interrupt, we want
    # to turn them back on if they were enabled before.
    andi t0, s1, 0x20
    beqz t0, 1f

    csrs sstatus, 0x02

1:
    # Look for syscalls - we will eventually have special handling for them
    li t0, 8        # 8 is the exception vector for user mode sys calls
    beq s4, t0, handle_syscall

    # Otherwise, jump through the vector table
    slli t0, s4, 2              # multiply the vector by 4 to get an offset into the table
    la t1, exception_vector_table
    la t2, exception_vector_table_end
    move a0, sp
    add t0, t1, t0
    bgeu t0, t2, 1f
    lw t0, 0(t0)
    jr t0

1:
    tail do_trap_unknown

    
ret_from_exception:
17:
    wfi
    li ra, 42
    j 17b

handle_syscall:
    # Currently we don't support syscalls, so try and stop in a reasonably obvious way
    wfi
    li ra, 0xbaadf00d
    j handle_syscall

.section .rodata
.global exception_vector_table
exception_vector_table:
	.word do_trap_insn_misaligned
	.word do_trap_insn_fault
	.word do_trap_insn_illegal
	.word do_trap_break
	.word do_trap_load_misaligned
	.word do_trap_load_fault
	.word do_trap_store_misaligned
	.word do_trap_store_fault
	.word do_trap_ecall_u /* system call, gets intercepted */
	.word do_trap_ecall_s
	.word do_trap_unknown
	.word do_trap_ecall_m
	.word do_page_fault   /* instruction page fault */
	.word do_page_fault   /* load page fault */
	.word do_trap_unknown
	.word do_page_fault   /* store page fault */
exception_vector_table_end:
.end