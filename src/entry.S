
# Disable generation of compressed instructions.
.option norvc

.section .text.entry
.global _entry
_entry:
    # for now we keep this very simple
    # If the hartid is 0, then we proceed else we spin
    csrr t0, mhartid
    bnez t0, app_processor_stall

    # Ensure that paging is disabled in supervisor mode for now before we switch into it
    csrw satp, zero

    # And load the correct global pointer
    # TODOTODOTODO how can I verify this is working?
.option push
.option norelax
	la		gp, _global_pointer
.option pop

    # Ensure we zero out the BSS segments and the TBSS segments
    la a0, __bss_start
    la a1, __bss_end
    call _boot_zero_range

    la a0, __tbss_start
    la a1, __tbss_end
    call _boot_zero_range

    # Set up a stack
    la sp, _boot_stack_end
    
    # Now we need to call rust, but we also want to get into supervisor mode (we're currently in machine mode
    # which is not what we want). To do that, we set up as if we are returning to rust from a machine mode trap

    # We start by configuring mstatus so that when we do an eret it will pop supervisor mode off the stack.
    # This sets the MIE flag, which enables machine mode interrupts, then it sets MPIE which means interrupts were enabled
    # prior to the trap, then it sets the previous mode to b'01', so that when we return from machine mode we go into supervisor mode
    li t0, (0b01 << 11) | (1 << 7) | (1 << 3)
    csrw mstatus, t0
    # Set the address to return to when we return from machine mode
    la ra, 3f

    la t0, start
    csrw mepc, t0
    # Set the machine mode trap vector
    la t0, m_trap_handler
    csrw mtvec, t0
    # Enable interrupts in machine mode
    li t0, (1 << 3) | (1 << 7) | (1 << 11)
    csrw mie, t0

    mret

app_processor_stall:
3:
    wfi
    j 3b

m_trap_handler:
3:
    wfi
    j 3b

_boot_zero_range:
    bgeu a0, a1, 2f
1:
    sw zero, (a0)
    addi a0, a0, 4
    bltu a0, a1, 1b
2:
    ret

.section .bss
.balign 4096
.global _boot_stack
_boot_stack:
.fill 4096
_boot_stack_end:

