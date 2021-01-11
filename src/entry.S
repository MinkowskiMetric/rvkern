
# Disable generation of compressed instructions.
.option norvc

.section .text.entry
.global _entry
_entry:
    # for now we keep this very simple
    # If the hartid is 0, then we proceed else we spin
    csrr t0, mhartid
    bnez t0, app_processor_stall

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

    # Before we transfer into supervisor mode, we need to set up the initial paging environment for it.
    # We do this here - annoyingly in assembly language - because it simplifies things if we can make sure
    # that rust is always running in supervisor mode with paging enabled. One way we could simplify this 
    # would be if we had a stage 2 bootloader that set things up. I would do that, but it makes building harder 
    # which I don't want to do until I have no choice.
    la a0, __memory_start
    la a1, __memory_end
    call _identity_map_physical_region

    # For the moment the rust code assumes that the UART is physically mapped at 0x10000000. Let's not upset it too
    # much for now
    li a0, 0x10000000
    add a1, a0, 1
    call _identity_map_physical_region

    # Load the page table register and enable paging
    # Need to shift the page table address right to get the PFN.
    la t0, _s_root_page_table
    srl t0, t0, 12
    li t1, 0x80000000
    or t0, t0, t1
    csrw satp, t0
    
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

_identity_map_physical_region:
    # Compute the page directory index of the start of the kernel. We keep this
    # as an offset into the page table for convenience
    srl t1, a0, 22 - 2
    la t0, _s_root_page_table
    add t1, t1, t0

    # the shift here is necessary because the CPU has a 34 bit physical address space, so we need to take that
    # into account
    srl a0, a0, 2
    srl a1, a1, 2

    # We also need to shift the start and end registers
1:
    # Map it as read, write, execute, valid, and global
    li t0, 0b101111
    or t0, t0, a0
    sw t0, (t1)

    # Move the table position on
    addi t1, t1, 4
    li t0, (1024 * 4096) >> 2
    add a0, a0, t0
    
    # Check if we've reached the end
    bltu a0, a1, 1b

    # done
    ret

.section .bss
.balign 4096
.global _boot_stack
_boot_stack:
.fill 4096
_boot_stack_end:

.balign 4096
.global _s_root_page_table
_s_root_page_table:
.fill 4096