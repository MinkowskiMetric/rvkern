.option norvc

.section .text
.global s_trap_entry
s_trap_entry:
    # All interrupts and exceptions start out here. At this point we cannot
    # trust any of our registers because they might have come from user mode
    # and we cannot corrupt any register state. Interrupts ought to be disabled

    # How the hell do we work out whether we need to reload the stack or not?
    # 
    li a0, 0xbaadf00d
    wfi
    j s_trap_entry