
.section .text
.global hello
delay_app_processor:
    addi a1, zero, 1023
halt_spin:
    wfi
    j halt_spin
    
.section .text.entry
.global _entry
_entry:
    # for now we keep this very simple
    # If the hartid is 0, then we proceed else we spin
    csrr a1, mhartid
    bne a1, zero, 1f
    call delay_app_processor
1:
    la sp, STACK0
    li a0, 16384
    add sp, sp, a0
    call start

2:
    wfi
    j 2b