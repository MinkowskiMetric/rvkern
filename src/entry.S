
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
    bne a1, zero, delay_app_processor

    la sp, STACK0
    call start

spin:
    j spin

bigstick:
.global bigstick
    j bigstick