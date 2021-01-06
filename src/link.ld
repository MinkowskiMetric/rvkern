OUTPUT_ARCH( "riscv" )
ENTRY( _entry )

SECTIONS
{
    /*
    * ensure that entry.S / _entry is at 0x80000000,
    * where qemu's -kernel jumps.
    */
    . = 0x80000000;

    .text : {
        __text_start = .;
        *(.text.entry);
        *(.text*)
        . = ALIGN(4096);
        __text_end = .;
    }

    .rodata : {
        __rodata_start = .;
        *(.rodata*)
        . = ALIGN(4096);
        __rodata_end = .;
    }

    .data : {
        __data_start = .;
        *(.data*)
        *(.got*)
        . = ALIGN(4096);
        __data_end = .;
    }

    .tdata : {
        __tdata_start = .;
        *(.tdata*)
        . = ALIGN(4096);
        __tdata_end = .;
        __tbss_start = .;
        *(.tbss*)
        . += 8;
        . = ALIGN(4096);
        __tbss_end = .;
    }

    .bss : {
        __bss_start = .;
        *(.bss*)
        . = ALIGN(4096);
        __bss_end = .;
    }
    
    . = ALIGN(0x1000);
    __kernel_end = .;
}
