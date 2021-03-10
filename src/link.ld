OUTPUT_ARCH( "riscv" )
ENTRY( _entry )

MEMORY
{
  ram  (wxa) : ORIGIN = 0x80000000, LENGTH = 128M
}

SECTIONS
{
    /*
    * ensure that entry.S / _entry is at 0x80000000,
    * where qemu's -kernel jumps.
    */
    . = 0x80000000;
    __kernel_start = .;
    __text_start = .;

    .text : {
        ASSERT(. == 0x80000000, "Entry point not at correct address");
        *(.text.entry);
        *(.text*)
    } 

    . = ALIGN(4096);
    __text_end = .;
    __rodata_start = .;

    .rodata : {
        . = ALIGN(4096);
        *(.rodata*)
    } 

    . = ALIGN(4096);
    __rodata_end = .;
    __data_start = .;

    .data : {
        . = ALIGN(4096);
        __data_start = .;
        *(.data*)
        *(.got*)
    } 

    . = ALIGN(16);
    __global_pointer$ = . + 0x800;

    .sdata : {

        *(.sdata)
        *(.sdata.*)
        *(.srodata.*)
        *(gnu.linkonce.s.*)
    } 

    .tdata : {
        __tdata_start = .;
        . = ALIGN(16);
        /* Allocate space for the TCB */
        . += 4;
        . = ALIGN(16);

        *(.tdata*)
        __tdata_end = .;
    } 

    .tbss : {
        __tbss_start = .;
        *(.tbss)
        *(.tbss.*)
        *(.gnu.linkonce.tb.*)
        . = ALIGN(16);
        __tbss_end = .;
    } 

    . = ALIGN(16);
    __bss_start = .;
    .sbss : {
        *(.dynsbss)
        *(.sbss .sbss.* .gnu.linkonce.sb.*)
        *(.scommon)
    }

    .bss : {
        *(.bss*)
    }
        
    . = ALIGN(4096);
    __bss_end = .;
    __data_end = .;
    __kernel_end = .;

    /* The goal here is to define all of RAM in one place, so the kernel will use these values
       to work out where physical memory is, and the bootloader will use them to map all of physical
       memory */
    __memory_start = ORIGIN(ram);
    __memory_end = ORIGIN(ram) + LENGTH(ram);

    /DISCARD/ : {
        *(.comment*)
        *(.eh_frame*)
        *(.gcc_except_table*)
        *(.note*)
        *(.rel.eh_frame*)
    }
}
