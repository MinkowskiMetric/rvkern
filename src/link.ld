OUTPUT_ARCH( "riscv" )
ENTRY( _entry )

MEMORY
{
  ram  (wxa) : ORIGIN = 0x80000000, LENGTH = 128M
}

PHDRS
{
  text PT_LOAD;
  data PT_LOAD;
  bss PT_LOAD;
}

SECTIONS
{
    /*
    * ensure that entry.S / _entry is at 0x80000000,
    * where qemu's -kernel jumps.
    */
    . = 0x80000000;
    __kernel_start = .;

    .text : {
        ASSERT(. == 0x80000000, "Entry point not at correct address");
        __text_start = .;
        *(.text.entry);
        *(.text*)
        . = ALIGN(4096);
        __text_end = .;
    } >ram AT>ram :text

    .rodata : {
        __rodata_start = .;
        _global_pointer = .;
        *(.rodata*)
        . = ALIGN(4096);
        __rodata_end = .;
    } >ram AT>ram :text

    .data : {
        __data_start = .;
        *(.data*)
        *(.got*)
        . = ALIGN(4096);
        __data_end = .;
    } >ram AT>ram :data

    .tdata : {
        __tdata_start = .;
        *(.tdata*)
        . = ALIGN(4096);
        __tdata_end = .;
        __tbss_start = .;
        *(.tbss*)
        /* This is space for the PCB */
        . += 8;
        . = ALIGN(4096);
        __tbss_end = .;
    } >ram AT>ram :data

    .bss : {
        __bss_start = .;
        *(.bss*)
        . = ALIGN(4096);
        __bss_end = .;
    } >ram AT>ram :bss
    
    . = ALIGN(0x1000);
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
