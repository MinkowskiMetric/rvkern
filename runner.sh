#!/bin/sh
cargo build
echo "Press ctrl-a followed by x to exit. Press ctrl-a followed by h to get monitor help. Press ctrl-a followed by c to get to the monitor, where you can quit"
qemu-system-riscv32 -kernel target/riscv32imac-unknown-none-elf/debug/rvkern -bios none -chardev stdio,mux=on,id=char0 -mon chardev=char0,mode=readline -serial chardev:char0 -smp cpus=2 -machine virt -nographic
