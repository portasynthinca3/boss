symbol-file .build/boss_emu.fat.elf -o 0xffffc00000000000

set disassembly-flavor intel
# set disassemble-next-line on

set print pretty on
set pagination off
set confirm off
set history save on
tui layout src
tui window height src 15

target remote localhost:1234

hb core::panicking::panic_nounwind_fmt
hb _start
