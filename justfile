# =============
# Configuration
# =============

boss_target := "x86_64-uefi"
profile := "release"
boot_features := ","
emu_features := "log-trace,"

# =====================
# Derived configuration
# =====================

boot_target := if boss_target == "x86_64-uefi" {
    "x86_64-unknown-uefi"
} else {
    error("invalid boss_target")
}

emu_target := if boss_target == "x86_64-uefi" {
    "x86_64-unknown-none"
} else {
    error("invalid boss_target")
}

profile_dir := if profile == "release" { "release" } else { "debug" }
cargo_flags := "--profile " + profile
export RUSTFLAGS := "--cfg boss_target=\"" + boss_target + "\""

# =======
# Recipes
# =======

default: iso

clean:
    rm -rf target
    rm -rf .build
    rm -rf apps/.build

build_dir:
    mkdir -p .build

# Base image
bosbaima: build_dir
    mkdir -p .build/bosbaima
    date > .build/bosbaima/date
    just apps/ build base
    cp apps/.build/base/base.bop .build/bosbaima/
    tar cf .build/bosbaima.tar -C .build/bosbaima base.bop date
    zlib-flate -compress < .build/bosbaima.tar > .build/bosbaima.tar.zlib

# Main executable (emulator)
emu: build_dir
    nasm -f bin boss_common/src/target/x86_64_uefi/ap_boot.asm -o .build/ap_boot.bin
    cargo build {{cargo_flags}} --features {{emu_features}} --target {{emu_target}} -p boss_emu
    cp target/{{emu_target}}/{{profile_dir}}/boss_emu .build/boss_emu.fat.elf
    strip -s .build/boss_emu.fat.elf -o .build/boss_emu.elf
    zlib-flate -compress < .build/boss_emu.elf > .build/boss_emu.elf.zlib

# Bootloader
boot: build_dir bosbaima emu
    cargo build {{cargo_flags}} --features {{boot_features}} --target {{boot_target}} -p boss_boot
    cp target/{{boot_target}}/{{profile_dir}}/boss_boot.efi .build/

cargo_check:
    cargo check {{cargo_flags}} --features {{emu_features}} --target {{emu_target}}

cargo_clippy:
    cargo clippy {{cargo_flags}} --features {{emu_features}} --target {{emu_target}}

image_size := "65536" # sectors
image_size_mb := "32"
# Bootable ISO
iso: boot
    dd if=/dev/zero of=.build/boss.iso bs=1M count={{image_size_mb}} 2> /dev/null
    mformat -i .build/boss.iso -T {{image_size}}
    mmd -i .build/boss.iso ::/EFI
    mmd -i .build/boss.iso ::/EFI/BOOT
    mmd -i .build/boss.iso ::/BOSS
    mcopy -i .build/boss.iso .build/boss_boot.efi ::/EFI/BOOT/BOOTX64.EFI
    mcopy -i .build/boss.iso .build/bosbaima.tar ::/BOSS/BOSBAIMA.TAR

qemu_args := "-enable-kvm -drive if=pflash,format=raw,readonly=on,file=/usr/share/ovmf/x64/OVMF.4m.fd -device ahci,id=ahci -device ide-hd,drive=disk,bus=ahci.0 -drive if=none,id=disk,format=raw,file=.build/boss.iso -m 128 -smp 4,sockets=1,cores=2,threads=2 -boot menu=off,splash-time=0 -d int -D qemu.log -gdb tcp::1234"

# Boot ISO in QEMU
qemu: iso
    qemu-system-x86_64 {{qemu_args}} -serial stdio
qemu_pause: iso
    qemu-system-x86_64 {{qemu_args}} -serial stdio -S
qemu_mon: iso
    qemu-system-x86_64 {{qemu_args}} -monitor stdio
qemu_hot:
    qemu-system-x86_64 {{qemu_args}} -serial stdio

gdb:
    rust-gdb
