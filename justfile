clean:
    rm -rf target/x86_64-boss-uefi
    rm -rf .build
    rm -rf apps/.build
    mkdir .build

# Base image
bosbaima:
    mkdir -p .build/bosbaima
    date > .build/bosbaima/date
    just apps/ build base
    cp apps/.build/base/base.bop .build/bosbaima/
    tar cf .build/BOSBAIMA.TAR -C .build/bosbaima base.bop date

profile := "release"        # "dev" or "release"
profile_dir := "release"    # "debug" or "release"
features := ","
cargo_flags := "--target x86_64-boss-uefi.json -Zbuild-std=core,compiler_builtins,alloc -Zbuild-std-features=compiler-builtins-mem --profile " + profile + " --features " + features
magic_section_offset := "0x141000000"
reloc_section_offset := "0x141001000"
# EFI executable
emulator:
    cargo build {{cargo_flags}}
    cargo clippy {{cargo_flags}}
    # magic! (read mem_manager::reloc::relocate_pe for an explanation)
    dd if=/dev/random of=.build/rand bs=1 count=1024 2> /dev/null
    dd if=/dev/zero of=.build/zero bs=1 count=14 2> /dev/null
    cat .build/rand .build/rand > .build/reloc-magic
    objdump -hj.data target/x86_64-boss-uefi/{{profile_dir}}/boss.efi | tail -n+6 | head -n1 >> .build/reloc-magic
    cat .build/zero >> .build/reloc-magic
    objcopy target/x86_64-boss-uefi/{{profile_dir}}/boss.efi \
    	--add-section .reloc-magic=.build/reloc-magic \
    	--change-section-address .reloc-magic={{magic_section_offset}} \
    	--change-section-address .reloc={{reloc_section_offset}} \
    	.build/BOOTX64.EFI

image_size := "65536" # sectors
image_size_mb := "32"
# Bootable image
iso: emulator bosbaima
    dd if=/dev/zero of=.build/boss.iso bs=1M count={{image_size_mb}} 2> /dev/null
    mformat -i .build/boss.iso -T {{image_size}}
    mmd -i .build/boss.iso ::/EFI
    mmd -i .build/boss.iso ::/EFI/BOOT
    mmd -i .build/boss.iso ::/BOSS
    mcopy -i .build/boss.iso .build/BOOTX64.EFI ::/EFI/BOOT
    mcopy -i .build/boss.iso .build/BOSBAIMA.TAR ::/BOSS/BOSBAIMA.TAR    

# Boot 
qemu: iso
    qemu-system-x86_64 -enable-kvm \
        -drive if=pflash,format=raw,readonly=on,file=/usr/share/ovmf/x64/OVMF.fd \
        -device ahci,id=ahci \
        -device ide-hd,drive=disk,bus=ahci.0 \
        -drive if=none,id=disk,format=raw,file=.build/boss.iso \
        -m 128 \
        -smp 1,sockets=1,cores=1,threads=1 \
        -boot menu=off,splash-time=0 \
        -serial stdio
