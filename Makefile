SHELL=bash

default: esp

clean:
	rm -rf target/x86_64-boss-uefi
	rm -rf build
	mkdir build

# Emulator (the EFI executable)
PROFILE=release
PROFILE_DIR=release
CARGOFLAGS=--target x86_64-boss-uefi.json -Zbuild-std=core,compiler_builtins,alloc -Zbuild-std-features=compiler-builtins-mem
MAGIC_SECTION_OFFSET=0x141000000
RELOC_SECTION_OFFSET=0x141001000
emu:
	cargo build $(CARGOFLAGS) --profile $(PROFILE)
	cargo clippy $(CARGOFLAGS) --profile $(PROFILE)
# magic! (read mem_manager::reloc::relocate_pe for an explanation)
	dd if=/dev/random of=build/rand bs=1 count=1024
	dd if=/dev/zero of=build/zero bs=1 count=1
	cat build/rand build/rand > build/reloc-magic
	objdump -hj.data target/x86_64-boss-uefi/$(PROFILE_DIR)/boss.efi | tail -n+6 | head -n1 >> build/reloc-magic
	cat build/zero >> build/reloc-magic
	objcopy target/x86_64-boss-uefi/$(PROFILE_DIR)/boss.efi \
		--add-section .reloc-magic=build/reloc-magic \
		--change-section-address .reloc-magic=$(MAGIC_SECTION_OFFSET) \
		--change-section-address .reloc=$(RELOC_SECTION_OFFSET) \
		build/emulator.efi

# Erlang
ERL_SOURCES=base/main.erl
ERLC_FLAGS=-b beam
ERL_OUT=build/ebin
ebin: $(ERL_SOURCES)
	mkdir -p $(ERL_OUT)
	erlc $(ERLC_FLAGS) -o $(ERL_OUT) $(ERL_SOURCES)
	src/etfify base/base.app.src $(ERL_OUT)/base.app

# BOSS Base Image, a collection of base BEAM files
bosbaima: ebin
	date > build/date
	cp base/emu.cfg build/emu.cfg
	tar cf build/bosbaima.tar -C build/ ebin/ date emu.cfg

esp: emu bosbaima
	mkdir -p build/esp/{EFI/BOOT,BOSS}
	cp build/bosbaima.tar build/esp/BOSS/BOSBAIMA.TAR
	cp build/emulator.efi build/esp/EFI/BOOT/BOOTX64.EFI

qemu: esp
	qemu-system-x86_64 -enable-kvm \
		-drive if=pflash,format=raw,readonly=on,file=/usr/share/ovmf/x64/OVMF.fd \
		-drive format=raw,file=fat:rw:build/esp \
		-m 128 \
		-smp 1,sockets=1,cores=1,threads=1 \
		-boot menu=off,splash-time=0 \
		-no-reboot \
		-serial stdio

qemu-mon: esp
	qemu-system-x86_64 \
		-drive if=pflash,format=raw,readonly=on,file=/usr/share/ovmf/x64/OVMF.fd \
		-drive format=raw,file=fat:rw:build/esp \
		-m 128 \
		-smp 1,sockets=1,cores=1,threads=1 \
		-boot menu=off,splash-time=0 \
		-no-shutdown -no-reboot \
		-d int \
		-D qemu.log \
		-accel tcg \
		-serial stdio

qemu-gdb: esp
	qemu-system-x86_64 \
		-drive if=pflash,format=raw,readonly=on,file=/usr/share/ovmf/x64/OVMF.fd \
		-drive format=raw,file=fat:rw:build/esp \
		-m 128 \
		-smp 1,sockets=1,cores=1,threads=1 \
		-boot menu=off,splash-time=0 \
		-serial stdio \
		-no-shutdown -no-reboot \
		-d int \
		-D qemu.log \
		-accel tcg \
		-s -S
