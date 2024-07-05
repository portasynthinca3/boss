SHELL=bash

default: esp

clean:
	rm -rf target/x86_64-unknown-uefi-debug
	rm -rf build
	mkdir build

# Emulator (the EFI executable)
PROFILE=release
PROFILE_DIR=release
CARGOFLAGS=--target x86_64-unknown-uefi-debug.json -Zbuild-std=core,compiler_builtins,alloc -Zbuild-std-features=compiler-builtins-mem
MAGIC_SECTION_OFFSET=0x140800000
RELOC_SECTION_OFFSET=0x140801000
emu:
	cargo build $(CARGOFLAGS) --profile $(PROFILE)
	cargo clippy $(CARGOFLAGS) --profile $(PROFILE)
# magic! (read mem_manager/reloc.rs::relocate_pe for an explanation)
	dd if=/dev/random of=build/rand bs=1 count=2048
	cat build/rand build/rand > build/reloc-magic
	objcopy target/x86_64-unknown-uefi-debug/$(PROFILE_DIR)/boss.efi \
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

# BOSS Base Image, a collection of base BEAM files
bosbaima: ebin
	date > build/date
	tar cf build/bosbaima.tar build/ebin/ build/date

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
	qemu-system-x86_64 -enable-kvm \
		-drive if=pflash,format=raw,readonly=on,file=/usr/share/ovmf/x64/OVMF.fd \
		-drive format=raw,file=fat:rw:build/esp \
		-m 128 \
		-smp 1,sockets=1,cores=1,threads=1 \
		-boot menu=off,splash-time=0 \
		-no-reboot \
		-monitor stdio

qemu-gdb: esp
	qemu-system-x86_64 -enable-kvm \
		-drive if=pflash,format=raw,readonly=on,file=/usr/share/ovmf/x64/OVMF.fd \
		-drive format=raw,file=fat:rw:build/esp \
		-m 128 \
		-smp 1,sockets=1,cores=1,threads=1 \
		-boot menu=off,splash-time=0 \
		-no-reboot \
		-serial stdio \
		-s -S
