SHELL=bash

default: esp

clean:
	rm -rf target/x86_64-unknown-uefi
	rm -rf build

# Emulator (the EFI executable)
emu:
	cargo build --target x86_64-unknown-uefi --config build.incremental=false

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
	cp target/x86_64-unknown-uefi/debug/boss.efi build/esp/EFI/BOOT/BOOTX64.EFI

qemu: esp
	qemu-system-x86_64 -enable-kvm \
		-drive if=pflash,format=raw,readonly=on,file=/usr/share/ovmf/x64/OVMF.fd \
		-drive format=raw,file=fat:rw:build/esp \
		-m 128 \
		-smp 4,sockets=1,cores=4,threads=1 \
		-boot menu=on,splash-time=0 \
		-serial mon:stdio
