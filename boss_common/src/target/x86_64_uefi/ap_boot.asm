; Assembler: NASM
; AP startup code in SMP mode.
; The BSP has copied this code into low memory and is calling us via the APIC.
;
;   AP = Application Processor
;   BSP = Bootstrap Processor
;   SMP = Symmetric Multiprocessing

[bits 16]
[org 0x0000:0x0000] ; the segment will be from 0x0000 to 0x00ff, actual value unknown

entry:
    jmp start

status_block:
    real_mode_up: db 0
    long_mode_up: db 0
    setup_sem: db 0
    setup_command: db 0

    addr_space: dd 0
    stack_top: dq 0
    main: dq 0

start:
    cli
    mov ax, cs
    mov ds, ax
    lock inc byte [real_mode_up]

    setup_wait:
        mov al, 0
        mov bl, 1
        lock cmpxchg byte [setup_sem], bl
        jnz setup_wait

    ; Only one AP is executing this critical section:
    ; CRITICAL START

        setup_command_wait:
            mov al, 1
            mov bl, 0
            lock cmpxchg byte [setup_command], bl
            jnz setup_command_wait

        setup:
            xor esi, esi
            mov si, cs
            shl esi, 4
            
            add dword [gdtr_offset], esi
            lgdt [gdtr_start]
            sub dword [gdtr_offset], esi

            mov eax, cr0
            or eax, 1
            mov cr0, eax

            add dword [jump_32_insn + 2], esi
            jump_32_insn: jmp dword 0x08:setup_32

        setup_32:
            [bits 32]
            mov bx, 0x10
            mov ds, bx

            sub dword [esi + jump_32_insn + 2], esi

            mov eax, cr4
            or eax, 0b10_0000 ; CR4.PAE
            mov cr4, eax

            mov ecx, 0xc000_0080 ; IA32_EFER
            rdmsr
            or eax, 0b1001__0000_0000 ; IA32_EFER.NXE, IA32_EFER.LMA
            wrmsr

            mov eax, dword [esi + addr_space]
            mov cr3, eax

            mov eax, cr0
            or eax, 0b1000_0000__0000_0001__0000_0000__0000_0000 ; CR0.PG, CR0.WP
            mov cr0, eax

            add dword [esi + jump_64_insn + 1], esi
            jump_64_insn: jmp dword 0x18:setup_64

        setup_64:
            [bits 64]
            xor ax, ax
            mov ds, ax
            mov ss, ax
            mov es, ax
            mov fs, ax
            mov gs, ax

            sub dword [esi + jump_64_insn + 1], esi

            mov rsp, qword [esi + stack_top]
            mov rbp, rsp
            mov rax, qword [esi + main]
            lock inc byte [esi + long_mode_up]

    ; CRITICAL END

    lock dec byte [esi + setup_sem]

    jmp rax

data:
    align 16
    gdt_start:
        ; segment 0x00
        dd 0
        dd 0
        ; segment 0x08: Code 0-4GiB
        dd 0x0000_FFFF
        dd 0b00000000__1100_1111__1001_1000__00000000
        ; segment 0x10: Data R/W 0-4GiB
        dd 0x0000_FFFF
        dd 0b00000000__1100_1111__1001_0010__00000000
        ; segment 0x18: Code 64-bit
        dd 0x0000_0000
        dd 0b00000000__1010_0000__1001_1000__00000000
    gdt_end:

    align 4
    gdtr_start:
        dw gdt_end - gdt_start
        gdtr_offset: dd gdt_start
