# AP startup code in SMP mode.
# The BSP has copied this code into low memory and is calling us via the APIC.
#
#   AP = Application Processor
#   BSP = Bootstrap Processor
#   SMP = Symmetric Multiprocessing

# implied intel syntax

.global smp_ap_boot_start
.global smp_ap_status_block
.global smp_ap_boot_end

smp_ap_boot_start:
.code16
jmp start

smp_ap_status_block:
    up_count: .byte 0

start:
    push cs
    pop ds
    lock inc byte ptr ds:[2]

endless:
    cli
    hlt
    jmp endless

smp_ap_boot_end:
.code64
