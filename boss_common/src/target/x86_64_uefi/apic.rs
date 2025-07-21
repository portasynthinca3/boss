//! Implements the Advanced Programmable Interrupt Controller (APIC), both local
//! (LAPIC) and I/O (IOAPIC).
//! 
//! Based on:
//!   - Intel SDM, Volume 3A, Chapter 12
//!   - ACPI specification version 6.5, Sections 5.2.12, 5.2.13
//! 
//! Glossary:
//!   - APIC: Advanced Programmable Interrupt Controller
//!   - LAPIC: Local APIC
//!   - IOAPIC: I/O APIC
//!   - ACPI: Advanced Configuration and Power Interface
//!   - NMI: Non-Maskable Interrupt
//!   - Processor UID: ACPI processor Unique Identifier
//!   - IRQ: legacy Interrupt Request number
//!   - GSI: Global System Interrupt number
//!   - LINT: Local Interrupt line
//!   - MADT: Multiple APIC Descriptor Table
//!   - LVT: Local Vector Table
//!   - BSP: Bootstrap Processor

use core::ptr::Pointee;
use strum_macros::FromRepr;
use bitflags::bitflags;
// use spin::Once;

use crate::util::{cursor::Cursor, boot_stage};
use super::{memmgr::{PhysAddr, VirtAddr, virt::AddressSpace}, acpi::{Table, TableHeader}, hal::io_port::Port, interrupt};

/// Processor UID, as encoded using AML
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct ProcessorUid(pub u8);

/// Local APIC ID
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct LocalApicId(pub u8);

/// I/O APIC ID
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct IoApicId(pub u8);

/// Processor Interrupt Request number
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Irq(pub u8);

/// Global System Interrupt number
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Gsi(pub u32);

/// Local APIC Interrupt number
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Lint(pub u8);

/// Multiple APIC Descriptor Table: describes the interrupt configuration
#[repr(C, packed)]
pub struct Madt {
    header: TableHeader,
    apic_address: u32,
    apic_flags: u32,
    encoded_descriptors: [u8],
}

impl Table for Madt {
    const SIGNATURE: &str = "APIC";
    fn get_header(&self) -> &TableHeader { &self.header }
    fn get_dst_metadata(contents_length: usize) -> <Self as Pointee>::Metadata {
        contents_length - (size_of::<u32>() * 2)
    }
}

struct MadtIter<'tab> {
    cursor: Cursor<'tab>,
}

impl Madt {
    fn iter(&self) -> MadtIter {
        MadtIter { cursor: Cursor::new(&self.encoded_descriptors) }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, FromRepr)]
enum MadtDescriptorType {
    LocalApic = 0,
    IoApic = 1,
    InterruptSourceOverride = 2,
    NmiSource = 3,
    LocalApicNmi = 4,
    LocalApicAddrOverride = 5,
    IoSapic = 6,
    LocalSapic = 7,
    PlatformInterruptSources = 8,
    ProcessorLocalX2Apic = 9,
    LocalX2ApicNmi = 10,
    GicCpuInterface = 11,
    GicDistributor = 12,
    GicMsiFrame = 13,
    GicRedistributor = 14,
    GicInterruptTranslationService = 15,
    MultiprocessorWakeup = 16,
    CorePic = 17,
    LegacyIoPic = 18,
    HyperTransportPic = 19,
    ExtendIoPic = 20,
    MsiPic = 21,
    BridgeIoPic = 22,
    LpcPic = 23,
}

bitflags! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    struct LocalApicFlags: u32 {
        const ENABLED = 1 << 0;
        const ONLINE_CAPABLE = 1 << 1;
    }
}

bitflags! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    struct InterruptFlags: u16 {
        const POLARITY_FIXED = 1 << 0; // inverse: polarity according to bus default
        const POLARITY_ACTIVE_LOW = 1 << 1; // inverse: polarity is active high
        const TRIGGER_FIXED = 1 << 2; // inverse: trigger according to bus default
        const TRIGGER_LEVEL = 1 << 3; // inverse: edge-triggered
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum MadtDescriptor {
    Unknown(u8),
    KnownUnsupported(MadtDescriptorType),
    LocalApic { processor: ProcessorUid, id: LocalApicId, flags: LocalApicFlags },
    IoApic { id: IoApicId, address: PhysAddr, gsi_base: Gsi },
    InterruptSourceOverride { irq: Irq, gsi: Gsi, flags: InterruptFlags },
    NmiSource { flags: InterruptFlags, gsi: Gsi },
    LocalApicNmi { processor: ProcessorUid, flags: InterruptFlags, lint: Lint },
}

impl<'tab> Iterator for MadtIter<'tab> {
    type Item = MadtDescriptor;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor.reached_end() { return None };
        let record_type = self.cursor.read_u8();
        let record_length = self.cursor.read_u8();
        let payload_length = record_length - 2;
        let mut record = Cursor::new(self.cursor.read_slice(payload_length as usize));

        use MadtDescriptorType::*;
        let desc = match MadtDescriptorType::from_repr(record_type as usize) {
            Some(LocalApic) => {
                let processor = ProcessorUid(record.read_u8());
                let id = LocalApicId(record.read_u8());
                let flags = LocalApicFlags::from_bits_truncate(record.read_u32_le());
                MadtDescriptor::LocalApic { processor, id, flags }
            },
            Some(IoApic) => {
                let id = IoApicId(record.read_u8());
                record.skip(1);
                let address = PhysAddr(record.read_u32_le() as usize);
                let gsi_base = Gsi(record.read_u32_le());
                MadtDescriptor::IoApic { id, address, gsi_base }
            },
            Some(InterruptSourceOverride) => {
                record.skip(1);
                let irq = Irq(record.read_u8());
                let gsi = Gsi(record.read_u32_le());
                let flags = InterruptFlags::from_bits_truncate(record.read_u16_le());
                MadtDescriptor::InterruptSourceOverride { irq, gsi, flags }
            }
            Some(NmiSource) => {
                let flags = InterruptFlags::from_bits_truncate(record.read_u16_le());
                let gsi = Gsi(record.read_u32_le());
                MadtDescriptor::NmiSource { flags, gsi }
            }
            Some(LocalApicNmi) => {
                let processor = ProcessorUid(record.read_u8());
                let flags = InterruptFlags::from_bits_truncate(record.read_u16_le());
                let lint = Lint(record.read_u8());
                MadtDescriptor::LocalApicNmi { processor, flags, lint }
            }

            Some(x) => MadtDescriptor::KnownUnsupported(x),
            None => MadtDescriptor::Unknown(record_type),
        };
        Some(desc)
    }
}

/// Local APIC registers
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[allow(dead_code)]
enum LapicRegister {
    Id = 0x020,
    Version = 0x030,
    TaskPriority = 0x080,
    ArbitrationPriority = 0x090,
    ProcessorPriority = 0x0a0,
    Eoi = 0x0b0,
    RemoteRead = 0x0c0,
    LogicalDest = 0x0d0,
    DestFormat = 0x0e0,
    SpuriousIntr = 0x0f0,
    InService0 = 0x100,
    InService1 = 0x110,
    InService2 = 0x120,
    InService3 = 0x130,
    InService4 = 0x140,
    InService5 = 0x150,
    InService6 = 0x160,
    InService7 = 0x170,
    TrigMode0 = 0x180,
    TrigMode1 = 0x190,
    TrigMode2 = 0x1a0,
    TrigMode3 = 0x1b0,
    TrigMode4 = 0x1c0,
    TrigMode5 = 0x1d0,
    TrigMode6 = 0x1e0,
    TrigMode7 = 0x1f0,
    Request0 = 0x200,
    Request1 = 0x210,
    Request2 = 0x220,
    Request3 = 0x230,
    Request4 = 0x240,
    Request5 = 0x250,
    Request6 = 0x260,
    Request7 = 0x270,
    ErrorStatus = 0x280,
    LvtCorrectedMachineCheck = 0x2f0,
    IntrCommand0 = 0x300,
    IntrCommand1 = 0x310,
    LvtTimer = 0x320,
    LvtThermal = 0x330,
    LvtPerformance = 0x340,
    LvtLint0 = 0x350,
    LvtLint1 = 0x360,
    LvtError = 0x370,
    TimerInitCount = 0x380,
    TimerCurCount = 0x390,
    TimerDivider = 0x3e0,
}

/// Local APIC
struct Lapic {
    base: *mut u32,
}

impl Lapic {
    unsafe fn new(addr: PhysAddr) -> Lapic {
        let addr: VirtAddr = addr.into();
        Lapic { base: addr.into() }
    }

    fn read(&mut self, reg: LapicRegister) -> u32 {
        unsafe { self.base.byte_add(reg as usize).read_volatile() }
    }

    fn write(&mut self, reg: LapicRegister, value: u32) {
        unsafe { self.base.byte_add(reg as usize).write_volatile(value) }
    }
}

// /// I/O APIC registers
// #[derive(Clone, Copy, PartialEq, Eq, Debug)]
// enum IoApicRegister {

// }

const NMI_VECTOR: u8 = 2;
const SPURIOUS_INTR_VECTOR: u8 = interrupt::MAX_INTERRUPT_CNT - 1;
pub const LAPIC_TIMER_VECTOR: interrupt::Vector = interrupt::Vector::External(interrupt::MAX_INTERRUPT_CNT - 2);
pub const LAPIC_MCI_VECTOR: interrupt::Vector = interrupt::Vector::External(interrupt::MAX_INTERRUPT_CNT - 3);
pub const LAPIC_ERROR_VECTOR: interrupt::Vector = interrupt::Vector::External(interrupt::MAX_INTERRUPT_CNT - 4);
pub const LAPIC_PERF_MON_VECTOR: interrupt::Vector = interrupt::Vector::External(interrupt::MAX_INTERRUPT_CNT - 5);
pub const LAPIC_THERMAL_VECTOR: interrupt::Vector = interrupt::Vector::External(interrupt::MAX_INTERRUPT_CNT - 6);
pub const ALL_PROCESSORS: ProcessorUid = ProcessorUid(255);

// static LAPIC: Once<Lapic> = Once::new();
// static PROCESSOR_LAPIC_MAP: Once<Vec<(ProcessorUid, LocalApicId)>> = Once::new();
// static IOAPIC: Once<Vec<Ioapic>> = Once::new();

/// Performs initialization of the local APIC and all I/O APICs
pub fn init(madt: &Madt, interrupt_mgr: &mut interrupt::Manager, addr_space: &mut AddressSpace) {
    boot_stage::new_level();

    boot_stage::same_level("LAPIC");
    let lapic_addr = PhysAddr(madt.apic_address as usize);
    let lapic_virt_addr = lapic_addr.into();
    addr_space.modify().map_range(lapic_virt_addr, lapic_addr, 1, Default::default(), false).unwrap();

    let mut lapic = unsafe { Lapic::new(lapic_addr) };
    // LAPIC.call_once(|| lapic);

    boot_stage::same_level("Disabling PIC");
    // SAFETY: It is assumed that the containing codebase does not use the PIC
    let pic_master = unsafe { Port::new(0x21) };
    let pic_slave = unsafe { Port::new(0xa1) };
    pic_master.write(0xffu8);
    pic_slave.write(0xffu8);

    // create processor uid to lapic id map
    // let processor_lapic_map = PROCESSOR_LAPIC_MAP.call_once(|| {
    //     let mut map = Vec::new();
    //     for entry in madt.iter() {
    //         if let MadtDescriptor::LocalApic { processor, id, flags: _flags } = entry {
    //             map.push((processor, id));
    //         }
    //     }
    //     map
    // });

    // we're the BSP
    let bsp_lapic_id = LocalApicId(lapic.read(LapicRegister::Id) as u8);
    let bsp_processor_uid = madt
        .iter()
        .find_map(|entry| {
            if let MadtDescriptor::LocalApic { processor, id, flags: _ } = entry && id == bsp_lapic_id {
                Some(processor)
            } else {
                None
            }
        })
        .unwrap();

    log::debug!("bootstrap {bsp_lapic_id:?}, {bsp_processor_uid:?}");

    boot_stage::same_level("Mapping basic interrupts");
    for entry in madt.iter() {
        log::trace!("{entry:?}");
        match entry {
            MadtDescriptor::LocalApicNmi { processor, flags: _, lint: Lint(lint) } if processor == bsp_processor_uid || processor == ALL_PROCESSORS => {
                let register = if lint == 0 { LapicRegister::LvtLint0 } else { LapicRegister::LvtLint1 };
                interrupt_mgr.set_handler(interrupt::Vector::External(NMI_VECTOR), Some(|_state| {
                    panic!("hardware failure (NMI)");
                })).unwrap();
                let value = (0b100u32 << 8) | (NMI_VECTOR as u32); // NMI delivery mode. Vol 3A Fig 12-8
                lapic.write(register, value);
            }
            _ => (),
        };
    }

    boot_stage::same_level("Enabling LAPIC");
    interrupt_mgr.set_handler(interrupt::Vector::External(SPURIOUS_INTR_VECTOR), Some(|_state| {
        log::warn!("spurious interrupt");
        false
    })).unwrap();
    lapic.write(LapicRegister::SpuriousIntr, 0x100 | (SPURIOUS_INTR_VECTOR as u32)); // bit 8: enable

    // TODO: IOAPIC
    boot_stage::end_level();
}
