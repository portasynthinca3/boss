#![allow(clippy::missing_safety_doc)]

use core::{arch::asm, marker::PhantomData};

pub trait PortAccessible {
    unsafe fn read(port: u16) -> Self
        where Self: Sized;
    unsafe fn write(port: u16, value: Self)
        where Self: Sized;
}

impl PortAccessible for u8 {
    unsafe fn read(port: u16) -> u8 {
        let out: u8;
        asm!("in al, dx",
             in("dx") port,
             out("al") out);
        out
    }

    unsafe fn write(port: u16, value: u8) {
        asm!("out dx, al",
             in("dx") port,
             in("al") value);
    }
}

impl PortAccessible for u16 {
    unsafe fn read(port: u16) -> u16 {
        let out: u16;
        asm!("in ax, dx",
             in("dx") port,
             out("ax") out);
        out
    }

    unsafe fn write(port: u16, value: u16) {
        asm!("out dx, ax",
             in("dx") port,
             in("ax") value);
    }
}

impl PortAccessible for u32 {
    unsafe fn read(port: u16) -> u32 {
        let out: u32;
        asm!("in eax, dx",
             in("dx") port,
             out("eax") out);
        out
    }
    unsafe fn write(port: u16, value: u32) {
        asm!("out dx, eax",
             in("dx") port,
             in("eax") value);
    }
}

pub struct Port<T: PortAccessible> {
    number: u16,
    phantom: PhantomData<T>,
}

impl<T: PortAccessible> Port<T> {
    /// Instantiates a [Port]
    /// 
    /// # Safety
    /// Care must be taken not to instantiate multiple instances of [Port] that
    /// use the same I/O port, otherwise race conditions may occur.
    pub unsafe fn new(number: u16) -> Port<T> {
        Port { number, phantom: PhantomData }
    }

    pub fn read(&self) -> T {
        // SAFETY: accessing an I/O port is memory safe, but may lead to race
        // conditions. It is the responsibility of the API user to make sure
        // that this doesn't happen. See [Port::new]
        unsafe { T::read(self.number) }
    }

    pub fn write(&self, value: T) {
        // SAFETY: see [Port::read]
        unsafe { T::write(self.number, value) }
    }
}
