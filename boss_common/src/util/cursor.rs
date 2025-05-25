//! Convenience functions for parsing data in immutable u8 slices

use core::str::{from_utf8, Utf8Error};

/// Convenience functions for reading a buffer as a stream
#[derive(Clone)]
pub struct Cursor<'d> {
    data: &'d [u8],
    pub position: usize,
}

// Convenience functions for parsing data in immutable u8 slices
impl<'d> Cursor<'d> {
    pub fn new(data: &'d [u8]) -> Cursor<'d> {
        Cursor { data, position: 0 }
    }

    pub fn read_u8(&mut self) -> u8 {
        let data = self.data[self.position];
        self.position += 1;
        data
    }

    pub fn read_u16_be(&mut self) -> u16 {
        let slice: [u8; 2] = self.data[self.position .. self.position + 2].try_into().unwrap();
        self.position += 2;
        u16::from_be_bytes(slice)
    }

    pub fn read_u32_be(&mut self) -> u32 {
        let slice: [u8; 4] = self.data[self.position .. self.position + 4].try_into().unwrap();
        self.position += 4;
        u32::from_be_bytes(slice)
    }

    pub fn read_u64_be(&mut self) -> u64 {
        let slice: [u8; 8] = self.data[self.position .. self.position + 8].try_into().unwrap();
        self.position += 8;
        u64::from_be_bytes(slice)
    }

    pub fn read_u16_le(&mut self) -> u16 {
        let slice: [u8; 2] = self.data[self.position .. self.position + 2].try_into().unwrap();
        self.position += 2;
        u16::from_le_bytes(slice)
    }

    pub fn read_u32_le(&mut self) -> u32 {
        let slice: [u8; 4] = self.data[self.position .. self.position + 4].try_into().unwrap();
        self.position += 4;
        u32::from_le_bytes(slice)
    }

    pub fn read_u64_le(&mut self) -> u64 {
        let slice: [u8; 8] = self.data[self.position .. self.position + 8].try_into().unwrap();
        self.position += 8;
        u64::from_le_bytes(slice)
    }

    pub fn read_i64_le(&mut self) -> i64 {
        let slice: [u8; 8] = self.data[self.position .. self.position + 8].try_into().unwrap();
        self.position += 8;
        i64::from_le_bytes(slice)
    }

    pub fn read_slice(&mut self, size: usize) -> &'d [u8] {
        let slice = &self.data[self.position .. self.position + size];
        self.position += size;
        slice
    }

    pub fn read_utf8(&mut self, size: usize) -> Result<&'d str, Utf8Error> {
        let slice = &self.data[self.position .. self.position + size];
        self.position += size;
        from_utf8(slice)
    }

    pub fn skip(&mut self, count: usize) {
        self.position += count;
    }

    pub fn reached_end(&self) -> bool {
        self.position >= self.data.len()
    }

    pub fn remaining(&self) -> usize {
        self.data.len() - self.position
    }
}
