//! Read-only TAR file format parser. Based on this specification:
//! https://www.gnu.org/software/tar/manual/html_node/Standard.html

use super::from_null_term;

const BLOCK_SIZE: usize = 512;

/// Read-only TAR file format parser
#[derive(Clone, Debug)]
pub struct TarFile<'d> {
    data: &'d [u8],
}

impl<'d> TarFile<'d> {
    pub fn new(data: &'d [u8]) -> TarFile<'d> {
        TarFile { data }
    }

    /// Returns a slice containing the contents of the file. If no file with the
    /// specified name is found, `None` is returned.
    pub fn read_file(&self, name: &str) -> Option<&'d [u8]> {
        let mut position = 0;

        loop {
            // read block
            let block: [u8; BLOCK_SIZE] = self.data[position .. position+BLOCK_SIZE].try_into().unwrap();
            let magic = from_null_term(&block[257..263]).unwrap();
            if !magic.starts_with("ustar") {
                // reached end of archive
                return None;
            }

            let block_name = from_null_term(&block[0..100]).unwrap();
            let size = usize::from_str_radix(from_null_term(&block[124..136]).unwrap(), 8).unwrap();

            if block_name == name {
                let file = &self.data[position+BLOCK_SIZE .. position+BLOCK_SIZE+size];
                return Some(file);
            }

            let skip_blocks = 1 + size.div_ceil(BLOCK_SIZE);
            position += skip_blocks * BLOCK_SIZE;
        }
    }
}
