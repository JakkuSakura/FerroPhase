//! Minimal Mach-O writer.
//!
//! This module intentionally implements only the tiny subset of Mach-O needed
//! to bootstrap `fp-native` without relying on external linkers.

use fp_core::error::{Error, Result};

pub(crate) fn align_up(value: u64, align: u64) -> u64 {
    debug_assert!(align.is_power_of_two());
    (value + (align - 1)) & !(align - 1)
}

pub(crate) fn put_u8(out: &mut Vec<u8>, x: u8) {
    out.push(x);
}
pub(crate) fn put_u16(out: &mut Vec<u8>, x: u16) {
    out.extend_from_slice(&x.to_le_bytes());
}
pub(crate) fn put_u32(out: &mut Vec<u8>, x: u32) {
    out.extend_from_slice(&x.to_le_bytes());
}
pub(crate) fn put_u64(out: &mut Vec<u8>, x: u64) {
    out.extend_from_slice(&x.to_le_bytes());
}
pub(crate) fn put_i32(out: &mut Vec<u8>, x: i32) {
    out.extend_from_slice(&x.to_le_bytes());
}

pub(crate) fn put_bytes_fixed<const N: usize>(out: &mut Vec<u8>, s: &str) {
    let mut buf = [0u8; N];
    let b = s.as_bytes();
    let n = b.len().min(N);
    buf[..n].copy_from_slice(&b[..n]);
    out.extend_from_slice(&buf);
}

pub(crate) fn ensure(condition: bool, msg: &'static str) -> Result<()> {
    if condition {
        Ok(())
    } else {
        Err(Error::from(msg))
    }
}

