use fp_core::ast::Value;
use fp_core::lir::{LirInteger, LirType};

pub(super) fn integer_constant_value(integer: &LirInteger) -> u64 {
    match integer {
        LirInteger::I1(value) => u64::from(*value),
        LirInteger::I8(value) => u64::from(*value),
        LirInteger::I16(value) => u64::from(*value),
        LirInteger::I32(value) => u64::from(*value),
        LirInteger::I64(value) => *value,
        LirInteger::I128(value) => *value as u64,
        LirInteger::Arbitrary(_) => {
            todo!("interpreter conversion for arbitrary integer constants")
        }
    }
}

pub(super) fn is_integer_type(ty: &LirType) -> bool {
    matches!(
        ty,
        LirType::Integer(_)
            | LirType::I1
            | LirType::I8
            | LirType::I16
            | LirType::I32
            | LirType::I64
            | LirType::I128
    )
}

pub(super) fn integer_value(value: u64, signed: bool) -> Value {
    if signed {
        Value::int(value as i64)
    } else {
        Value::uint(value)
    }
}

pub(super) fn decode_integer(raw: u64, signed: bool, bits: u32) -> Value {
    match (signed, bits) {
        (_, 1) => Value::bool(raw != 0),
        (true, 8) => Value::int(raw as i8 as i64),
        (false, 8) => Value::uint(raw as u8 as u64),
        (true, 16) => Value::int(raw as i16 as i64),
        (false, 16) => Value::uint(raw as u16 as u64),
        (true, 32) => Value::int(raw as i32 as i64),
        (false, 32) => Value::uint(raw as u32 as u64),
        (true, _) => Value::int(raw as i64),
        (false, _) => Value::uint(raw),
    }
}

pub(super) fn mask_integer(value: u64, bits: u32) -> u64 {
    match bits {
        0 => 0,
        64.. => value,
        bits => value & ((1u64 << bits) - 1),
    }
}

pub(super) fn sign_extend_integer(value: u64, source_bits: u32, destination_bits: u32) -> u64 {
    let value = mask_integer(value, source_bits);
    if source_bits == 0 || source_bits >= destination_bits || source_bits >= 64 {
        return value;
    }
    let sign_bit = 1u64 << (source_bits - 1);
    if value & sign_bit == 0 {
        value
    } else {
        value | (!0u64 << source_bits)
    }
}
