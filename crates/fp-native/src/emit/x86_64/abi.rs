use super::*;

pub(super) fn is_float_type(ty: &AsmType) -> bool {
    matches!(ty, AsmType::F32 | AsmType::F64)
}

pub(super) fn is_aggregate_type(ty: &AsmType) -> bool {
    matches!(ty, AsmType::Struct { .. } | AsmType::Array(_, _))
}

pub(super) fn is_aggregate_storage(ty: &AsmType, _data_layout: &LirDataLayout) -> bool {
    is_aggregate_type(ty)
}

/// True if `ty` is a `{ptr, i64}` fat pointer (the `str`/slice
/// representation), possibly wrapped in single-field newtype structs (e.g.
/// `String`). A single-field wrapper has identical layout to its inner
/// field (same size, field at offset 0), so callers can use the same
/// offsets regardless of how many wrapper layers are present.
pub(super) fn is_fat_ptr_layout(ty: &AsmType) -> bool {
    match ty {
        AsmType::Struct { fields, .. } if fields.len() == 2 => {
            matches!(fields[0], AsmType::Ptr(_))
        }
        AsmType::Struct { fields, .. } if fields.len() == 1 => is_fat_ptr_layout(&fields[0]),
        _ => false,
    }
}

#[allow(dead_code)]
pub(super) fn is_vector_type(ty: &AsmType) -> bool {
    matches!(ty, AsmType::Vector(_, _))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum AbiPassMode {
    Ignore,
    Direct,
    Pair,
    Indirect,
}

pub(super) fn abi_pass_mode(ty: &AsmType, data_layout: &LirDataLayout) -> Result<AbiPassMode> {
    if matches!(ty, AsmType::Void) {
        return Ok(AbiPassMode::Ignore);
    }
    if !is_aggregate_type(ty) {
        return Ok(if matches!(ty, AsmType::I128) {
            AbiPassMode::Pair
        } else {
            AbiPassMode::Direct
        });
    }
    let size = data_layout
        .size_of(ty)
        .map_err(|error| Error::from(error.to_string()))?;
    if size == 0 {
        return Ok(AbiPassMode::Ignore);
    }
    if let AsmType::Struct { fields, .. } = ty {
        let pair = fields.len() == 2
            && fields.iter().all(|field| {
                matches!(
                    field,
                    AsmType::I1
                        | AsmType::I8
                        | AsmType::I16
                        | AsmType::I32
                        | AsmType::I64
                        | AsmType::Ptr(_)
                ) && data_layout.size_of(field).ok() == Some(8)
            });
        if pair && size == 16 {
            return Ok(AbiPassMode::Pair);
        }
    }
    Ok(AbiPassMode::Indirect)
}

pub(super) fn is_integer_type(ty: &AsmType) -> bool {
    matches!(
        ty,
        AsmType::I1 | AsmType::I8 | AsmType::I16 | AsmType::I32 | AsmType::I64 | AsmType::I128
    )
}

pub(super) fn int_bits(ty: &AsmType) -> Result<u32> {
    match ty {
        AsmType::I1 => Ok(1),
        AsmType::I8 => Ok(8),
        AsmType::I16 => Ok(16),
        AsmType::I32 => Ok(32),
        AsmType::I64 => Ok(64),
        AsmType::I128 => Ok(128),
        _ => Err(Error::from("expected integer type")),
    }
}

pub(super) fn constant_type(constant: &AsmConstant) -> AsmType {
    match constant {
        AsmConstant::Int(_, ty) => ty.clone(),
        AsmConstant::UInt(_, ty) => ty.clone(),
        AsmConstant::Float(_, ty) => ty.clone(),
        AsmConstant::Bool(_) => AsmType::I1,
        AsmConstant::String(_) => AsmType::Ptr(Box::new(AsmType::I8)),
        AsmConstant::Bytes(bytes) => AsmType::Array(Box::new(AsmType::I8), bytes.len() as u64),
        AsmConstant::Null(ty) => ty.clone(),
        AsmConstant::Undef(ty) => ty.clone(),
        AsmConstant::Array(_, ty) => ty.clone(),
        AsmConstant::Struct(_, ty) => ty.clone(),
        AsmConstant::GlobalRef(_, ty, _) => ty.clone(),
        AsmConstant::FunctionRef(_, ty) => ty.clone(),
    }
}

pub(super) fn value_type(
    value: &AsmValue,
    reg_types: &HashMap<u32, AsmType>,
    local_types: &HashMap<u32, AsmType>,
) -> Result<AsmType> {
    match value {
        AsmValue::Register(id) => reg_types
            .get(id)
            .cloned()
            .ok_or_else(|| Error::from("missing register type")),
        AsmValue::PhysicalRegister(register) => Ok(match register.size_bits {
            0..=8 => AsmType::I8,
            9..=16 => AsmType::I16,
            17..=32 => AsmType::I32,
            33..=64 => AsmType::I64,
            _ => AsmType::I128,
        }),
        AsmValue::Address(_) => Ok(AsmType::Ptr(Box::new(AsmType::I8))),
        AsmValue::Condition(_) => Ok(AsmType::I1),
        AsmValue::Comparison(_) => Ok(AsmType::I1),
        AsmValue::Flags(_) => Ok(AsmType::I1),
        AsmValue::Constant(constant) => Ok(constant_type(constant)),
        AsmValue::Null(ty) | AsmValue::Undef(ty) => Ok(ty.clone()),
        AsmValue::StackSlot(_) => Ok(AsmType::Ptr(Box::new(AsmType::I8))),
        AsmValue::Local(id) => local_types
            .get(id)
            .cloned()
            .ok_or_else(|| Error::from("missing local type")),
        AsmValue::Global(_, ty) => Ok(ty.clone()),
        AsmValue::Function(_) => Ok(AsmType::Ptr(Box::new(AsmType::I8))),
    }
}

pub(super) fn constant_to_i64(constant: &AsmConstant, data_layout: &LirDataLayout) -> Result<i64> {
    let size_of = |ty: &LirType| data_layout.size_of(ty).expect("layout query failed");
    match constant {
        AsmConstant::Int(value, _) => Ok(*value),
        AsmConstant::UInt(value, _) => Ok(i64::try_from(*value).unwrap_or(i64::MAX)),
        AsmConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => Ok(0),
        AsmConstant::Array(values, _) if values.is_empty() => Ok(0),
        AsmConstant::Struct(values, ty) if values.is_empty() || size_of(ty) == 0 => Ok(0),
        _ => Err(Error::from(format!(
            "unsupported constant for x86_64: {:?}",
            constant
        ))),
    }
}

pub(super) fn constant_to_u64_bits(constant: &AsmConstant) -> Result<u64> {
    match constant {
        AsmConstant::Int(value, _) => Ok(*value as u64),
        AsmConstant::UInt(value, _) => Ok(*value),
        AsmConstant::Bool(value) => Ok(if *value { 1 } else { 0 }),
        AsmConstant::Float(value, _) => Ok(value.to_bits()),
        AsmConstant::Null(_) | AsmConstant::Undef(_) => Ok(0),
        AsmConstant::GlobalRef(_, _, _) | AsmConstant::FunctionRef(_, _) => Ok(0),
        AsmConstant::Array(..) | AsmConstant::Struct(..) => Err(Error::from(
            "nested aggregate in store — call pack_small_aggregate instead",
        )),
        AsmConstant::String(_) | AsmConstant::Bytes(_) => Err(Error::from(
            "string/bytes constant in aggregate store — should have been lowered to pointer+len",
        )),
    }
}

pub(super) fn pack_small_aggregate(
    constant: &AsmConstant,
    ty: &AsmType,
    data_layout: &LirDataLayout,
) -> Result<u64> {
    let size_of = |ty: &LirType| data_layout.size_of(ty).expect("layout query failed");
    let struct_layout = |ty: &LirType| data_layout.struct_layout(ty).expect("layout query failed");
    if !matches!(abi_pass_mode(ty, data_layout)?, AbiPassMode::Direct) {
        return Err(Error::from("aggregate is not a direct word value"));
    }
    match (constant, ty) {
        (AsmConstant::Struct(values, _), AsmType::Struct { fields, .. }) => {
            let layout = struct_layout(ty)
                .ok_or_else(|| Error::from("missing struct layout for aggregate store"))?;
            let mut packed = 0u64;
            for (idx, field) in values.iter().enumerate() {
                let field_ty = fields
                    .get(idx)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                let field_size = size_of(field_ty) as u64;
                if field_size == 0 {
                    continue;
                }
                if field_size > 8 {
                    return Err(Error::from("unsupported aggregate field size"));
                }
                let mut bits = constant_to_u64_bits(field)?;
                let mask = if field_size == 8 {
                    u64::MAX
                } else {
                    (1u64 << (field_size * 8)) - 1
                };
                bits &= mask;
                let offset = *layout
                    .field_offsets
                    .get(idx)
                    .ok_or_else(|| Error::from("aggregate field out of range"))?;
                packed |= bits << (offset as u64 * 8);
            }
            Ok(packed)
        }
        (AsmConstant::Array(values, _), AsmType::Array(elem, len)) => {
            let elem_ty = elem.as_ref();
            let elem_size = size_of(elem_ty) as u64;
            if elem_size == 0 {
                return Ok(0);
            }
            if elem_size > 8 {
                return Err(Error::from("unsupported array element size"));
            }
            let mut packed = 0u64;
            for idx in 0..(*len as usize).min(values.len()) {
                let mut bits = constant_to_u64_bits(&values[idx])?;
                let mask = if elem_size == 8 {
                    u64::MAX
                } else {
                    (1u64 << (elem_size * 8)) - 1
                };
                bits &= mask;
                let offset = (idx as u64) * elem_size;
                packed |= bits << (offset * 8);
            }
            Ok(packed)
        }
        (AsmConstant::Array(values, _), other_ty) => {
            let elem_size = size_of(other_ty) as u64;
            if elem_size == 0 {
                return Ok(0);
            }
            if elem_size > 8 {
                return Err(Error::from("unsupported array element size"));
            }
            let mut packed = 0u64;
            for (idx, value) in values.iter().enumerate() {
                let mut bits = constant_to_u64_bits(value)?;
                let mask = if elem_size == 8 {
                    u64::MAX
                } else {
                    (1u64 << (elem_size * 8)) - 1
                };
                bits &= mask;
                let offset = (idx as u64) * elem_size;
                if offset >= 8 {
                    break;
                }
                packed |= bits << (offset * 8);
            }
            Ok(packed)
        }
        _ => Err(Error::from("unsupported aggregate packing")),
    }
}

pub(super) fn vreg_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .vreg_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing vreg slot"))
}

pub(super) fn stack_slot_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .slot_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing stack slot"))
}

pub(super) fn local_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout.local_offsets.get(&id).copied().ok_or_else(|| {
        let mut known = layout.local_offsets.keys().copied().collect::<Vec<_>>();
        known.sort_unstable();
        Error::from(format!(
            "missing local slot: id={} known_local_ids={:?}",
            id, known
        ))
    })
}

pub(super) fn agg_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .agg_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from(format!("missing aggregate slot for vreg {}", id)))
}

pub(super) fn alloca_offset(layout: &FrameLayout, id: u32) -> Result<i32> {
    layout
        .alloca_offsets
        .get(&id)
        .copied()
        .ok_or_else(|| Error::from("missing alloca slot"))
}
