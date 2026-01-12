use super::LirType;

#[derive(Debug, Clone)]
pub struct StructLayout {
    pub size: u64,
    pub align: u32,
    pub field_offsets: Vec<u64>,
}

pub fn size_of(ty: &LirType) -> u64 {
    match ty {
        LirType::I1 | LirType::I8 => 1,
        LirType::I16 => 2,
        LirType::I32 | LirType::F32 => 4,
        LirType::I64 | LirType::F64 => 8,
        LirType::I128 => 16,
        LirType::Ptr(_) | LirType::Function { .. } => 8,
        LirType::Array(elem, len) => size_of(elem) * (*len as u64),
        LirType::Struct { .. } => struct_layout(ty).map(|layout| layout.size).unwrap_or(0),
        LirType::Vector(elem, count) => size_of(elem) * (*count as u64),
        LirType::Void | LirType::Label | LirType::Token | LirType::Metadata => 0,
        LirType::Error => 8,
    }
}

pub fn align_of(ty: &LirType) -> u32 {
    match ty {
        LirType::I1 | LirType::I8 => 1,
        LirType::I16 => 2,
        LirType::I32 | LirType::F32 => 4,
        LirType::I64 | LirType::F64 => 8,
        LirType::I128 => 16,
        LirType::Ptr(_) | LirType::Function { .. } => 8,
        LirType::Array(elem, _) => align_of(elem),
        LirType::Struct { .. } => struct_layout(ty).map(|layout| layout.align).unwrap_or(1),
        LirType::Vector(elem, _) => align_of(elem),
        LirType::Void | LirType::Label | LirType::Token | LirType::Metadata => 1,
        LirType::Error => 8,
    }
}

pub fn struct_layout(ty: &LirType) -> Option<StructLayout> {
    let LirType::Struct { fields, packed, .. } = ty else {
        return None;
    };
    if fields.is_empty() {
        return Some(StructLayout {
            size: 0,
            align: 1,
            field_offsets: Vec::new(),
        });
    }

    let mut offsets = Vec::with_capacity(fields.len());
    let mut offset = 0u64;
    let mut max_align = 1u32;

    for field in fields {
        let field_align = if *packed { 1 } else { align_of(field) };
        max_align = max_align.max(field_align);
        if !*packed && field_align > 1 {
            offset = align_to(offset, field_align as u64);
        }
        offsets.push(offset);
        offset = offset.saturating_add(size_of(field));
    }

    let align = if *packed { 1 } else { max_align.max(1) };
    let size = if *packed {
        offset
    } else {
        align_to(offset, align as u64)
    };

    Some(StructLayout {
        size,
        align,
        field_offsets: offsets,
    })
}

fn align_to(value: u64, alignment: u64) -> u64 {
    if alignment <= 1 {
        return value;
    }
    let rem = value % alignment;
    if rem == 0 {
        value
    } else {
        value + (alignment - rem)
    }
}
