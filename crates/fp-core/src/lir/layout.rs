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

    let layout = StructLayout {
        size,
        align,
        field_offsets: offsets,
    };
    #[cfg(debug_assertions)]
    debug_validate_struct_layout(ty, &layout);
    Some(layout)
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

#[cfg(debug_assertions)]
fn debug_validate_struct_layout(ty: &LirType, layout: &StructLayout) {
    let LirType::Struct { fields, packed, .. } = ty else {
        return;
    };
    debug_assert_eq!(
        fields.len(),
        layout.field_offsets.len(),
        "layout field count mismatch"
    );
    if fields.is_empty() {
        debug_assert_eq!(layout.size, 0, "empty struct size should be 0");
        debug_assert_eq!(layout.align, 1, "empty struct align should be 1");
        return;
    }

    for (idx, field) in fields.iter().enumerate() {
        let offset = *layout.field_offsets.get(idx).expect("field offset missing");
        let field_align = if *packed { 1 } else { align_of(field).max(1) };
        if !*packed {
            debug_assert_eq!(offset % (field_align as u64), 0, "field offset not aligned");
        }
        let field_size = size_of(field);
        debug_assert!(
            offset.saturating_add(field_size) <= layout.size,
            "field range exceeds struct size"
        );
    }
}

#[cfg(test)]
mod tests {
    use super::{struct_layout, LirType};

    #[test]
    fn struct_layout_respects_padding() {
        let ty = LirType::Struct {
            fields: vec![LirType::I8, LirType::I32],
            packed: false,
            name: None,
        };
        let layout = struct_layout(&ty).expect("layout expected");
        assert_eq!(layout.field_offsets, vec![0, 4]);
        assert_eq!(layout.align, 4);
        assert_eq!(layout.size, 8);
    }

    #[test]
    fn packed_struct_layout_is_tight() {
        let ty = LirType::Struct {
            fields: vec![LirType::I8, LirType::I32],
            packed: true,
            name: None,
        };
        let layout = struct_layout(&ty).expect("layout expected");
        assert_eq!(layout.field_offsets, vec![0, 1]);
        assert_eq!(layout.align, 1);
        assert_eq!(layout.size, 5);
    }

    #[test]
    fn empty_struct_layout_is_zero() {
        let ty = LirType::Struct {
            fields: Vec::new(),
            packed: false,
            name: None,
        };
        let layout = struct_layout(&ty).expect("layout expected");
        assert_eq!(layout.field_offsets, Vec::<u64>::new());
        assert_eq!(layout.align, 1);
        assert_eq!(layout.size, 0);
    }
}
