use super::{LirDataLayout, LirDataLayoutError, LirType};

#[derive(Debug, Clone)]
pub struct StructLayout {
    pub size: u64,
    pub align: u32,
    pub field_offsets: Vec<u64>,
}

impl LirDataLayout {
    pub fn size_of(&self, ty: &LirType) -> Result<u64, LirDataLayoutError> {
        let size = match ty {
            LirType::Integer(width) => u64::from(width.div_ceil(8)),
            LirType::I1 | LirType::I8 => 1,
            LirType::I16 => 2,
            LirType::I32 | LirType::F32 => 4,
            LirType::I64 | LirType::F64 => 8,
            LirType::I128 => 16,
            LirType::Ptr(_) | LirType::Function { .. } => {
                u64::from(self.pointer_size_bits.div_ceil(8))
            }
            LirType::Array(elem, len) => self
                .size_of(elem)?
                .checked_mul(*len)
                .ok_or(LirDataLayoutError::SizeOverflow)?,
            LirType::Struct { .. } => {
                self.struct_layout(ty)?
                    .ok_or_else(|| LirDataLayoutError::ExpectedStruct(ty.clone()))?
                    .size
            }
            LirType::Vector(elem, count) => self
                .size_of(elem)?
                .checked_mul(u64::from(*count))
                .ok_or(LirDataLayoutError::SizeOverflow)?,
            LirType::Void | LirType::Label | LirType::Token | LirType::Metadata => 0,
            LirType::Error => return Err(LirDataLayoutError::ErrorTypeHasNoLayout),
        };
        Ok(size)
    }

    pub fn align_of(&self, ty: &LirType) -> Result<u32, LirDataLayoutError> {
        match ty {
            LirType::Integer(width) => self.integer_alignment(*width),
            LirType::I1 => self.integer_alignment(1),
            LirType::I8 => self.integer_alignment(8),
            LirType::I16 => self.integer_alignment(16),
            LirType::I32 | LirType::F32 => self.integer_alignment(32),
            LirType::I64 | LirType::F64 => self.integer_alignment(64),
            LirType::I128 => self.integer_alignment(128),
            LirType::Ptr(_) | LirType::Function { .. } => Ok(self.pointer_alignment),
            LirType::Array(elem, _) | LirType::Vector(elem, _) => self.align_of(elem),
            LirType::Struct { .. } => Ok(self
                .struct_layout(ty)?
                .ok_or_else(|| LirDataLayoutError::ExpectedStruct(ty.clone()))?
                .align),
            LirType::Void | LirType::Label | LirType::Token | LirType::Metadata => Ok(1),
            LirType::Error => Err(LirDataLayoutError::ErrorTypeHasNoLayout),
        }
    }

    pub fn struct_layout(&self, ty: &LirType) -> Result<Option<StructLayout>, LirDataLayoutError> {
        let LirType::Struct { fields, packed, .. } = ty else {
            return Ok(None);
        };
        if fields.is_empty() {
            return Ok(Some(StructLayout {
                size: 0,
                align: 1,
                field_offsets: Vec::new(),
            }));
        }

        let mut offsets = Vec::with_capacity(fields.len());
        let mut offset = 0u64;
        let mut max_align = 1u32;

        for field in fields {
            let field_align = if *packed { 1 } else { self.align_of(field)? };
            max_align = max_align.max(field_align);
            if !*packed && field_align > 1 {
                offset = align_to(offset, u64::from(field_align));
            }
            offsets.push(offset);
            offset = offset
                .checked_add(self.size_of(field)?)
                .ok_or(LirDataLayoutError::SizeOverflow)?;
        }

        let align = if *packed { 1 } else { max_align.max(1) };
        let size = if *packed {
            offset
        } else {
            align_to(offset, u64::from(align))
        };

        let layout = StructLayout {
            size,
            align,
            field_offsets: offsets,
        };
        #[cfg(debug_assertions)]
        self.debug_validate_struct_layout(ty, &layout)?;
        Ok(Some(layout))
    }
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
impl LirDataLayout {
    fn debug_validate_struct_layout(
        &self,
        ty: &LirType,
        layout: &StructLayout,
    ) -> Result<(), LirDataLayoutError> {
        let LirType::Struct { fields, packed, .. } = ty else {
            return Ok(());
        };
        debug_assert_eq!(
            fields.len(),
            layout.field_offsets.len(),
            "layout field count mismatch"
        );
        if fields.is_empty() {
            debug_assert_eq!(layout.size, 0, "empty struct size should be 0");
            debug_assert_eq!(layout.align, 1, "empty struct align should be 1");
            return Ok(());
        }

        for (idx, field) in fields.iter().enumerate() {
            let offset = *layout.field_offsets.get(idx).expect("field offset missing");
            let field_align = if *packed {
                1
            } else {
                self.align_of(field)?.max(1)
            };
            if !*packed {
                debug_assert_eq!(
                    offset % u64::from(field_align),
                    0,
                    "field offset not aligned"
                );
            }
            let field_size = self.size_of(field)?;
            debug_assert!(
                offset.saturating_add(field_size) <= layout.size,
                "field range exceeds struct size"
            );
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{LirDataLayout, LirType};

    fn layout() -> LirDataLayout {
        LirDataLayout::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .expect("valid data layout")
    }

    #[test]
    fn struct_layout_respects_padding() {
        let ty = LirType::Struct {
            fields: vec![LirType::I8, LirType::I32],
            packed: false,
            name: None,
        };
        let layout = layout()
            .struct_layout(&ty)
            .expect("layout expected")
            .expect("struct layout");
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
        let layout = layout()
            .struct_layout(&ty)
            .expect("layout expected")
            .expect("struct layout");
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
        let layout = layout()
            .struct_layout(&ty)
            .expect("layout expected")
            .expect("struct layout");
        assert_eq!(layout.field_offsets, Vec::<u64>::new());
        assert_eq!(layout.align, 1);
        assert_eq!(layout.size, 0);
    }

    #[test]
    fn arbitrary_integer_requires_declared_alignment() {
        let layout = layout();
        assert!(layout.align_of(&LirType::Integer(24)).is_err());
        assert_eq!(
            layout.size_of(&LirType::Integer(24)).expect("integer size"),
            3
        );
    }
}
