use fp_core::error::Result;
use fp_core::mir::ty::{Ty, TyKind};
use fp_core::{lir, mir};

use super::MirToLirLowerer;

impl MirToLirLowerer {
    /// Analyze MIR body to extract const values assigned to locals
    pub(crate) fn analyze_const_values(&mut self, mir_body: &mir::Body) -> Result<()> {
        // Iterate to propagate simple aliases like x = y where y is const-evaluated
        let mut changed = true;
        while changed {
            changed = false;
            for basic_block in &mir_body.basic_blocks {
                for stmt in &basic_block.statements {
                    if let mir::StatementKind::Assign(place, rvalue) = &stmt.kind {
                        if let Some(const_value) = self.extract_const_from_rvalue(rvalue)? {
                            if self.const_values.insert(place.local, const_value).is_none() {
                                changed = true;
                            }
                        } else if let mir::Rvalue::Use(op) = rvalue {
                            match op {
                                mir::Operand::Move(from) | mir::Operand::Copy(from) => {
                                    if let Some(cv) = self.const_values.get(&from.local).cloned() {
                                        if self.const_values.insert(place.local, cv).is_none() {
                                            changed = true;
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Extract a const value from an rvalue if it represents a constant
    pub(crate) fn extract_const_from_rvalue(
        &self,
        rvalue: &mir::Rvalue,
    ) -> Result<Option<lir::LirConstant>> {
        match rvalue {
            mir::Rvalue::Query(_) => Ok(None),
            mir::Rvalue::Use(operand) => {
                if let mir::Operand::Constant(constant) = operand {
                    match &constant.literal {
                        mir::ConstantKind::Int(value) => {
                            let value = i32::try_from(*value).map_err(|_| {
                                fp_core::error::Error::from("constant does not fit i32")
                            })?;
                            Ok(Some(
                                lir::LirConstant::integer(
                                    lir::LirType::I32,
                                    lir::LirInteger::I32(u32::from_ne_bytes(value.to_ne_bytes())),
                                )
                                .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                            ))
                        }
                        mir::ConstantKind::UInt(value) => {
                            let value = u32::try_from(*value).map_err(|_| {
                                fp_core::error::Error::from("constant does not fit i32")
                            })?;
                            Ok(Some(
                                lir::LirConstant::integer(
                                    lir::LirType::I32,
                                    lir::LirInteger::I32(value),
                                )
                                .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                            ))
                        }
                        mir::ConstantKind::Float(value) => Ok(Some(
                            lir::LirConstant::float(
                                lir::LirType::F64,
                                lir::LirFloat::F64(value.to_bits()),
                            )
                            .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                        )),
                        mir::ConstantKind::Bool(value) => Ok(Some(
                            lir::LirConstant::integer(
                                lir::LirType::I1,
                                lir::LirInteger::I1(*value),
                            )
                            .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                        )),
                        // LIR strings are data globals plus an address constant. This
                        // local-only const evaluator cannot create that global.
                        mir::ConstantKind::Str(_) => Ok(None),
                        _ => Ok(None),
                    }
                } else {
                    Ok(None)
                }
            }
            // Handle binary operations that can be const-folded (simple ints)
            mir::Rvalue::BinaryOp(bin_op, lhs, rhs) => {
                if let (mir::Operand::Constant(lhs_const), mir::Operand::Constant(rhs_const)) =
                    (lhs, rhs)
                {
                    if let (mir::ConstantKind::Int(lhs_val), mir::ConstantKind::Int(rhs_val)) =
                        (&lhs_const.literal, &rhs_const.literal)
                    {
                        let result = match bin_op {
                            mir::BinOp::Add => lhs_val + rhs_val,
                            mir::BinOp::Sub => lhs_val - rhs_val,
                            mir::BinOp::Mul => lhs_val * rhs_val,
                            mir::BinOp::Div => {
                                if *rhs_val != 0 {
                                    lhs_val / rhs_val
                                } else {
                                    return Ok(None);
                                }
                            }
                            _ => return Ok(None),
                        };
                        let result = i32::try_from(result).map_err(|_| {
                            fp_core::error::Error::from("constant result does not fit i32")
                        })?;
                        Ok(Some(
                            lir::LirConstant::integer(
                                lir::LirType::I32,
                                lir::LirInteger::I32(u32::from_ne_bytes(result.to_ne_bytes())),
                            )
                            .map_err(|error| fp_core::error::Error::from(error.to_string()))?,
                        ))
                    } else {
                        Ok(None)
                    }
                } else {
                    Ok(None)
                }
            }
            mir::Rvalue::IntrinsicCall { .. } => Ok(None),
            _ => Ok(None),
        }
    }
}

impl MirToLirLowerer {
    pub(super) fn transform_static(&mut self, mir_static: mir::Static) -> Result<lir::LirGlobal> {
        let name = lir::Name::new(mir_static.name.as_str().to_string());
        let lir_ty = self.lir_type_from_ty(&mir_static.ty);
        let raw_initializer = self.convert_static_initializer(&mir_static.init, &mir_static.ty)?;
        let (initializer, relocations) =
            self.canonicalize_global_initializer(raw_initializer, &lir_ty)?;
        let alignment = self.alignment_for_lir_type(&lir_ty).max(1);

        Ok(lir::LirGlobal {
            name,
            ty: lir_ty,
            initializer: Some(initializer),
            relocations,
            linkage: lir::Linkage::Internal,
            visibility: lir::Visibility::Hidden,
            is_constant: matches!(mir_static.mutability, mir::Mutability::Not),
            alignment: Some(alignment),
            section: None,
        })
    }

    pub(super) fn convert_static_initializer(
        &mut self,
        init: &mir::Operand,
        ty: &Ty,
    ) -> Result<lir::LirConstant> {
        match init {
            mir::Operand::Constant(constant) => self.constant_to_lir_constant(constant, ty),
            other => Err(fp_core::error::Error::from(format!(
                "unsupported static initializer operand: {:?}",
                other
            ))),
        }
    }

    pub(super) fn constant_to_lir_constant(
        &mut self,
        constant: &mir::Constant,
        ty_hint: &Ty,
    ) -> Result<lir::LirConstant> {
        let target_ty = self.lir_type_from_ty(ty_hint);
        let lir_constant = match &constant.literal {
            mir::ConstantKind::Bool(value) => {
                lir::LirConstant::integer(target_ty.clone(), lir::LirInteger::I1(*value))
                    .map_err(|error| fp_core::error::Error::from(error.to_string()))?
            }
            mir::ConstantKind::Int(value) => {
                self.integer_constant(&target_ty, *value).map_err(|error| {
                    fp_core::error::Error::from(format!(
                        "constant at {:?}: {}",
                        constant.span, error
                    ))
                })?
            }
            mir::ConstantKind::UInt(value) => {
                self.unsigned_constant(&target_ty, *value)
                    .map_err(|error| {
                        fp_core::error::Error::from(format!(
                            "constant at {:?}: {}",
                            constant.span, error
                        ))
                    })?
            }
            mir::ConstantKind::Float(value) => self.float_constant(&target_ty, *value)?,
            mir::ConstantKind::Str(value) => {
                let needs_fat_ptr = matches!(&ty_hint.kind, TyKind::Slice(_))
                    || matches!(&ty_hint.kind, TyKind::Ref(_, inner, _) if matches!(&inner.kind, TyKind::Slice(_)));
                if needs_fat_ptr {
                    let elem_lir_ty = lir::LirType::I8;
                    let slice_ty = self.slice_lir_type(&elem_lir_ty);
                    let ptr_const = self.const_string_ptr(value);
                    let len_const =
                        self.unsigned_constant(&lir::LirType::I64, value.len() as u64)?;
                    lir::LirConstant::aggregate(
                        slice_ty,
                        lir::LirConstantAggregate::Struct(vec![ptr_const, len_const]),
                    )
                } else {
                    self.const_string_ptr(value)
                }
            }
            mir::ConstantKind::Null => lir::LirConstant::null(target_ty.clone()),
            mir::ConstantKind::Undef => lir::LirConstant::undef(target_ty.clone()),
            mir::ConstantKind::Val(value) => {
                self.const_value_to_lir_constant(value, &constant.ty)?
            }
            mir::ConstantKind::FnDef(_, _) => {
                return Err(fp_core::error::Error::from(
                    "function definition references are not valid static initializer data",
                ));
            }
            mir::ConstantKind::Fn(name) => lir::LirConstant::function_address(
                target_ty.clone(),
                lir::LirFunctionRef::Name(lir::Name::new(name.as_str().to_string())),
            ),
            mir::ConstantKind::Global(path) => lir::LirConstant::global_address(
                target_ty.clone(),
                self.resolve_global_symbol(path),
            ),
            mir::ConstantKind::Ty(_) => {
                return Err(fp_core::error::Error::from(
                    "type-only constant is not a valid static initializer",
                ));
            }
            mir::ConstantKind::TokenStream { .. } => {
                return Err(fp_core::error::Error::from(
                    "token stream is not a valid LIR constant",
                ));
            }
        };

        if lir_constant.ty != target_ty {
            return Err(fp_core::error::Error::from(format!(
                "typed constant mismatch at {:?}: MIR type {:?}, literal {:?}, LIR value {:?}, target {:?}",
                constant.span, constant.ty, constant.literal, lir_constant.ty, target_ty
            )));
        }
        Ok(lir_constant)
    }

    /// A fieldless (C-like) enum's variant literal (e.g. `Value::C`) is
    /// sometimes const-folded straight to its bare discriminant scalar,
    /// while the enum's own registered layout (used everywhere else it
    /// appears, e.g. as a struct field) is the canonical
    /// `Struct{fields:[tag_ty]}` shape every enum gets, even a payload-
    /// less one, for consistency with enums that do carry a payload. Both
    /// describe the same value — when a scalar integer constant is asked
    /// for against such a single-field struct type, build it against the
    /// struct's own field type and wrap it, instead of every caller
    /// needing to special-case this itself (three call sites already
    /// needed exactly this before it was centralized here).
    pub(super) fn single_field_struct_tag_ty<'a>(ty: &'a lir::LirType) -> Option<&'a lir::LirType> {
        match ty {
            lir::LirType::Struct { fields, .. } => match fields.as_slice() {
                [tag_ty] => Some(tag_ty),
                _ => None,
            },
            _ => None,
        }
    }

    pub(super) fn integer_constant(
        &self,
        ty: &lir::LirType,
        value: i64,
    ) -> Result<lir::LirConstant> {
        if let Some(tag_ty) = Self::single_field_struct_tag_ty(ty) {
            let inner = self.integer_constant(tag_ty, value)?;
            return Ok(lir::LirConstant::aggregate(
                ty.clone(),
                lir::LirConstantAggregate::Struct(vec![inner]),
            ));
        }
        let integer =
            match ty {
                lir::LirType::I1 => lir::LirInteger::I1(value != 0),
                lir::LirType::I8 => lir::LirInteger::I8(u8::try_from(value).map_err(|_| {
                    fp_core::error::Error::from("integer constant does not fit i8")
                })?),
                lir::LirType::I16 => lir::LirInteger::I16(u16::try_from(value).map_err(|_| {
                    fp_core::error::Error::from("integer constant does not fit i16")
                })?),
                lir::LirType::I32 => lir::LirInteger::I32(u32::try_from(value).map_err(|_| {
                    fp_core::error::Error::from("integer constant does not fit i32")
                })?),
                lir::LirType::I64 => lir::LirInteger::I64(value as u64),
                lir::LirType::I128 => lir::LirInteger::I128(value as i128 as u128),
                lir::LirType::Integer(width) => {
                    let bits = value as i128 as u128;
                    let words = vec![bits as u64; (*width).div_ceil(64) as usize];
                    lir::LirInteger::Arbitrary(
                        lir::LirApInt::from_words(*width, words).ok_or_else(|| {
                            fp_core::error::Error::from("invalid arbitrary integer constant")
                        })?,
                    )
                }
                _ => {
                    return Err(fp_core::error::Error::from(format!(
                        "integer constant {value} requires integer type, got {ty:?}"
                    )));
                }
            };
        lir::LirConstant::integer(ty.clone(), integer)
            .map_err(|error| fp_core::error::Error::from(error.to_string()))
    }

    pub(super) fn unsigned_constant(
        &self,
        ty: &lir::LirType,
        value: u64,
    ) -> Result<lir::LirConstant> {
        if let Some(tag_ty) = Self::single_field_struct_tag_ty(ty) {
            let inner = self.unsigned_constant(tag_ty, value)?;
            return Ok(lir::LirConstant::aggregate(
                ty.clone(),
                lir::LirConstantAggregate::Struct(vec![inner]),
            ));
        }
        let integer =
            match ty {
                lir::LirType::I1 => lir::LirInteger::I1(value != 0),
                lir::LirType::I8 => lir::LirInteger::I8(u8::try_from(value).map_err(|_| {
                    fp_core::error::Error::from("integer constant does not fit i8")
                })?),
                lir::LirType::I16 => lir::LirInteger::I16(u16::try_from(value).map_err(|_| {
                    fp_core::error::Error::from("integer constant does not fit i16")
                })?),
                lir::LirType::I32 => lir::LirInteger::I32(u32::try_from(value).map_err(|_| {
                    fp_core::error::Error::from("integer constant does not fit i32")
                })?),
                lir::LirType::I64 => lir::LirInteger::I64(value),
                lir::LirType::I128 => lir::LirInteger::I128(u128::from(value)),
                lir::LirType::Integer(width) => {
                    let words = vec![value; (*width).div_ceil(64) as usize];
                    lir::LirInteger::Arbitrary(
                        lir::LirApInt::from_words(*width, words).ok_or_else(|| {
                            fp_core::error::Error::from("invalid arbitrary integer constant")
                        })?,
                    )
                }
                _ => {
                    return Err(fp_core::error::Error::from(format!(
                        "unsigned integer constant {value} requires integer type, got {ty:?}"
                    )));
                }
            };
        lir::LirConstant::integer(ty.clone(), integer)
            .map_err(|error| fp_core::error::Error::from(error.to_string()))
    }

    pub(super) fn float_constant(&self, ty: &lir::LirType, value: f64) -> Result<lir::LirConstant> {
        let float = match ty {
            lir::LirType::F32 => lir::LirFloat::F32((value as f32).to_bits()),
            lir::LirType::F64 => lir::LirFloat::F64(value.to_bits()),
            _ => {
                return Err(fp_core::error::Error::from(
                    "floating constant requires float type",
                ));
            }
        };
        lir::LirConstant::float(ty.clone(), float)
            .map_err(|error| fp_core::error::Error::from(error.to_string()))
    }

    pub(super) fn const_value_to_lir_constant(
        &mut self,
        value: &mir::ConstValue,
        ty: &Ty,
    ) -> Result<lir::LirConstant> {
        match value {
            mir::ConstValue::Unit => Ok(lir::LirConstant::undef(self.lir_type_from_ty(ty))),
            mir::ConstValue::Bool(value) => Ok(lir::LirConstant::integer(
                self.lir_type_from_ty(ty),
                lir::LirInteger::I1(*value),
            )
            .map_err(|error| fp_core::error::Error::from(error.to_string()))?),
            mir::ConstValue::Int(value) => {
                self.integer_constant(&self.lir_type_from_ty(ty), *value)
            }
            mir::ConstValue::UInt(value) => {
                self.unsigned_constant(&self.lir_type_from_ty(ty), *value)
            }
            mir::ConstValue::Float(value) => {
                self.float_constant(&self.lir_type_from_ty(ty), *value)
            }
            mir::ConstValue::Str(value) => {
                if let Some(elem_ty) = Self::slice_ref_element_ty(ty) {
                    let elem_lir_ty = self.lir_type_from_ty(elem_ty);
                    let slice_ty = self.slice_lir_type(&elem_lir_ty);
                    let ptr_const = self.const_string_ptr(value);
                    let len_const =
                        self.unsigned_constant(&lir::LirType::I64, value.len() as u64)?;
                    return Ok(lir::LirConstant::aggregate(
                        slice_ty,
                        lir::LirConstantAggregate::Struct(vec![ptr_const, len_const]),
                    ));
                }
                Ok(self.const_string_ptr(value))
            }
            mir::ConstValue::Null => Ok(lir::LirConstant::null(self.lir_type_from_ty(ty))),
            mir::ConstValue::Fn(name) => Ok(lir::LirConstant::function_address(
                self.lir_type_from_ty(ty),
                lir::LirFunctionRef::Name(lir::Name::new(name.as_str().to_string())),
            )),
            // `ty.kind` isn't always `TyKind::Tuple` for a `ConstValue::
            // Tuple` payload — `fp-interpret` stores every register-
            // resident aggregate this way regardless of nominal type, so a
            // struct/enum-typed comptime result (e.g. `Vec::new()`'s
            // `{ptr,len,capacity}`) arrives here as `Tuple` even when `ty`
            // is `TyKind::Adt`. Delegate to `lir_type_from_ty` (which
            // already resolves `Adt` via the substitution-aware
            // `struct_layouts`/`full_layouts` cache, computing on demand
            // via `instantiate_ty` rather than guessing from an
            // unsubstituted or mismatched-instantiation field list) and
            // the generic `LirType`-driven converter below, instead of
            // requiring `ty.kind` to literally be `Tuple`.
            mir::ConstValue::Tuple(elements) => {
                let lir_ty = self.lir_type_from_ty(ty);
                self.const_value_to_lir_constant_with_lir_type(
                    &mir::ConstValue::Tuple(elements.clone()),
                    &lir_ty,
                )
            }
            mir::ConstValue::Array(elements) => {
                let elem_ty = match &ty.kind {
                    TyKind::Array(inner, _) => inner.as_ref(),
                    _ => {
                        return Err(fp_core::error::Error::from(format!(
                            "array constant requires array type hint, got `{ty}`"
                        )));
                    }
                };
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.const_value_to_lir_constant(element, elem_ty)?);
                }
                Ok(lir::LirConstant::aggregate(
                    self.lir_type_from_ty(ty),
                    lir::LirConstantAggregate::Array(lowered),
                ))
            }
            mir::ConstValue::Struct(fields) => {
                let lir_ty = self.lir_type_from_ty(ty);
                let lir::LirType::Struct {
                    fields: lir_fields, ..
                } = &lir_ty
                else {
                    return Err(fp_core::error::Error::from(
                        "struct constant requires a struct layout in LIR",
                    ));
                };
                if lir_fields.len() != fields.len() {
                    return Err(fp_core::error::Error::from(format!(
                        "struct constant field count mismatch: expected {}, got {}",
                        lir_fields.len(),
                        fields.len()
                    )));
                }
                let mut lowered = Vec::with_capacity(fields.len());
                for (idx, field) in fields.iter().enumerate() {
                    let field_lir_ty = lir_fields
                        .get(idx)
                        .ok_or_else(|| {
                            fp_core::error::Error::from("struct constant field type missing")
                        })?
                        .clone();
                    lowered.push(
                        self.const_value_to_lir_constant_with_lir_type(field, &field_lir_ty)?,
                    );
                }
                Ok(lir::LirConstant::aggregate(
                    lir_ty,
                    lir::LirConstantAggregate::Struct(lowered),
                ))
            }
            mir::ConstValue::List { elements, elem_ty } => {
                let elem_lir_ty = self.lir_type_from_ty(elem_ty);
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.const_value_to_lir_constant(element, elem_ty)?);
                }
                let data_global = self.allocate_const_array_global(elem_lir_ty.clone(), lowered);
                let ptr_ty = lir::LirType::Ptr(Box::new(elem_lir_ty.clone()));
                let ptr_const = lir::LirConstant::get_element_ptr(
                    ptr_ty,
                    lir::LirConstant::global_address(
                        lir::LirType::Ptr(Box::new(elem_lir_ty.clone())),
                        data_global.name.clone(),
                    ),
                    Vec::new(),
                    true,
                );
                let slice_ty = self.slice_lir_type(&elem_lir_ty);
                let len_const =
                    self.unsigned_constant(&lir::LirType::I64, elements.len() as u64)?;
                Ok(lir::LirConstant::aggregate(
                    slice_ty,
                    lir::LirConstantAggregate::Struct(vec![ptr_const, len_const]),
                ))
            }
            mir::ConstValue::Map {
                entries,
                key_ty,
                value_ty,
            } => {
                let key_lir_ty = self.lir_type_from_ty(key_ty);
                let value_lir_ty = self.lir_type_from_ty(value_ty);
                let entry_lir_ty = lir::LirType::Struct {
                    fields: vec![key_lir_ty.clone(), value_lir_ty.clone()],
                    packed: false,
                    name: Some("__map_entry".to_string()),
                };
                let mut lowered_entries = Vec::with_capacity(entries.len());
                for (key, value) in entries {
                    let key_val = self.const_value_to_lir_constant(key, key_ty)?;
                    let value_val = self.const_value_to_lir_constant(value, value_ty)?;
                    lowered_entries.push(lir::LirConstant::aggregate(
                        entry_lir_ty.clone(),
                        lir::LirConstantAggregate::Struct(vec![key_val, value_val]),
                    ));
                }
                let data_global =
                    self.allocate_const_array_global(entry_lir_ty.clone(), lowered_entries);
                let ptr_ty = lir::LirType::Ptr(Box::new(entry_lir_ty.clone()));
                let ptr_const = lir::LirConstant::get_element_ptr(
                    ptr_ty,
                    lir::LirConstant::global_address(
                        lir::LirType::Ptr(Box::new(entry_lir_ty.clone())),
                        data_global.name.clone(),
                    ),
                    Vec::new(),
                    true,
                );
                let slice_ty = self.slice_lir_type(&entry_lir_ty);
                let len_const = self.unsigned_constant(&lir::LirType::I64, entries.len() as u64)?;
                Ok(lir::LirConstant::aggregate(
                    slice_ty,
                    lir::LirConstantAggregate::Struct(vec![ptr_const, len_const]),
                ))
            }
        }
    }

    pub(super) fn const_value_to_lir_constant_with_lir_type(
        &mut self,
        value: &mir::ConstValue,
        lir_ty: &lir::LirType,
    ) -> Result<lir::LirConstant> {
        match value {
            mir::ConstValue::Unit => Ok(lir::LirConstant::undef(lir_ty.clone())),
            mir::ConstValue::Bool(value) => Ok(lir::LirConstant::integer(
                lir_ty.clone(),
                lir::LirInteger::I1(*value),
            )
            .map_err(|error| fp_core::error::Error::from(error.to_string()))?),
            mir::ConstValue::Int(value) => self.integer_constant(lir_ty, *value),
            mir::ConstValue::UInt(value) => self.unsigned_constant(lir_ty, *value),
            mir::ConstValue::Float(value) => self.float_constant(lir_ty, *value),
            mir::ConstValue::Str(value) => {
                if let lir::LirType::Struct { fields, .. } = lir_ty {
                    if fields.len() == 2
                        && matches!(&fields[0], lir::LirType::Ptr(inner) if **inner == lir::LirType::I8)
                        && fields[1] == lir::LirType::I64
                    {
                        let ptr_const = self.const_string_ptr(value);
                        let len_const =
                            self.unsigned_constant(&lir::LirType::I64, value.len() as u64)?;
                        return Ok(lir::LirConstant::aggregate(
                            lir_ty.clone(),
                            lir::LirConstantAggregate::Struct(vec![ptr_const, len_const]),
                        ));
                    }
                }
                Ok(self.const_string_ptr(value))
            }
            mir::ConstValue::Null => Ok(lir::LirConstant::null(lir_ty.clone())),
            mir::ConstValue::Fn(name) => Ok(lir::LirConstant::function_address(
                lir_ty.clone(),
                lir::LirFunctionRef::Name(lir::Name::new(name.as_str().to_string())),
            )),
            mir::ConstValue::Array(elements) => {
                let lir::LirType::Array(elem_ty, _len) = lir_ty else {
                    return Err(fp_core::error::Error::from(
                        "array constant requires an array type in LIR",
                    ));
                };
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(
                        self.const_value_to_lir_constant_with_lir_type(element, elem_ty.as_ref())?,
                    );
                }
                Ok(lir::LirConstant::aggregate(
                    lir_ty.clone(),
                    lir::LirConstantAggregate::Array(lowered),
                ))
            }
            mir::ConstValue::Tuple(elements) => {
                let lir::LirType::Struct { fields, .. } = lir_ty else {
                    return Err(fp_core::error::Error::from(
                        "tuple constant requires tuple type hint",
                    ));
                };
                if fields.len() != elements.len() {
                    return Err(fp_core::error::Error::from(format!(
                        "tuple/struct constant field count mismatch: expected {}, got {}",
                        fields.len(),
                        elements.len()
                    )));
                }
                let mut lowered = Vec::with_capacity(elements.len());
                for (idx, element) in elements.iter().enumerate() {
                    let field_ty = fields
                        .get(idx)
                        .ok_or_else(|| fp_core::error::Error::from("missing tuple field type"))?;
                    lowered.push(
                        self.const_value_to_lir_constant_with_lir_type(element, field_ty)?,
                    );
                }
                Ok(lir::LirConstant::aggregate(
                    lir_ty.clone(),
                    lir::LirConstantAggregate::Struct(lowered),
                ))
            }
            mir::ConstValue::Struct(elements) => {
                let lir::LirType::Struct { fields, .. } = lir_ty else {
                    return Err(fp_core::error::Error::from(
                        "tuple/struct constant requires a struct type in LIR",
                    ));
                };
                if fields.len() != elements.len() {
                    return Err(fp_core::error::Error::from(format!(
                        "tuple/struct constant field count mismatch: expected {}, got {}",
                        fields.len(),
                        elements.len()
                    )));
                }
                let mut lowered = Vec::with_capacity(elements.len());
                for (idx, element) in elements.iter().enumerate() {
                    let field_ty = fields
                        .get(idx)
                        .ok_or_else(|| {
                            fp_core::error::Error::from("struct constant field type missing")
                        })?
                        .clone();
                    lowered
                        .push(self.const_value_to_lir_constant_with_lir_type(element, &field_ty)?);
                }
                Ok(lir::LirConstant::aggregate(
                    lir_ty.clone(),
                    lir::LirConstantAggregate::Struct(lowered),
                ))
            }
            mir::ConstValue::List { .. } | mir::ConstValue::Map { .. } => Err(
                fp_core::error::Error::from("container constants require MIR type information"),
            ),
        }
    }

    pub(super) fn allocate_const_array_global(
        &mut self,
        elem_ty: lir::LirType,
        elements: Vec<lir::LirConstant>,
    ) -> lir::LirGlobal {
        let name = lir::Name::new(format!("__const_data_{}", self.const_global_counter));
        self.const_global_counter += 1;
        let array_ty = lir::LirType::Array(Box::new(elem_ty), elements.len() as u64);
        let initializer_constant = lir::LirConstant::aggregate(
            array_ty.clone(),
            lir::LirConstantAggregate::Array(elements),
        );
        let (initializer, relocations) = self
            .canonicalize_global_initializer(initializer_constant, &array_ty)
            .expect("constant array initializer must have a valid layout");
        let align = self.alignment_for_lir_type(&array_ty);
        let global = lir::LirGlobal {
            name,
            ty: array_ty,
            initializer: Some(initializer),
            relocations,
            linkage: lir::Linkage::Internal,
            visibility: lir::Visibility::Hidden,
            is_constant: true,
            alignment: Some(align),
            section: None,
        };
        self.extra_globals.push(global.clone());
        global
    }

    pub(super) fn canonicalize_global_initializer(
        &self,
        initializer: lir::LirConstant,
        ty: &lir::LirType,
    ) -> Result<(lir::LirConstant, Vec<lir::LirGlobalRelocation>)> {
        match &initializer.kind {
            lir::LirConstantKind::Aggregate(_) | lir::LirConstantKind::Data(_) => {
                let (bytes, relocations) =
                    self.try_encode_global_initializer_bytes(&initializer, ty)?;
                Ok((lir::LirConstant::bytes(ty.clone(), bytes), relocations))
            }
            lir::LirConstantKind::GlobalAddress { global } => {
                let size = self
                    .data_layout
                    .size_of(&initializer.ty)
                    .map_err(|error| fp_core::error::Error::from(error.to_string()))?
                    as usize;
                let reloc = lir::LirGlobalRelocation {
                    offset: 0,
                    kind: lir::LirRelocationKind::Abs64,
                    target: lir::LirRelocationTarget::Global(global.clone()),
                    addend: 0,
                };
                Ok((
                    lir::LirConstant::bytes(ty.clone(), vec![0u8; size]),
                    vec![reloc],
                ))
            }
            lir::LirConstantKind::FunctionAddress(function) => {
                let size = self
                    .data_layout
                    .size_of(&initializer.ty)
                    .map_err(|error| fp_core::error::Error::from(error.to_string()))?
                    as usize;
                let lir::LirFunctionRef::Name(name) = function else {
                    return Err(fp_core::error::Error::from(
                        "unsupported non-name function relocation",
                    ));
                };
                let reloc = lir::LirGlobalRelocation {
                    offset: 0,
                    kind: lir::LirRelocationKind::Abs64,
                    target: lir::LirRelocationTarget::Function(name.clone()),
                    addend: 0,
                };
                Ok((
                    lir::LirConstant::bytes(ty.clone(), vec![0u8; size]),
                    vec![reloc],
                ))
            }
            lir::LirConstantKind::Null | lir::LirConstantKind::Undef => {
                let (bytes, relocations) =
                    self.try_encode_global_initializer_bytes(&initializer, ty)?;
                Ok((lir::LirConstant::bytes(ty.clone(), bytes), relocations))
            }
            lir::LirConstantKind::Expr(lir::LirConstantExpr::GetElementPtr {
                base,
                indices,
                ..
            }) if indices.is_empty() => self.canonicalize_global_initializer((**base).clone(), ty),
            lir::LirConstantKind::Poison | lir::LirConstantKind::Expr(_) => {
                Err(fp_core::error::Error::from(
                    "unsupported constant expression in global initializer",
                ))
            }
        }
    }

    pub(super) fn try_encode_global_initializer_bytes(
        &self,
        constant: &lir::LirConstant,
        ty: &lir::LirType,
    ) -> Result<(Vec<u8>, Vec<lir::LirGlobalRelocation>)> {
        let mut bytes = vec![
            0u8;
            self.data_layout
                .size_of(ty)
                .map_err(|error| fp_core::error::Error::from(error.to_string()))?
                as usize
        ];
        let mut relocations = Vec::new();
        self.encode_global_initializer_into(&mut bytes, &mut relocations, 0, constant, ty)
            .ok_or_else(|| {
                fp_core::error::Error::from(format!(
                    "invalid global initializer: constant {:?}, target {:?}",
                    constant, ty
                ))
            })?;
        Ok((bytes, relocations))
    }

    pub(super) fn encode_global_initializer_into(
        &self,
        out: &mut [u8],
        relocations: &mut Vec<lir::LirGlobalRelocation>,
        base: usize,
        constant: &lir::LirConstant,
        ty: &lir::LirType,
    ) -> Option<()> {
        match &constant.kind {
            lir::LirConstantKind::Data(lir::LirConstantData::Integer(value)) => {
                Self::write_initializer_integer(out, base, value, &constant.ty)?;
            }
            lir::LirConstantKind::Data(lir::LirConstantData::Float(value)) => {
                let bits = match value {
                    lir::LirFloat::F32(bits) => u64::from(*bits),
                    lir::LirFloat::F64(bits) => *bits,
                };
                Self::write_initializer_int(
                    out,
                    base,
                    u128::from(bits),
                    self.data_layout.size_of(&constant.ty).ok()? as usize,
                    false,
                )?;
            }
            lir::LirConstantKind::Data(lir::LirConstantData::Bytes(bytes)) => {
                let end = base.checked_add(bytes.len())?;
                out.get_mut(base..end)?.copy_from_slice(bytes);
            }
            lir::LirConstantKind::Aggregate(lir::LirConstantAggregate::Array(elements)) => {
                let lir::LirType::Array(elem_ty, len) = ty else {
                    return None;
                };
                if elements.len() > *len as usize {
                    return None;
                }
                let elem_size = self.data_layout.size_of(elem_ty).ok()? as usize;
                for (idx, element) in elements.iter().enumerate() {
                    self.encode_global_initializer_into(
                        out,
                        relocations,
                        base + idx * elem_size,
                        element,
                        elem_ty,
                    )?;
                }
            }
            lir::LirConstantKind::Aggregate(lir::LirConstantAggregate::Struct(fields)) => {
                let lir::LirType::Struct {
                    fields: field_tys, ..
                } = ty
                else {
                    return None;
                };
                if fields.len() > field_tys.len() {
                    return None;
                }
                let struct_layout = self.data_layout.struct_layout(ty).ok()??;
                for (idx, field) in fields.iter().enumerate() {
                    let field_ty = field_tys.get(idx)?;
                    let field_offset = *struct_layout.field_offsets.get(idx)? as usize;
                    self.encode_global_initializer_into(
                        out,
                        relocations,
                        base + field_offset,
                        field,
                        field_ty,
                    )?;
                }
            }
            lir::LirConstantKind::GlobalAddress { global } => {
                Self::write_initializer_int(
                    out,
                    base,
                    0,
                    self.data_layout.size_of(&constant.ty).ok()? as usize,
                    false,
                )?;
                relocations.push(lir::LirGlobalRelocation {
                    offset: base as u64,
                    kind: lir::LirRelocationKind::Abs64,
                    target: lir::LirRelocationTarget::Global(global.clone()),
                    addend: 0,
                });
            }
            lir::LirConstantKind::FunctionAddress(lir::LirFunctionRef::Name(name)) => {
                Self::write_initializer_int(
                    out,
                    base,
                    0,
                    self.data_layout.size_of(&constant.ty).ok()? as usize,
                    false,
                )?;
                relocations.push(lir::LirGlobalRelocation {
                    offset: base as u64,
                    kind: lir::LirRelocationKind::Abs64,
                    target: lir::LirRelocationTarget::Function(name.clone()),
                    addend: 0,
                });
            }
            lir::LirConstantKind::Expr(lir::LirConstantExpr::GetElementPtr {
                base: inner,
                indices,
                ..
            }) if indices.is_empty() => {
                self.encode_global_initializer_into(out, relocations, base, inner, ty)?;
            }
            lir::LirConstantKind::Null | lir::LirConstantKind::Undef => {
                let size = self.data_layout.size_of(&constant.ty).ok()? as usize;
                let end = base.checked_add(size)?;
                let slot = out.get_mut(base..end)?;
                slot.fill(0);
            }
            _ => return None,
        }
        Some(())
    }

    pub(super) fn write_initializer_integer(
        out: &mut [u8],
        offset: usize,
        value: &lir::LirInteger,
        ty: &lir::LirType,
    ) -> Option<()> {
        let bits = match value {
            lir::LirInteger::I1(value) => u128::from(*value as u8),
            lir::LirInteger::I8(value) => u128::from(*value),
            lir::LirInteger::I16(value) => u128::from(*value),
            lir::LirInteger::I32(value) => u128::from(*value),
            lir::LirInteger::I64(value) => u128::from(*value),
            lir::LirInteger::I128(value) => *value,
            lir::LirInteger::Arbitrary(value) => {
                let mut bits = 0u128;
                for (idx, word) in value.words.iter().take(2).enumerate() {
                    bits |= u128::from(*word) << (idx * 64);
                }
                bits
            }
        };
        let size = match ty {
            lir::LirType::Integer(width) => usize::try_from(width.div_ceil(8)).ok()?,
            lir::LirType::I1 | lir::LirType::I8 => 1,
            lir::LirType::I16 => 2,
            lir::LirType::I32 => 4,
            lir::LirType::I64 => 8,
            lir::LirType::I128 => 16,
            _ => return None,
        };
        Self::write_initializer_int(out, offset, bits, size, false)
    }

    pub(super) fn write_initializer_int(
        out: &mut [u8],
        offset: usize,
        value: u128,
        size: usize,
        signed: bool,
    ) -> Option<()> {
        let end = offset.checked_add(size)?;
        let slot = out.get_mut(offset..end)?;
        let mut bits = value;
        if signed && size < 16 {
            let mask = (1u128 << (size * 8)) - 1;
            bits &= mask;
        }
        for (idx, byte) in slot.iter_mut().enumerate() {
            *byte = (bits >> (idx * 8)) as u8;
        }
        Some(())
    }

    pub(super) fn const_string_ptr(&mut self, value: &str) -> lir::LirConstant {
        let name = if let Some(existing) = self.const_string_globals.get(value) {
            existing.clone()
        } else {
            let mut bytes = Vec::with_capacity(value.len() + 1);
            for byte in value.as_bytes() {
                bytes.push(
                    self.unsigned_constant(&lir::LirType::I8, u64::from(*byte))
                        .expect("byte must fit i8"),
                );
            }
            bytes.push(
                self.unsigned_constant(&lir::LirType::I8, 0)
                    .expect("zero must fit i8"),
            );
            let global = self.allocate_const_array_global(lir::LirType::I8, bytes);
            let name = global.name.clone();
            self.const_string_globals
                .insert(value.to_string(), name.clone());
            name
        };

        lir::LirConstant::get_element_ptr(
            lir::LirType::Ptr(Box::new(lir::LirType::I8)),
            lir::LirConstant::global_address(lir::LirType::Ptr(Box::new(lir::LirType::I8)), name),
            Vec::new(),
            true,
        )
    }
}
