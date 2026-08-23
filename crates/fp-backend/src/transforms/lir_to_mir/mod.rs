//! LIR->MIR lift: turns the runtime `ast::Value` a comptime entry's
//! `LirInterpreter` execution produced back into the `mir::Constant`/
//! `mir::ConstValue` shape MIR/LIR can embed as a literal. Mirror image of
//! `hir_to_mir`'s lowering direction — this crate's `MirToHir` (in the
//! sibling `mir_to_hir` module) continues the lift the rest of the way
//! back to a HIR-level value.

use fp_core::ast::Value;
use fp_core::mir;
use fp_core::mir::ty::{FloatTy, IntTy, TyKind, UintTy};
use fp_core::span::Span;
use fp_core::hir;

/// Owns exactly the context this lift direction needs: every already-
/// compiled package's `MirPackage::adt_defs`, consulted only when a
/// comptime result's declared type is a nominal `Adt` and its real field
/// layout is needed to shape a `ConstValue::Struct`.
pub struct LirToMir {
    packages: Vec<mir::MirPackage>,
}

impl LirToMir {
    pub fn new(packages: Vec<mir::MirPackage>) -> Self {
        Self { packages }
    }

    /// The authoritative source for an Adt's real field list — populated by
    /// `fp-backend`'s `take_adt_defs()` specifically so a downstream
    /// consumer with no live `MirLowering` (like this one) can look it up.
    /// Never use `Ty::Adt(adt_def, _).variants` directly: it's deliberately
    /// left empty by several real construction paths (`adt_shell_ty`, the
    /// general Adt case in `lower_hir_ty`) that only ever needed to convey
    /// type *identity*, not full field layout.
    fn lookup_real_adt_def(&self, def_id: hir::DefId) -> Option<mir::ty::AdtDef> {
        self.packages
            .iter()
            .find_map(|p| p.adt_defs.get(&def_id).cloned())
    }

    pub fn value_to_mir_constant(&self, value: &Value, ty: &mir::Ty) -> Option<mir::Constant> {
        let literal = match value {
            Value::Bool(value) => mir::ConstantKind::Bool(value.value),
            Value::Int(value) => mir::ConstantKind::Int(value.value),
            Value::UInt(value) => mir::ConstantKind::UInt(value.value),
            Value::Decimal(value) => mir::ConstantKind::Float(value.value),
            Value::String(value) => mir::ConstantKind::Str(value.value.clone()),
            Value::Bytes(bytes) => {
                let s = String::from_utf8_lossy(&bytes.value)
                    .trim_end_matches('\0')
                    .to_string();
                mir::ConstantKind::Str(s)
            }
            Value::Null(_) => mir::ConstantKind::Null,
            _ => mir::ConstantKind::Val(self.value_to_const_value(value, ty)?),
        };
        Some(mir::Constant {
            span: Span::null(),
            ty: ty.clone(),
            user_ty: None,
            literal,
        })
    }

    pub fn value_to_const_value(&self, value: &Value, ty: &mir::Ty) -> Option<mir::ConstValue> {
        match value {
            Value::Unit(_) => Some(mir::ConstValue::Unit),
            Value::Bool(value) => Some(mir::ConstValue::Bool(value.value)),
            Value::Int(value) => Some(match ty.kind {
                TyKind::Uint(UintTy::Usize)
                | TyKind::Uint(UintTy::U8)
                | TyKind::Uint(UintTy::U16)
                | TyKind::Uint(UintTy::U32)
                | TyKind::Uint(UintTy::U64)
                | TyKind::Uint(UintTy::U128) => mir::ConstValue::UInt(value.value as u64),
                _ => mir::ConstValue::Int(value.value),
            }),
            Value::UInt(value) => Some(match ty.kind {
                TyKind::Int(IntTy::Isize)
                | TyKind::Int(IntTy::I8)
                | TyKind::Int(IntTy::I16)
                | TyKind::Int(IntTy::I32)
                | TyKind::Int(IntTy::I64)
                | TyKind::Int(IntTy::I128) => mir::ConstValue::Int(value.value as i64),
                _ => mir::ConstValue::UInt(value.value),
            }),
            Value::Decimal(value) => Some(match ty.kind {
                TyKind::Float(FloatTy::F32) | TyKind::Float(FloatTy::F64) => {
                    mir::ConstValue::Float(value.value)
                }
                _ => return None,
            }),
            Value::String(value) => Some(mir::ConstValue::Str(value.value.clone())),
            Value::Bytes(bytes) => {
                let s = String::from_utf8_lossy(&bytes.value)
                    .trim_end_matches('\0')
                    .to_string();
                Some(mir::ConstValue::Str(s))
            }
            Value::Null(_) => Some(mir::ConstValue::Null),
            // A raw pointer's comptime value is just its address — e.g.
            // `Vec::new()`'s `ptr: *mut T` field, always null before any
            // allocation happens. There's no dedicated pointer/address
            // `ConstValue` variant, so mirror `Value::Null`'s treatment
            // for a null address (the only case that can arise from a
            // `const`/`const fn` evaluation — a real heap/stack address
            // from a genuinely *runtime* allocation has no meaningful
            // representation as a compile-time constant at all) and
            // otherwise surface the address as a plain integer.
            Value::Pointer(pointer) => Some(if pointer.value == 0 {
                mir::ConstValue::Null
            } else {
                mir::ConstValue::UInt(pointer.value as u64)
            }),
            // `fp-interpret` stores every register-resident aggregate as a
            // plain `Value::Tuple` regardless of its nominal type (structs
            // included — see `default_value_for_type`/`load_value_at`), so
            // a struct/enum-typed comptime result (e.g. `Vec::new()`'s
            // `Vec<T>{ptr,len,capacity}`) arrives here as `Value::Tuple`
            // even though `ty.kind` is `TyKind::Adt`, not `TyKind::Tuple`.
            // Mirror the `Value::Struct`/`TyKind::Adt` arm below rather
            // than rejecting it.
            Value::Tuple(tuple) => match &ty.kind {
                TyKind::Tuple(fields) => {
                    if tuple.values.len() != fields.len() {
                        return None;
                    }
                    let values = tuple
                        .values
                        .iter()
                        .zip(fields.iter())
                        .map(|(value, field_ty)| self.value_to_const_value(value, field_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Tuple(values))
                }
                // Never derive field info from `adt_def.variants` directly
                // (see `lookup_real_adt_def`'s doc comment) — look up the
                // real, registered `AdtDef` instead, and convert each field
                // against its own declared `Ty` rather than blindly
                // guessing (the previous untyped conversion always
                // produced a signed `Int` even for an unsigned field).
                TyKind::Adt(adt_def, _substs) => {
                    let variant = self
                        .lookup_real_adt_def(adt_def.did)?
                        .variants
                        .first()?
                        .clone();
                    if tuple.values.len() != variant.fields.len() {
                        return None;
                    }
                    let values = tuple
                        .values
                        .iter()
                        .zip(variant.fields.iter())
                        .map(|(value, field_def)| self.value_to_const_value(value, &field_def.ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                _ => None,
            },
            Value::List(list) => match &ty.kind {
                TyKind::Array(elem_ty, _) => {
                    let values = list
                        .values
                        .iter()
                        .map(|value| self.value_to_const_value(value, elem_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Array(values))
                }
                TyKind::Slice(elem_ty) => {
                    let values = list
                        .values
                        .iter()
                        .map(|value| self.value_to_const_value(value, elem_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::List {
                        elements: values,
                        elem_ty: elem_ty.as_ref().clone(),
                    })
                }
                _ => None,
            },
            Value::Struct(value_struct) => match &ty.kind {
                TyKind::Tuple(fields) => {
                    if value_struct.structural.fields.len() != fields.len() {
                        return None;
                    }
                    let values = value_struct
                        .structural
                        .fields
                        .iter()
                        .zip(fields.iter())
                        .map(|(field, field_ty)| self.value_to_const_value(&field.value, field_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                TyKind::Adt(adt_def, _substs) => {
                    let variant = self
                        .lookup_real_adt_def(adt_def.did)?
                        .variants
                        .first()?
                        .clone();
                    if value_struct.structural.fields.len() != variant.fields.len() {
                        return None;
                    }
                    let values = value_struct
                        .structural
                        .fields
                        .iter()
                        .zip(variant.fields.iter())
                        .map(|(field, field_def)| {
                            self.value_to_const_value(&field.value, &field_def.ty)
                        })
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                _ => return None,
            },
            Value::Structural(structural) => match &ty.kind {
                TyKind::Tuple(fields) => {
                    if structural.fields.len() != fields.len() {
                        return None;
                    }
                    let values = structural
                        .fields
                        .iter()
                        .zip(fields.iter())
                        .map(|(field, field_ty)| self.value_to_const_value(&field.value, field_ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                TyKind::Adt(adt_def, _substs) => {
                    let variant = self
                        .lookup_real_adt_def(adt_def.did)?
                        .variants
                        .first()?
                        .clone();
                    if structural.fields.len() != variant.fields.len() {
                        return None;
                    }
                    let values = structural
                        .fields
                        .iter()
                        .zip(variant.fields.iter())
                        .map(|(field, field_def)| {
                            self.value_to_const_value(&field.value, &field_def.ty)
                        })
                        .collect::<Option<Vec<_>>>()?;
                    Some(mir::ConstValue::Struct(values))
                }
                _ => None,
            },
            _ => None,
        }
    }
}
