//! MIR->HIR lift: turns an already-resolved `mir::Constant`/`mir::ConstValue`
//! back into the `hir::Value` shape HIR-level consumers (typing's own
//! `HirPackage::const_values`/`const_block_values`, `eval_script`,
//! interpreter global-seeding) read directly. Pure structural pattern
//! matching, no lowering context needed — the mirror image of
//! `lir_to_mir::LirToMir`, which handles the other direction (needs ADT
//! layout context, hence a stateful struct instead of bare functions).

use fp_core::hir;
use fp_core::mir;

pub struct MirToHir;

impl MirToHir {
    /// The reverse of `lir_to_mir::LirToMir::value_to_mir_constant` —
    /// needed because a directly-foldable top-level const (e.g.
    /// `const X = 1 + 2 * 3;`, no `let` needed) never becomes a comptime
    /// entry requiring the interpreter (see `MirLowering::lower_const`'s
    /// constant-folding fast path), so its value never reaches the
    /// package's own `HirPackage::const_values` the way an interpreted one
    /// does unless something converts its already-computed `mir::Constant`
    /// back into a `hir::Value`.
    pub fn constant_to_value(constant: &mir::Constant) -> Option<hir::Value> {
        match &constant.literal {
            mir::ConstantKind::Bool(v) => Some(hir::Value::bool(*v)),
            mir::ConstantKind::Int(v) => Some(hir::Value::int(*v)),
            mir::ConstantKind::UInt(v) => Some(hir::Value::uint(*v)),
            mir::ConstantKind::Float(v) => Some(hir::Value::decimal(*v)),
            mir::ConstantKind::Str(v) => Some(hir::Value::string(v.clone())),
            mir::ConstantKind::Null => Some(hir::Value::null()),
            mir::ConstantKind::Val(value) => Self::const_value_to_value(value),
            // A function reference, token stream, or global-path constant
            // has no meaningful runtime `Value` representation outside
            // actual execution — an honest "can't convert this" rather
            // than a placeholder.
            mir::ConstantKind::Ty(_)
            | mir::ConstantKind::Fn(_)
            | mir::ConstantKind::FnDef(_, _)
            | mir::ConstantKind::Global(_)
            | mir::ConstantKind::TokenStream { .. }
            | mir::ConstantKind::Undef => None,
        }
    }

    pub fn const_value_to_value(value: &mir::ConstValue) -> Option<hir::Value> {
        match value {
            mir::ConstValue::Unit => Some(hir::Value::unit()),
            mir::ConstValue::Bool(v) => Some(hir::Value::bool(*v)),
            mir::ConstValue::Int(v) => Some(hir::Value::int(*v)),
            mir::ConstValue::UInt(v) => Some(hir::Value::uint(*v)),
            mir::ConstValue::Float(v) => Some(hir::Value::decimal(*v)),
            mir::ConstValue::Str(v) => Some(hir::Value::string(v.clone())),
            mir::ConstValue::Null => Some(hir::Value::null()),
            // The comptime interpreter represents every positional
            // aggregate (tuple *or* struct) as `Value::Tuple` — see
            // `LirToMir::value_to_const_value`'s own doc comment on the
            // same asymmetry in the forward direction.
            mir::ConstValue::Tuple(values) | mir::ConstValue::Struct(values) => {
                let values = values
                    .iter()
                    .map(Self::const_value_to_value)
                    .collect::<Option<Vec<_>>>()?;
                Some(hir::Value::Tuple(fp_core::ast::ValueTuple::new(values)))
            }
            mir::ConstValue::Array(values) => {
                let values = values
                    .iter()
                    .map(Self::const_value_to_value)
                    .collect::<Option<Vec<_>>>()?;
                Some(hir::Value::List(fp_core::ast::ValueList::new(values)))
            }
            mir::ConstValue::List { elements, .. } => {
                let values = elements
                    .iter()
                    .map(Self::const_value_to_value)
                    .collect::<Option<Vec<_>>>()?;
                Some(hir::Value::List(fp_core::ast::ValueList::new(values)))
            }
            // No `Value::Map` constructor exists to convert into (see
            // `all_adt_field_tys`'s neighbors) — an honest "can't convert
            // this" rather than a placeholder.
            mir::ConstValue::Map { .. } => None,
            // A function reference has no meaningful runtime `Value`
            // representation outside actual execution.
            mir::ConstValue::Fn(_) => None,
        }
    }
}
