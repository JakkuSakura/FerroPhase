use super::{Expr, Symbol};
use crate::hir::ty::Ty;

/// A refinement annotation encountered by `fp_typing::check_type_expr`,
/// recorded on the owning `HirPackage` (see `HirPackage::refinement_hints`)
/// so a caller that still has the same `TypeExpr`/function signature (e.g.
/// the `Let` arm, or a later call site of an already-checked function) can
/// look it up and discharge it against the value actually being coerced,
/// without re-deriving it from source. Lives in `fp-core` (rather than
/// `fp-typing`, where the actual `decide`/`omega` discharge procedures do)
/// purely because `HirPackage` needs to name this type to cache it —
/// discharging one is still entirely `fp-typing`'s concern.
#[derive(Debug, Clone, PartialEq)]
pub struct RefinementHint {
    pub binder: Symbol,
    pub predicate: Expr,
    pub base: Ty,
}

/// Which part of a function's signature a persisted `RefinementHint`
/// belongs to — see `HirPackage::refinement_hints`'s doc comment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParamSlot {
    Input(usize),
    Output,
}
