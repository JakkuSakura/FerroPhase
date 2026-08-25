use super::*;

impl AstToHirLowerer {
    pub(super) fn borrowed_string_from_bytes(bytes: &ast::ValueBytes) -> Option<String> {
        let raw = bytes.value.as_ref();
        let trimmed = raw.strip_suffix(&[0])?;
        std::str::from_utf8(trimmed).ok().map(str::to_string)
    }

    /// Lowers an AST `Value::Bytes` expression, produced either by a real
    /// `b"..."`/`c"..."` literal (`ast/expr.rs::parse_string`, which
    /// attaches a `&[u8; N]`/`&std::ffi::CStr` `ty_slot` to disambiguate
    /// the two) or by some other, older producer of a bare `Value::Bytes`
    /// with no such type hint (the Python frontend, `fp-interpret`'s
    /// raw-memory intrinsics) — preserved via the same UTF-8-plus-
    /// trailing-NUL fallback this used to always take.
    pub(super) fn transform_bytes_value_to_hir(
        bytes: &ast::ValueBytes,
        ty: Option<&ast::Ty>,
    ) -> hir::ExprKind {
        let raw: Vec<u8> = bytes.value.as_ref().to_vec();
        if let Some(ast::Ty::Reference(reference)) = ty {
            return if matches!(reference.ty.as_ref(), ast::Ty::Array(_)) {
                hir::ExprKind::Literal(hir::Lit::Bytes(raw))
            } else {
                hir::ExprKind::Literal(hir::Lit::CStr(raw))
            };
        }
        if let Some(text) = Self::borrowed_string_from_bytes(bytes) {
            hir::ExprKind::Literal(hir::Lit::Str(text))
        } else {
            hir::ExprKind::Literal(hir::Lit::Bytes(raw))
        }
    }
}
