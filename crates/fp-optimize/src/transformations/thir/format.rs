use fp_core::hir::typed::ty::{FloatTy, IntTy, Ty, TyKind, UintTy};
use fp_core::hir::typed::{Expr, FormatArgRef, FormatString, FormatTemplatePart};

pub fn build_printf_format(format: &FormatString, args: &[Expr], newline: bool) -> String {
    debug_assert!(
        format.kwargs.is_empty(),
        "kwargs not supported in println! lowering"
    );

    let mut result = String::new();
    let mut implicit_index = 0usize;

    for part in &format.parts {
        match part {
            FormatTemplatePart::Literal(text) => result.push_str(text),
            FormatTemplatePart::Placeholder(placeholder) => {
                let arg_index = match &placeholder.arg_ref {
                    FormatArgRef::Implicit => {
                        let current = implicit_index;
                        implicit_index += 1;
                        current
                    }
                    FormatArgRef::Positional(index) => *index,
                    FormatArgRef::Named(name) => panic!(
                        "Named argument '{}' should have been rejected during lowering",
                        name
                    ),
                };

                let expr = args.get(arg_index).unwrap_or_else(|| {
                    panic!(
                        "Format placeholder references missing argument at index {}",
                        arg_index
                    )
                });

                let spec = placeholder
                    .format_spec
                    .clone()
                    .unwrap_or_else(|| infer_printf_spec(&expr.ty));
                result.push_str(&spec);
            }
        }
    }

    if newline && !result.ends_with('\n') {
        result.push('\n');
    }

    result
}

fn infer_printf_spec(ty: &Ty) -> String {
    match &ty.kind {
        TyKind::Bool => "%d".to_string(),
        TyKind::Char => "%c".to_string(),
        TyKind::Int(int_ty) => match int_ty {
            IntTy::I8 => "%hhd".to_string(),
            IntTy::I16 => "%hd".to_string(),
            IntTy::I32 => "%d".to_string(),
            IntTy::I64 => "%lld".to_string(),
            IntTy::I128 => "%lld".to_string(),
            IntTy::Isize => "%ld".to_string(),
        },
        TyKind::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => "%hhu".to_string(),
            UintTy::U16 => "%hu".to_string(),
            UintTy::U32 => "%u".to_string(),
            UintTy::U64 => "%llu".to_string(),
            UintTy::U128 => "%llu".to_string(),
            UintTy::Usize => "%lu".to_string(),
        },
        TyKind::Float(float_ty) => match float_ty {
            FloatTy::F32 => "%f".to_string(),
            FloatTy::F64 => "%f".to_string(),
        },
        TyKind::RawPtr(_) | TyKind::Ref(..) => "%p".to_string(),
        other => panic!("Cannot infer printf spec for type {:?}", other),
    }
}
