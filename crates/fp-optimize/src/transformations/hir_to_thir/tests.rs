use super::*;
use crate::transformations::ast_to_hir::HirGenerator;
use fp_core::ast::{register_threadlocal_serializer, File};
use fp_core::Result;
use fp_rust::{printer::RustPrinter, shll_parse_items};
use std::sync::Arc;

#[test]
fn test_thir_generator_creation() {
    let generator = ThirGenerator::new();
    assert_eq!(generator.next_thir_id, 0);
}

#[test]
fn test_literal_transformation() -> Result<()> {
    let mut generator = ThirGenerator::new();
    let (thir_lit, ty) = generator.transform_literal(hir::Lit::Integer(42)).unwrap();

    match thir_lit {
        thir::Lit::Int(value, thir::IntTy::I32) => {
            assert_eq!(value, 42);
        }
        _ => {
            return Err(crate::error::optimization_error(
                "Expected i32 literal".to_string(),
            ))
        }
    }

    assert!(ty.is_integral());
    Ok(())
}

#[test]
fn test_binary_op_transformation() {
    let generator = ThirGenerator::new();
    let op = generator.transform_binary_op(hir::BinOp::Add);
    assert_eq!(op, thir::BinOp::Add);
}

#[test]
fn lowers_impl_items() -> Result<()> {
    let printer = Arc::new(RustPrinter::new());
    register_threadlocal_serializer(printer.clone());

    let items = shll_parse_items! {
        struct Point {
            x: i64,
        }

        impl Point {
            fn get_x(&self) -> i64 {
                self.x
            }
        }
    };

    let ast_file = File {
        path: "thir_test.fp".into(),
        items,
    };

    let mut hir_gen = HirGenerator::new();
    let hir_program = hir_gen.transform_file(&ast_file)?;

    let mut thir_gen = ThirGenerator::new();
    let thir_program = thir_gen.transform(hir_program)?;

    assert!(thir_program
        .items
        .iter()
        .any(|item| matches!(item.kind, thir::ItemKind::Struct(_))));
    assert!(thir_program
        .items
        .iter()
        .any(|item| matches!(item.kind, thir::ItemKind::Impl(_))));

    let point_id = thir_gen
        .type_context
        .lookup_struct_def_id("Point")
        .expect("point id");
    let point_ty = thir_gen
        .type_context
        .make_struct_ty_by_id(point_id, Vec::new())
        .expect("point type registered");
    let (idx, field_ty) = thir_gen
        .type_context
        .lookup_field_info(&point_ty, "x")
        .expect("field info");
    assert_eq!(idx, 0);
    assert!(matches!(
        field_ty.kind,
        types::TyKind::Int(types::IntTy::I64)
    ));

    let method_sig = thir_gen
        .type_context
        .lookup_method_signature(&point_ty, "get_x")
        .expect("method signature");
    assert!(matches!(
        method_sig.output.kind,
        types::TyKind::Int(types::IntTy::I64)
    ));

    Ok(())
}
