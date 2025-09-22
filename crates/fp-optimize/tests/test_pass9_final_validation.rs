use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::Result;
use fp_optimize::utils::{ConstEvalState, ConstEvaluator, SideEffect};
use fp_rust::printer::RustPrinter;
use std::sync::Arc;

fn create_evaluator() -> ConstEvaluator {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    ConstEvaluator::new(Arc::new(RustPrinter::new()))
}

fn create_test_module_with_struct() -> AstModule {
    AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: vec![AstItem::DefStruct(ItemDefStruct {
            visibility: Visibility::Public,
            name: "TestStruct".into(),
            value: TypeStruct {
                name: "TestStruct".into(),
                fields: vec![
                    StructuralField {
                        name: "id".into(),
                        value: AstType::ident("i64".into()),
                    },
                    StructuralField {
                        name: "name".into(),
                        value: AstType::ident("String".into()),
                    },
                ],
            },
        })],
    }
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_final_validation_clean_module() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    let module = create_test_module_with_struct();

    // Run final type validation on a clean module
    let validation_passed = evaluator.final_type_validation(&module, &ctx)?;

    // Should pass since the module has valid types
    assert!(
        validation_passed,
        "Final validation should pass for valid module"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_final_validation_with_unknown_type() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // Create module with unknown field type
    let module = AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: vec![AstItem::DefStruct(ItemDefStruct {
            visibility: Visibility::Public,
            name: "BadStruct".into(),
            value: TypeStruct {
                name: "BadStruct".into(),
                fields: vec![StructuralField {
                    name: "unknown_field".into(),
                    value: AstType::ident("UnknownType".into()), // This type doesn't exist
                }],
            },
        })],
    };

    // Run final type validation
    let validation_passed = evaluator.final_type_validation(&module, &ctx)?;

    // Should fail due to unknown type reference
    assert!(
        !validation_passed,
        "Final validation should fail for unknown type references"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_final_validation_with_impl_blocks() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // Create module with struct and impl block
    let mut module = create_test_module_with_struct();

    // Add an impl block
    let method_def = ItemDefFunction::new_simple(
        "get_id".into(),
        AstExpr::Value(AstValue::unit().into()).into(),
    );

    let impl_def = ItemImpl {
        trait_ty: None,
        self_ty: AstExpr::ident("TestStruct".into()),
        items: vec![AstItem::DefFunction(method_def)],
    };

    module.items.push(AstItem::Impl(impl_def));

    // Run final type validation
    let validation_passed = evaluator.final_type_validation(&module, &ctx)?;

    // Should pass since impl is for known type
    assert!(
        validation_passed,
        "Final validation should pass with valid impl blocks"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_final_validation_impl_unknown_type() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // Create module with impl for unknown type
    let module = AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: vec![AstItem::Impl(ItemImpl {
            trait_ty: None,
            self_ty: AstExpr::ident("UnknownStruct".into()),
            items: vec![],
        })],
    };

    // Run final type validation
    let validation_passed = evaluator.final_type_validation(&module, &ctx)?;

    // Should fail due to impl for unknown type
    assert!(
        !validation_passed,
        "Final validation should fail for impl on unknown type"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_final_validation_with_trait_impl() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // Create module with trait impl
    let mut module = create_test_module_with_struct();

    // Add trait impl
    let display_method =
        ItemDefFunction::new_simple("fmt".into(), AstExpr::Value(AstValue::unit().into()).into());

    let trait_impl = ItemImpl {
        trait_ty: Some(fp_core::id::Locator::Ident("Display".into())),
        self_ty: AstExpr::ident("TestStruct".into()),
        items: vec![AstItem::DefFunction(display_method)],
    };

    module.items.push(AstItem::Impl(trait_impl));

    // Run final type validation
    let validation_passed = evaluator.final_type_validation(&module, &ctx)?;

    // Should pass (trait existence is not strictly checked in current implementation)
    assert!(
        validation_passed,
        "Final validation should handle trait impls"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_unresolved_type_references_detection() -> Result<()> {
    let evaluator = create_evaluator();

    // Create module with unresolved references
    let module = AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: vec![AstItem::DefStruct(ItemDefStruct {
            visibility: Visibility::Public,
            name: "StructWithUnresolvedTypes".into(),
            value: TypeStruct {
                name: "StructWithUnresolvedTypes".into(),
                fields: vec![
                    StructuralField {
                        name: "field1".into(),
                        value: AstType::ident("UnresolvedType1".into()),
                    },
                    StructuralField {
                        name: "field2".into(),
                        value: AstType::ident("UnresolvedType2".into()),
                    },
                    StructuralField {
                        name: "field3".into(),
                        value: AstType::ident("i64".into()), // This is a primitive, should be OK
                    },
                ],
            },
        })],
    };

    // Find unresolved type references
    let unresolved = evaluator.find_unresolved_type_references(&module)?;

    // Should find the two unresolved types
    assert_eq!(unresolved.len(), 2);
    assert!(unresolved.contains(&"UnresolvedType1".to_string()));
    assert!(unresolved.contains(&"UnresolvedType2".to_string()));

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_primitive_type_recognition() -> Result<()> {
    let evaluator = create_evaluator();

    // Test all primitive types are recognized
    let primitives = vec![
        "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64", "bool", "char", "str",
        "String",
    ];

    for primitive in primitives {
        assert!(
            evaluator.is_primitive_type(primitive),
            "Should recognize {} as primitive type",
            primitive
        );
    }

    // Test non-primitive type is not recognized
    assert!(
        !evaluator.is_primitive_type("CustomType"),
        "Should not recognize CustomType as primitive"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_readiness_check_with_pending_side_effects() -> Result<()> {
    let evaluator = create_evaluator();

    // Initially should be ready
    assert!(
        evaluator.is_ready_for_final_validation(),
        "Should be ready when no side effects or unevaluated blocks"
    );

    // Add a side effect
    let side_effect = SideEffect::GenerateType {
        type_name: "GeneratedType".to_string(),
        type_definition: AstType::Struct(TypeStruct {
            name: "GeneratedType".into(),
            fields: vec![],
        }),
    };

    evaluator.add_side_effect(side_effect);

    // Should not be ready now
    assert!(
        !evaluator.is_ready_for_final_validation(),
        "Should not be ready with pending side effects"
    );

    // Clear side effects
    evaluator.clear_side_effects();

    // Should be ready again
    assert!(
        evaluator.is_ready_for_final_validation(),
        "Should be ready after clearing side effects"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_readiness_check_with_unevaluated_blocks() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // Create and add an unevaluated const block
    let const_expr = AstExpr::Value(AstValue::int(42).into());
    let block_id = evaluator.register_const_block(&const_expr, None, &ctx)?;

    // Should not be ready with unevaluated blocks
    assert!(
        !evaluator.is_ready_for_final_validation(),
        "Should not be ready with unevaluated const blocks"
    );

    // Evaluate the const block (simulate evaluation)
    evaluator.update_const_block_state(
        block_id,
        ConstEvalState::Evaluated,
        Some(AstValue::int(42)),
    );

    // Should be ready now
    assert!(
        evaluator.is_ready_for_final_validation(),
        "Should be ready after evaluating all const blocks"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_freeze_type_system_state() -> Result<()> {
    let evaluator = create_evaluator();

    // Add some types to the registry
    use fp_core::ctx::ty::{TypeId, TypeInfo};

    let type_info = TypeInfo {
        id: TypeId::new(),
        name: "FreezeTestType".to_string(),
        ast_type: AstType::ident("FreezeTestType".into()),
        size_bytes: Some(8),
        fields: vec![],
        methods: vec![],
        traits_implemented: vec![],
    };

    evaluator.get_type_registry().register_type(type_info);

    // Freeze the type system
    let freeze_result = evaluator.freeze_type_system_state()?;

    // Should have frozen at least 1 type
    assert!(
        freeze_result.frozen_types_count >= 1,
        "Should have frozen at least one type"
    );
    assert!(
        freeze_result.snapshot_created,
        "Should have created snapshot"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_comprehensive_validation_workflow() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // Create a comprehensive module
    let mut module = create_test_module_with_struct();

    // Register TestStruct in the type registry (simulate what type validation would do)
    use fp_core::ctx::ty::{FieldInfo, TypeId, TypeInfo};

    let test_struct_type_info = TypeInfo {
        id: TypeId::new(),
        name: "TestStruct".to_string(),
        ast_type: AstType::Struct(TypeStruct {
            name: "TestStruct".into(),
            fields: vec![
                StructuralField {
                    name: "id".into(),
                    value: AstType::ident("i64".into()),
                },
                StructuralField {
                    name: "name".into(),
                    value: AstType::ident("String".into()),
                },
            ],
        }),
        size_bytes: None,
        fields: vec![
            FieldInfo {
                name: "id".to_string(),
                type_id: TypeId::new(),
                ast_type: AstType::ident("i64".into()),
                attributes: vec![],
            },
            FieldInfo {
                name: "name".to_string(),
                type_id: TypeId::new(),
                ast_type: AstType::ident("String".into()),
                attributes: vec![],
            },
        ],
        methods: vec![],
        traits_implemented: vec![],
    };

    evaluator
        .get_type_registry()
        .register_type(test_struct_type_info);

    // Add generated types from side effects
    let generated_struct = TypeStruct {
        name: "GeneratedStruct".into(),
        fields: vec![StructuralField {
            name: "generated_field".into(),
            value: AstType::ident("TestStruct".into()), // Reference to existing struct
        }],
    };

    let generated_type_item = AstItem::DefStruct(ItemDefStruct {
        visibility: Visibility::Public,
        name: "GeneratedStruct".into(),
        value: generated_struct,
    });

    module.items.push(generated_type_item);

    // Should be ready for final validation
    assert!(evaluator.is_ready_for_final_validation());

    // Run final validation
    let validation_passed = evaluator.final_type_validation(&module, &ctx)?;

    // Debug: check what unresolved references exist
    if !validation_passed {
        let unresolved = evaluator.find_unresolved_type_references(&module)?;
        println!("Debug: Unresolved type references: {:?}", unresolved);
    }

    // Should pass comprehensive validation
    assert!(validation_passed, "Comprehensive validation should pass");

    Ok(())
}
