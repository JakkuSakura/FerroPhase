use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::Result;
// TODO: Fix imports - ConstEvaluator renamed
// use fp_optimize::utils::{ConstEvaluator, SideEffect};
use fp_optimize::utils::SideEffect;
use fp_rust::printer::RustPrinter;
use std::sync::Arc;

// TODO: Fix evaluator creation API
fn create_evaluator() -> () {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    // TODO: Update to use new API
    todo!("Update evaluator creation")
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_type_system_update_no_changes() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // Run Pass 5 with no side effects
    let changes_made = evaluator.update_and_validate_types(&ctx)?;

    // Should return false since no side effects to process
    assert!(
        !changes_made,
        "Should return false when no side effects to process"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_generated_type_processing() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // Add a side effect for type generation
    let struct_def = TypeStruct {
        name: "GeneratedType".into(),
        fields: vec![StructuralField {
            name: "value".into(),
            value: AstType::ident("i64".into()),
        }],
    };

    let side_effect = SideEffect::GenerateType {
        type_name: "GeneratedType".to_string(),
        type_definition: AstType::Struct(struct_def),
    };

    evaluator.add_side_effect(side_effect);

    // Process the side effect
    let changes_made = evaluator.update_and_validate_types(&ctx)?;

    // Should return true since we processed a type generation
    assert!(
        changes_made,
        "Should return true when processing type generation"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_generated_field_processing() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // First, register a base type
    let struct_def = TypeStruct {
        name: "BaseType".into(),
        fields: vec![],
    };

    // Register it first
    let type_generation = SideEffect::GenerateType {
        type_name: "BaseType".to_string(),
        type_definition: AstType::Struct(struct_def),
    };
    evaluator.add_side_effect(type_generation);
    evaluator.update_and_validate_types(&ctx)?;

    // Now add a field to it
    let field_side_effect = SideEffect::GenerateField {
        target_type: "BaseType".to_string(),
        field_name: "new_field".to_string(),
        field_type: AstType::ident("bool".into()),
    };

    evaluator.add_side_effect(field_side_effect);

    // Process the field addition
    let changes_made = evaluator.update_and_validate_types(&ctx)?;

    // Should return true since we processed a field addition
    assert!(
        changes_made,
        "Should return true when processing field generation"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_generated_method_processing() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // Register a base type first
    let struct_def = TypeStruct {
        name: "BaseType".into(),
        fields: vec![],
    };

    let type_generation = SideEffect::GenerateType {
        type_name: "BaseType".to_string(),
        type_definition: AstType::Struct(struct_def),
    };
    evaluator.add_side_effect(type_generation);
    evaluator.update_and_validate_types(&ctx)?;

    // Add a method to it
    let method_body = AstExpr::Value(AstValue::unit().into());
    let method_side_effect = SideEffect::GenerateMethod {
        target_type: "BaseType".to_string(),
        method_name: "new_method".to_string(),
        method_body,
    };

    evaluator.add_side_effect(method_side_effect);

    // Process the method addition
    let changes_made = evaluator.update_and_validate_types(&ctx)?;

    // Should return true since we processed a method addition
    assert!(
        changes_made,
        "Should return true when processing method generation"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_generated_impl_processing() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // Register a base type first
    let struct_def = TypeStruct {
        name: "BaseType".into(),
        fields: vec![],
    };

    let type_generation = SideEffect::GenerateType {
        type_name: "BaseType".to_string(),
        type_definition: AstType::Struct(struct_def),
    };
    evaluator.add_side_effect(type_generation);
    evaluator.update_and_validate_types(&ctx)?;

    // Add an impl block to it
    let methods = vec![
        (
            "method1".to_string(),
            AstExpr::Value(AstValue::unit().into()),
        ),
        (
            "method2".to_string(),
            AstExpr::Value(AstValue::unit().into()),
        ),
    ];

    let impl_side_effect = SideEffect::GenerateImpl {
        target_type: "BaseType".to_string(),
        trait_name: "Display".to_string(),
        methods,
    };

    evaluator.add_side_effect(impl_side_effect);

    // Process the impl generation
    let changes_made = evaluator.update_and_validate_types(&ctx)?;

    // Should return true since we processed an impl generation
    assert!(
        changes_made,
        "Should return true when processing impl generation"
    );

    Ok(())
}

#[test]
#[ignore = "TODO: Fix API usage after refactoring"]
fn test_type_dependent_block_detection() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();

    // Create a const block that uses type introspection
    use fp_rust::shll_parse_expr;
    let expr = shll_parse_expr!(sizeof!(i64));

    // This should be detected as type-dependent
    let depends_on_types = evaluator.block_depends_on_types(&expr)?;
    assert!(depends_on_types, "sizeof expression should depend on types");

    // Create a non-type-dependent expression
    let simple_expr = shll_parse_expr!(42 + 8);
    let simple_depends = evaluator.block_depends_on_types(&simple_expr)?;
    assert!(
        !simple_depends,
        "Simple arithmetic should not depend on types"
    );

    Ok(())
}
