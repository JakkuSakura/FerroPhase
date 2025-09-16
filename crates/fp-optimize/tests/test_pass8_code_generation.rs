use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::id::Locator;
use fp_core::Result;
use fp_optimize::utils::{ConstEvaluator, SideEffect};
use fp_rust_lang::printer::RustPrinter;
use std::sync::Arc;

fn create_evaluator() -> ConstEvaluator {
    register_threadlocal_serializer(Arc::new(RustPrinter::new()));
    ConstEvaluator::new(Arc::new(RustPrinter::new()))
}

#[test]
fn test_code_generation_no_side_effects() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    let mut module = AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: vec![],
    };
    
    // Run Pass 8 with no side effects
    let changes_made = evaluator.generate_and_modify_ast(&mut module, &ctx)?;
    
    // Should return false since no side effects to process
    assert!(!changes_made, "Should return false when no side effects to process");
    
    // Module should remain unchanged
    assert!(module.items.is_empty(), "Module should remain empty");
    
    Ok(())
}

#[test]
fn test_generate_type_in_ast() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    let mut module = AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: vec![],
    };
    
    // Add a side effect for type generation
    let struct_def = TypeStruct {
        name: "GeneratedStruct".into(),
        fields: vec![
            StructuralField {
                name: "value".into(),
                value: AstType::ident("i64".into()),
            }
        ],
    };
    
    let side_effect = SideEffect::GenerateType {
        type_name: "GeneratedStruct".to_string(),
        type_definition: AstType::Struct(struct_def),
    };
    
    evaluator.add_side_effect(side_effect);
    
    // Process the side effect
    let changes_made = evaluator.generate_and_modify_ast(&mut module, &ctx)?;
    
    // Should return true since we processed a type generation
    assert!(changes_made, "Should return true when processing type generation");
    
    // Module should now contain the generated struct
    assert_eq!(module.items.len(), 1, "Module should contain one generated item");
    
    if let AstItem::DefStruct(struct_def) = &module.items[0] {
        assert_eq!(struct_def.name.name, "GeneratedStruct");
        assert_eq!(struct_def.value.fields.len(), 1);
        assert_eq!(struct_def.value.fields[0].name.name, "value");
    } else {
        panic!("Expected DefStruct item");
    }
    
    Ok(())
}

#[test]
fn test_generate_field_in_ast() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    let mut module = AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: vec![
            // Create a base struct to add field to
            AstItem::DefStruct(ItemDefStruct {
                visibility: Visibility::Public,
                name: "BaseStruct".into(),
                value: TypeStruct {
                    name: "BaseStruct".into(),
                    fields: vec![],
                },
            })
        ],
    };
    
    // Add a side effect for field generation
    let field_side_effect = SideEffect::GenerateField {
        target_type: "BaseStruct".to_string(),
        field_name: "new_field".to_string(),
        field_type: AstType::ident("bool".into()),
    };
    
    evaluator.add_side_effect(field_side_effect);
    
    // Process the field addition
    let changes_made = evaluator.generate_and_modify_ast(&mut module, &ctx)?;
    
    // Should return true since we processed a field addition
    assert!(changes_made, "Should return true when processing field generation");
    
    // Check that the field was added
    if let AstItem::DefStruct(struct_def) = &module.items[0] {
        assert_eq!(struct_def.value.fields.len(), 1);
        assert_eq!(struct_def.value.fields[0].name.name, "new_field");
    } else {
        panic!("Expected DefStruct item");
    }
    
    Ok(())
}

#[test]
fn test_generate_method_in_ast_new_impl() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    let mut module = AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: vec![
            // Create a base struct to add method to
            AstItem::DefStruct(ItemDefStruct {
                visibility: Visibility::Public,
                name: "BaseStruct".into(),
                value: TypeStruct {
                    name: "BaseStruct".into(),
                    fields: vec![],
                },
            })
        ],
    };
    
    // Add a side effect for method generation
    let method_body = AstExpr::Value(AstValue::unit().into());
    let method_side_effect = SideEffect::GenerateMethod {
        target_type: "BaseStruct".to_string(),
        method_name: "new_method".to_string(),
        method_body,
    };
    
    evaluator.add_side_effect(method_side_effect);
    
    // Process the method addition
    let changes_made = evaluator.generate_and_modify_ast(&mut module, &ctx)?;
    
    // Should return true since we processed a method addition
    assert!(changes_made, "Should return true when processing method generation");
    
    // Module should now have 2 items: struct + impl
    assert_eq!(module.items.len(), 2, "Module should contain struct and impl block");
    
    // Check the impl block was created
    if let AstItem::Impl(impl_def) = &module.items[1] {
        assert!(impl_def.trait_ty.is_none(), "Should be regular impl, not trait impl");
        assert_eq!(impl_def.items.len(), 1, "Should have one method");
        
        if let AstItem::DefFunction(func_def) = &impl_def.items[0] {
            assert_eq!(func_def.name.name, "new_method");
        } else {
            panic!("Expected DefFunction item in impl block");
        }
    } else {
        panic!("Expected Impl item");
    }
    
    Ok(())
}

#[test]
fn test_generate_method_in_existing_impl() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    
    // Create existing method
    let existing_method = ItemDefFunction::new_simple(
        "existing_method".into(),
        AstExpr::Value(AstValue::unit().into()).into()
    );
    
    let mut module = AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: vec![
            // Create a struct with existing impl block
            AstItem::DefStruct(ItemDefStruct {
                visibility: Visibility::Public,
                name: "BaseStruct".into(),
                value: TypeStruct {
                    name: "BaseStruct".into(),
                    fields: vec![],
                },
            }),
            AstItem::Impl(ItemImpl {
                trait_ty: None,
                self_ty: AstExpr::Locator(Locator::Ident("BaseStruct".into())),
                items: vec![AstItem::DefFunction(existing_method)],
            })
        ],
    };
    
    // Add a side effect for method generation
    let method_body = AstExpr::Value(AstValue::unit().into());
    let method_side_effect = SideEffect::GenerateMethod {
        target_type: "BaseStruct".to_string(),
        method_name: "new_method".to_string(),
        method_body,
    };
    
    evaluator.add_side_effect(method_side_effect);
    
    // Process the method addition
    let changes_made = evaluator.generate_and_modify_ast(&mut module, &ctx)?;
    
    // Should return true since we processed a method addition
    assert!(changes_made, "Should return true when processing method generation");
    
    // Check that the method was added to existing impl
    if let AstItem::Impl(impl_def) = &module.items[1] {
        assert_eq!(impl_def.items.len(), 2, "Should have two methods now");
        
        // Check both methods exist
        let method_names: Vec<String> = impl_def.items.iter()
            .filter_map(|item| {
                if let AstItem::DefFunction(func) = item {
                    Some(func.name.name.clone())
                } else {
                    None
                }
            })
            .collect();
        
        assert!(method_names.contains(&"existing_method".to_string()));
        assert!(method_names.contains(&"new_method".to_string()));
    } else {
        panic!("Expected Impl item");
    }
    
    Ok(())
}

#[test]
fn test_generate_trait_impl() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    let mut module = AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: vec![
            // Create a base struct
            AstItem::DefStruct(ItemDefStruct {
                visibility: Visibility::Public,
                name: "BaseStruct".into(),
                value: TypeStruct {
                    name: "BaseStruct".into(),
                    fields: vec![],
                },
            })
        ],
    };
    
    // Add a side effect for impl generation
    let methods = vec![
        ("method1".to_string(), AstExpr::Value(AstValue::unit().into())),
        ("method2".to_string(), AstExpr::Value(AstValue::unit().into())),
    ];
    
    let impl_side_effect = SideEffect::GenerateImpl {
        target_type: "BaseStruct".to_string(),
        trait_name: "Display".to_string(),
        methods,
    };
    
    evaluator.add_side_effect(impl_side_effect);
    
    // Process the impl generation
    let changes_made = evaluator.generate_and_modify_ast(&mut module, &ctx)?;
    
    // Should return true since we processed an impl generation
    assert!(changes_made, "Should return true when processing impl generation");
    
    // Module should now have 2 items: struct + trait impl
    assert_eq!(module.items.len(), 2, "Module should contain struct and trait impl");
    
    // Check the trait impl was created
    if let AstItem::Impl(impl_def) = &module.items[1] {
        // Should be trait impl
        assert!(impl_def.trait_ty.is_some(), "Should be trait impl");
        if let Some(trait_locator) = &impl_def.trait_ty {
            if let Some(trait_ident) = trait_locator.as_ident() {
                assert_eq!(trait_ident.name, "Display");
            } else {
                panic!("Expected trait ident");
            }
        }
        
        // Should have 2 methods
        assert_eq!(impl_def.items.len(), 2, "Should have two methods");
        
        let method_names: Vec<String> = impl_def.items.iter()
            .filter_map(|item| {
                if let AstItem::DefFunction(func) = item {
                    Some(func.name.name.clone())
                } else {
                    None
                }
            })
            .collect();
        
        assert!(method_names.contains(&"method1".to_string()));
        assert!(method_names.contains(&"method2".to_string()));
    } else {
        panic!("Expected Impl item");
    }
    
    Ok(())
}

#[test]
fn test_duplicate_generation_skipped() -> Result<()> {
    let evaluator = create_evaluator();
    let ctx = SharedScopedContext::new();
    let mut module = AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: vec![
            // Create a struct that already exists
            AstItem::DefStruct(ItemDefStruct {
                visibility: Visibility::Public,
                name: "ExistingStruct".into(),
                value: TypeStruct {
                    name: "ExistingStruct".into(),
                    fields: vec![],
                },
            })
        ],
    };
    
    // Try to generate the same type again
    let struct_def = TypeStruct {
        name: "ExistingStruct".into(),
        fields: vec![],
    };
    
    let side_effect = SideEffect::GenerateType {
        type_name: "ExistingStruct".to_string(),
        type_definition: AstType::Struct(struct_def),
    };
    
    evaluator.add_side_effect(side_effect);
    
    // Process the side effect
    let changes_made = evaluator.generate_and_modify_ast(&mut module, &ctx)?;
    
    // Should return false since struct already exists
    assert!(!changes_made, "Should return false when struct already exists");
    
    // Module should still have only one item
    assert_eq!(module.items.len(), 1, "Module should still have only one struct");
    
    Ok(())
}