use fp_core::ast::*;
use fp_core::Result;
use fp_optimize::orchestrators::ConstEvaluationOrchestrator;
use fp_optimize::utils::ConstEval;
use fp_optimize::ConstEvaluationOrchestrator;
use fp_rust::printer::RustPrinter;
use std::sync::Arc;

fn make_orchestrator() -> ConstEvaluationOrchestrator {
    let printer = Arc::new(RustPrinter::new());
    register_threadlocal_serializer(printer.clone());
    ConstEvaluationOrchestrator::new(printer)
}

fn empty_module(name: &str) -> AstModule {
    AstModule {
        visibility: Visibility::Public,
        name: name.into(),
        items: Vec::new(),
    }
}

#[test]
fn generates_new_struct_type() -> Result<()> {
    let mut orchestrator = make_orchestrator();
    let mut module = empty_module("test_module");

    orchestrator.record_const_eval(ConstEval::GenerateType {
        type_name: "GeneratedType".to_string(),
        type_definition: AstType::Struct(TypeStruct {
            name: "GeneratedType".into(),
            fields: vec![StructuralField::new(
                "value".into(),
                AstType::ident("i64".into()),
            )],
        }),
    });

    let changed = orchestrator.apply_const_eval_ops_to_module(&mut module)?;
    assert!(changed, "applying const-eval ops should modify the module");
    assert_eq!(module.items.len(), 1);

    let item = &module.items[0];
    match item {
        AstItem::DefStruct(def) => {
            assert_eq!(def.name.name, "GeneratedType");
            assert_eq!(def.value.fields.len(), 1);
            assert_eq!(def.value.fields[0].name.name, "value");
        }
        other => panic!("expected struct definition, got {other:?}"),
    }

    Ok(())
}

#[test]
fn augments_existing_struct_with_field() -> Result<()> {
    let mut orchestrator = make_orchestrator();
    let mut module = empty_module("test_module");
    module.items.push(AstItem::DefStruct(ItemDefStruct {
        visibility: Visibility::Public,
        name: "Base".into(),
        value: TypeStruct {
            name: "Base".into(),
            fields: vec![],
        },
    }));

    orchestrator.record_const_eval(ConstEval::GenerateField {
        target_type: "Base".to_string(),
        field_name: "extra".to_string(),
        field_type: AstType::ident("bool".into()),
    });

    let changed = orchestrator.apply_const_eval_ops_to_module(&mut module)?;
    assert!(changed);

    match &module.items[0] {
        AstItem::DefStruct(def) => {
            assert_eq!(def.value.fields.len(), 1);
            assert_eq!(def.value.fields[0].name.name, "extra");
            assert_eq!(def.value.fields[0].value.to_string(), "bool");
        }
        _ => panic!("expected struct definition"),
    }

    Ok(())
}

#[test]
fn generates_impl_with_method() -> Result<()> {
    let mut orchestrator = make_orchestrator();
    let mut module = empty_module("test_module");
    module.items.push(AstItem::DefStruct(ItemDefStruct {
        visibility: Visibility::Public,
        name: "Widget".into(),
        value: TypeStruct {
            name: "Widget".into(),
            fields: vec![],
        },
    }));

    orchestrator.record_const_eval(ConstEval::GenerateMethod {
        target_type: "Widget".to_string(),
        method_name: "tick".to_string(),
        method_body: AstExpr::Value(AstValue::unit().into()),
    });

    let changed = orchestrator.apply_const_eval_ops_to_module(&mut module)?;
    assert!(changed);

    assert_eq!(module.items.len(), 2, "struct plus generated impl");
    match &module.items[1] {
        AstItem::Impl(impl_block) => {
            assert!(impl_block.trait_ty.is_none());
            assert!(matches!(impl_block.self_ty.as_ref(), AstExpr::Locator(_)));
            assert_eq!(impl_block.items.len(), 1);
            match &impl_block.items[0] {
                AstItem::DefFunction(func) => {
                    assert_eq!(func.name.name, "tick");
                }
                _ => panic!("expected generated function"),
            }
        }
        other => panic!("expected impl block, got {other:?}"),
    }

    Ok(())
}
