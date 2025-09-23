use fp_core::ast::*;
use fp_core::Result;
use fp_optimize::orchestrators::ConstEvaluationOrchestrator;
use fp_optimize::utils::ConstEval;
use fp_rust::printer::RustPrinter;
use std::sync::Arc;

fn make_orchestrator() -> ConstEvaluationOrchestrator {
    let printer = Arc::new(RustPrinter::new());
    register_threadlocal_serializer(printer.clone());
    ConstEvaluationOrchestrator::new(printer)
}

#[test]
fn applying_without_operations_is_noop() -> Result<()> {
    let mut orchestrator = make_orchestrator();
    let mut module = AstModule {
        visibility: Visibility::Public,
        name: "test_module".into(),
        items: Vec::new(),
    };

    let changed = orchestrator.apply_const_eval_ops_to_module(&mut module)?;
    assert!(
        !changed,
        "no pending const-eval ops should leave module untouched"
    );
    assert!(module.items.is_empty());
    Ok(())
}

#[test]
fn generates_trait_impl_with_methods() -> Result<()> {
    let mut orchestrator = make_orchestrator();
    let mut module = AstModule {
        visibility: Visibility::Public,
        name: "traits".into(),
        items: vec![AstItem::DefStruct(ItemDefStruct {
            visibility: Visibility::Public,
            name: "Device".into(),
            value: TypeStruct {
                name: "Device".into(),
                fields: vec![],
            },
        })],
    };

    orchestrator.record_const_eval(ConstEval::GenerateImpl {
        target_type: "Device".to_string(),
        trait_name: "Operate".to_string(),
        methods: vec![
            ("start".into(), AstExpr::Value(AstValue::unit().into())),
            ("stop".into(), AstExpr::Value(AstValue::unit().into())),
        ],
    });

    let changed = orchestrator.apply_const_eval_ops_to_module(&mut module)?;
    assert!(changed);

    assert_eq!(module.items.len(), 2);
    let impl_block = match &module.items[1] {
        AstItem::Impl(block) => block,
        other => panic!("expected impl block, got {other:?}"),
    };

    let trait_name = impl_block
        .trait_ty
        .as_ref()
        .and_then(|locator| locator.as_ident())
        .map(|ident| ident.name.to_string())
        .expect("trait name");
    assert_eq!(trait_name, "Operate");
    assert_eq!(impl_block.items.len(), 2);

    let method_names: Vec<_> = impl_block
        .items
        .iter()
        .map(|item| match item {
            AstItem::DefFunction(func) => func.name.name.clone(),
            other => panic!("expected function, got {other:?}"),
        })
        .collect();
    assert!(method_names.contains(&"start".to_string()));
    assert!(method_names.contains(&"stop".to_string()));

    Ok(())
}
