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
    let mut module = Module {
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
fn generates_inherent_methods() -> Result<()> {
    let mut orchestrator = make_orchestrator();
    let mut module = Module {
        visibility: Visibility::Public,
        name: "traits".into(),
        items: vec![Item::DefStruct(ItemDefStruct {
            visibility: Visibility::Public,
            name: "Device".into(),
            value: TypeStruct {
                name: "Device".into(),
                fields: vec![],
            },
        })],
    };

    orchestrator.record_const_eval(ConstEval::GenerateMethod {
        target_type: "Device".to_string(),
        method_name: "start".into(),
        method_body: Expr::Value(Value::unit().into()),
    });
    orchestrator.record_const_eval(ConstEval::GenerateMethod {
        target_type: "Device".to_string(),
        method_name: "stop".into(),
        method_body: Expr::Value(Value::unit().into()),
    });

    let changed = orchestrator.apply_const_eval_ops_to_module(&mut module)?;
    assert!(changed);

    assert_eq!(module.items.len(), 2);
    let inherent_impl = match &module.items[1] {
        Item::Impl(block) => block,
        other => panic!("expected impl block, got {other:?}"),
    };

    assert!(inherent_impl.trait_ty.is_none());
    assert_eq!(inherent_impl.items.len(), 2);

    let method_names: Vec<_> = inherent_impl
        .items
        .iter()
        .map(|item| match item {
            Item::DefFunction(func) => func.name.name.clone(),
            other => panic!("expected function, got {other:?}"),
        })
        .collect();
    assert!(method_names.contains(&"start".to_string()));
    assert!(method_names.contains(&"stop".to_string()));

    Ok(())
}
