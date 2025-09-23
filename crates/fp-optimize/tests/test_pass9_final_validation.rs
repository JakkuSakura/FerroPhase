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
fn records_and_clears_pending_const_eval_ops() -> Result<()> {
    let mut orchestrator = make_orchestrator();
    assert!(orchestrator.get_const_eval_ops().is_empty());

    orchestrator.record_const_eval(ConstEval::CompileWarning {
        message: "test warning".into(),
    });
    orchestrator.record_const_eval(ConstEval::CompileError {
        message: "test error".into(),
    });

    let pending = orchestrator.get_const_eval_ops();
    assert_eq!(pending.len(), 2);

    orchestrator.clear_const_eval_ops();
    assert!(orchestrator.get_const_eval_ops().is_empty());
    Ok(())
}
