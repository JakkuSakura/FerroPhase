use super::super::*;
use fp_core::ast::Node;
use std::fs;

impl Pipeline {
    pub(crate) fn stage_const_eval(
        &mut self,
        ast: &mut Node,
        options: &PipelineOptions,
        manager: &DiagnosticManager,
    ) -> Result<ConstEvalOutcome, CliError> {
        let serializer = self.serializer.clone().ok_or_else(|| {
            CliError::Compilation("No serializer registered for const-eval".to_string())
        })?;
        register_threadlocal_serializer(serializer.clone());

        let shared_context = SharedScopedContext::new();
        let mut orchestrator = ConstEvaluationOrchestrator::new(serializer);
        orchestrator.set_debug_assertions(!options.release);
        orchestrator.set_execute_main(options.execute_main);

        for std_path in runtime_std_paths() {
            let source = fs::read_to_string(&std_path).map_err(|e| {
                CliError::Compilation(format!(
                    "failed to read std module {}: {}",
                    std_path.display(),
                    e
                ))
            })?;
            let std_node = self.parse_input_source(options, &source, Some(&std_path))?;
            *ast = merge_std_module(ast.clone(), std_node, manager, STAGE_CONST_EVAL)?;
        }

        let outcome = match orchestrator.evaluate(ast, &shared_context) {
            Ok(outcome) => outcome,
            Err(e) => {
                let diagnostic = Diagnostic::error(format!("Const evaluation failed: {}", e))
                    .with_source_context(STAGE_CONST_EVAL);
                manager.add_diagnostic(diagnostic);
                return Err(Self::stage_failure(STAGE_CONST_EVAL));
            }
        };

        manager.add_diagnostics(outcome.diagnostics.clone());
        if outcome.has_errors {
            return Err(Self::stage_failure(STAGE_CONST_EVAL));
        }

        Ok(outcome)
    }
}
