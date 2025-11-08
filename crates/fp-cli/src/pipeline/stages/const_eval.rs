use super::super::*;

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

        // Enforce: in strict mode (non-tolerant), no quote/splice remain after const-eval
        if !options.error_tolerance.enabled {
            if ast_contains_quote_or_splice(ast) {
                manager.add_diagnostic(
                    Diagnostic::error(
                        "quote/splice nodes remain after const-eval in strict mode".to_string(),
                    )
                    .with_source_context(STAGE_CONST_EVAL),
                );
                return Err(Self::stage_failure(STAGE_CONST_EVAL));
            }
        }

        Ok(outcome)
    }
}
