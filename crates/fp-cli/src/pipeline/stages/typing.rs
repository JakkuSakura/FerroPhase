use super::super::*;
use fp_pipeline::{PipelineDiagnostics, PipelineError, PipelineStage};
use fp_core::config;

pub(crate) struct TypingContext {
    pub ast: Node,
    pub stage_label: &'static str,
}

pub(crate) struct TypingStage {
    pub stage_label: &'static str,
}

impl PipelineStage for TypingStage {
    type SrcCtx = TypingContext;
    type DstCtx = Node;

    fn name(&self) -> &'static str {
        self.stage_label
    }

    fn run(
        &self,
        context: TypingContext,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<Node, PipelineError> {
        let mut ast = context.ast;
        match fp_typing::annotate(&mut ast) {
            Ok(outcome) => {
                let mut saw_error = false;
                for message in outcome.diagnostics {
                    let mut diagnostic = match message.level {
                        TypingDiagnosticLevel::Warning => Diagnostic::warning(message.message)
                            .with_source_context(context.stage_label),
                        TypingDiagnosticLevel::Error => {
                            saw_error = true;
                            Diagnostic::error(message.message)
                                .with_source_context(context.stage_label)
                        }
                    };
                    if let Some(span) = message.span {
                        diagnostic = diagnostic.with_span(span);
                    }
                    diagnostics.push(diagnostic);
                }

                if saw_error || outcome.has_errors {
                    return Err(PipelineError::new(
                        context.stage_label,
                        "AST typing reported errors",
                    ));
                }
                Ok(ast)
            }
            Err(err) => {
                let diagnostic = match err {
                    fp_core::error::Error::Diagnostic(diag) => {
                        diag.with_source_context(context.stage_label)
                    }
                    err => Diagnostic::error(format!("AST typing failed: {}", err))
                        .with_source_context(context.stage_label),
                };
                diagnostics.push(diagnostic);
                Err(PipelineError::new(context.stage_label, "AST typing failed"))
            }
        }
    }
}

impl Pipeline {
    pub(crate) fn stage_type_check(
        &mut self,
        ast: &mut Node,
        stage_label: &'static str,
        options: &PipelineOptions,
    ) -> Result<(), CliError> {
        let stage = TypingStage { stage_label };
        let context = TypingContext {
            ast: ast.clone(),
            stage_label,
        };
        match self.run_pipeline_stage(stage_label, stage, context, options) {
            Ok(next_ast) => {
                *ast = next_ast;
                Ok(())
            }
            Err(err) => {
                if options.error_tolerance.enabled || config::lossy_mode() {
                    let diagnostic = Diagnostic::warning(format!(
                        "AST typing failed; continuing due to error tolerance (language={})",
                        self.source_language.as_deref().unwrap_or("unknown")
                    ))
                    .with_source_context(stage_label);
                    diag::emit(&[diagnostic], Some(stage_label), options);
                    Ok(())
                } else {
                    Err(err)
                }
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn stage_type_check_for_tests(&mut self, ast: &mut Node) -> Result<(), CliError> {
        let options = PipelineOptions::default();
        self.stage_type_check(ast, STAGE_TYPE_ENRICH, &options)
    }
}
