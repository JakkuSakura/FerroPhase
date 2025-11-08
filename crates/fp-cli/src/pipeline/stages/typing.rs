use super::super::*;

impl Pipeline {
    pub(crate) fn stage_type_check(
        &mut self,
        ast: &mut Node,
        stage_label: &'static str,
        manager: &DiagnosticManager,
        options: &PipelineOptions,
    ) -> Result<(), CliError> {
        let tolerate_fail = self.bootstrap_mode || options.bootstrap_mode;
        match fp_typing::annotate(ast) {
            Ok(outcome) => {
                let mut saw_error = false;
                for message in outcome.diagnostics {
                    let msg_text = message.message.clone();
                    if tolerate_fail
                        && !self.should_emit_bootstrap_diagnostic(stage_label, &msg_text)
                    {
                        continue;
                    }
                    let diagnostic = match message.level {
                        TypingDiagnosticLevel::Warning => {
                            Diagnostic::warning(message.message).with_source_context(stage_label)
                        }
                        TypingDiagnosticLevel::Error => {
                            if tolerate_fail {
                                Diagnostic::warning(message.message)
                                    .with_source_context(stage_label)
                            } else {
                                saw_error = true;
                                Diagnostic::error(message.message).with_source_context(stage_label)
                            }
                        }
                    };
                    manager.add_diagnostic(diagnostic);
                }

                if (saw_error || outcome.has_errors) && !tolerate_fail {
                    return Err(Self::stage_failure(stage_label));
                }
                Ok(())
            }
            Err(err) => {
                let diagnostic = if tolerate_fail {
                    let message = format!("AST typing failed: {}", err);
                    if !self.should_emit_bootstrap_diagnostic(stage_label, &message) {
                        return Ok(());
                    }
                    Diagnostic::warning(message).with_source_context(stage_label)
                } else {
                    Diagnostic::error(format!("AST typing failed: {}", err))
                        .with_source_context(stage_label)
                };
                manager.add_diagnostic(diagnostic);
                if tolerate_fail {
                    Ok(())
                } else {
                    Err(Self::stage_failure(stage_label))
                }
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn stage_type_check_for_tests(
        &mut self,
        ast: &mut Node,
        manager: &DiagnosticManager,
    ) -> Result<(), CliError> {
        let options = PipelineOptions::default();
        self.stage_type_check(ast, STAGE_TYPE_ENRICH, manager, &options)
    }
}
