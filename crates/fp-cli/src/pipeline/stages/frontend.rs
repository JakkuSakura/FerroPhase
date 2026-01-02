use super::super::*;

impl Pipeline {
    pub(crate) fn stage_hir_generation(
        &mut self,
        ast: &Node,
        options: &PipelineOptions,
        file_path: Option<&Path>,
        base_path: &Path,
        manager: &DiagnosticManager,
    ) -> Result<hir::Program, CliError> {
        let tolerate_fail = self.bootstrap_mode || options.bootstrap_mode;
        let mut generator = match file_path {
            Some(path) => HirGenerator::with_file(path),
            None => HirGenerator::new(),
        };

        if options.error_tolerance.enabled {
            generator.enable_error_tolerance(options.error_tolerance.max_errors);
        }

        if matches!(
            ast.kind(),
            NodeKind::Item(_) | NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_)
        ) {
            let message = "Top-level items are not supported; provide a file or expression";
            if tolerate_fail {
                if self.should_emit_bootstrap_diagnostic(STAGE_AST_TO_HIR, message) {
                    manager.add_diagnostic(
                        Diagnostic::warning(message.to_string())
                            .with_source_context(STAGE_AST_TO_HIR),
                    );
                }
                return Ok(hir::Program {
                    items: Vec::new(),
                    def_map: HashMap::new(),
                    next_hir_id: 0,
                });
            } else {
                manager.add_diagnostic(
                    Diagnostic::error(message.to_string()).with_source_context(STAGE_AST_TO_HIR),
                );
                return Err(Self::stage_failure(STAGE_AST_TO_HIR));
            }
        }

        // Closure lowering mutates the AST; keep the stage API immutable and
        // operate on a cloned tree that is only used for HIR generation.
        let mut lowered_ast = ast.clone();
        if let Err(err) = lower_closures(&mut lowered_ast) {
            manager.add_diagnostic(
                Diagnostic::error(format!("Closure lowering failed: {}", err))
                    .with_source_context(STAGE_AST_TO_HIR),
            );
            return Err(Self::stage_failure(STAGE_AST_TO_HIR));
        }

        if options.save_intermediates {
            self.save_pretty(&lowered_ast, base_path, "ast-closure", options)?;
        }

        let result = match lowered_ast.kind() {
            NodeKind::Expr(expr) => generator.transform(expr),
            NodeKind::File(file) => generator.transform(file),
            NodeKind::Item(_) => unreachable!(),
            NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => unreachable!(),
        };

        let (errors, warnings) = generator.take_diagnostics();

        let transform_failed = result.is_err();

        if let Err(e) = &result {
            if tolerate_fail {
                let message = format!("AST→HIR transformation failed: {}", e);
                if self.should_emit_bootstrap_diagnostic(STAGE_AST_TO_HIR, &message) {
                    manager.add_diagnostic(
                        Diagnostic::warning(message).with_source_context(STAGE_AST_TO_HIR),
                    );
                }
            } else {
                manager.add_diagnostic(
                    Diagnostic::error(format!("AST→HIR transformation failed: {}", e))
                        .with_source_context(STAGE_AST_TO_HIR),
                );
            }
        }

        if !warnings.is_empty() {
            if tolerate_fail {
                for diagnostic in warnings {
                    let message = diagnostic.to_string();
                    if self.should_emit_bootstrap_diagnostic(STAGE_AST_TO_HIR, &message) {
                        manager.add_diagnostic(
                            Diagnostic::warning(message).with_source_context(STAGE_AST_TO_HIR),
                        );
                    }
                }
            } else {
                manager.add_diagnostics(warnings);
            }
        }

        if !errors.is_empty() {
            if tolerate_fail {
                for diagnostic in errors {
                    let mut warning = diagnostic.as_string_diagnostic();
                    warning.level = DiagnosticLevel::Warning;
                    let message = warning.message.clone();
                    if self.should_emit_bootstrap_diagnostic(STAGE_AST_TO_HIR, &message) {
                        manager.add_diagnostic(warning);
                    }
                }
            } else {
                manager.add_diagnostics(errors);
            }
        }

        if transform_failed {
            if tolerate_fail {
                return Ok(hir::Program {
                    items: Vec::new(),
                    def_map: HashMap::new(),
                    next_hir_id: 0,
                });
            }
            return Err(Self::stage_failure(STAGE_AST_TO_HIR));
        }

        let program = result.expect("hir generation errors accounted for");

        if options.save_intermediates {
            let mut pretty_opts = PrettyOptions::default();
            pretty_opts.show_spans = options.debug.verbose;
            let rendered = format!("{}", pretty(&program, pretty_opts));
            if let Err(err) = fs::write(base_path.with_extension(EXT_HIR), rendered) {
                warn!(error = %err, "failed to persist HIR intermediate");
            }
        }

        Ok(program)
    }
}
