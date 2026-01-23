use super::super::*;
use fp_pipeline::{PipelineDiagnostics, PipelineError, PipelineStage};
use tracing::warn;

pub(crate) struct FrontendContext {
    pub ast: Node,
    pub options: PipelineOptions,
    pub file_path: Option<PathBuf>,
    pub base_path: PathBuf,
}

pub(crate) struct FrontendStage;

impl PipelineStage for FrontendStage {
    type SrcCtx = FrontendContext;
    type DstCtx = hir::Program;

    fn name(&self) -> &'static str {
        STAGE_AST_TO_HIR
    }

    fn run(
        &self,
        context: FrontendContext,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<hir::Program, PipelineError> {
        let diag_manager = fp_core::diagnostics::diagnostic_manager();
        let diag_snapshot = diag_manager.snapshot();
        let mut generator = match context.file_path.as_deref() {
            Some(path) => HirGenerator::with_file(path),
            None => HirGenerator::new(),
        };

        if matches!(
            context.ast.kind(),
            NodeKind::Item(_) | NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_)
        ) {
            let message = "Top-level items are not supported; provide a file or expression";
            diagnostics
                .push(Diagnostic::error(message.to_string()).with_source_context(STAGE_AST_TO_HIR));
            return Ok(hir::Program {
                items: Vec::new(),
                def_map: std::collections::HashMap::new(),
                next_hir_id: 0,
            });
        }

        let result = match context.ast.kind() {
            NodeKind::Expr(expr) => generator.transform_expr(expr),
            NodeKind::File(file) => generator.transform_file(file),
            NodeKind::Item(_) => unreachable!(),
            NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => unreachable!(),
        };

        let new_diagnostics = diag_manager.diagnostics_since(diag_snapshot);
        diagnostics.extend(new_diagnostics);

        let program = match result {
            Ok(program) => program,
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!("AST→HIR transformation failed: {}", err))
                        .with_source_context(STAGE_AST_TO_HIR),
                );
                return Ok(hir::Program {
                    items: Vec::new(),
                    def_map: std::collections::HashMap::new(),
                    next_hir_id: 0,
                });
            }
        };

        if context.options.save_intermediates {
            let mut pretty_opts = PrettyOptions::default();
            pretty_opts.show_spans = context.options.debug.verbose;
            let rendered = format!("{}", pretty(&program, pretty_opts));
            if let Err(err) = fs::write(context.base_path.with_extension(EXT_HIR), rendered) {
                warn!(error = %err, "failed to persist HIR intermediate");
            }
        }

        Ok(program)
    }
}

impl Pipeline {
    pub(crate) fn stage_hir_generation(
        &mut self,
        ast: &Node,
        options: &PipelineOptions,
        file_path: Option<&Path>,
        base_path: &Path,
    ) -> Result<hir::Program, CliError> {
        let stage = FrontendStage;
        let context = FrontendContext {
            ast: ast.clone(),
            options: options.clone(),
            file_path: file_path.map(Path::to_path_buf),
            base_path: base_path.to_path_buf(),
        };
        self.run_pipeline_stage(STAGE_AST_TO_HIR, stage, context, options)
    }
}
