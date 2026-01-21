use super::super::*;
use fp_pipeline::{PipelineDiagnostics, PipelineError, PipelineStage};
use fp_core::ast::{File, ItemKind, Node, NodeKind};
use std::fs;
use std::path::Path;
use std::sync::Arc;

pub(crate) struct NormalizeContext {
    pub ast: Node,
    pub normalizer: Option<Arc<dyn IntrinsicNormalizer>>,
}

pub(crate) struct NormalizeStage;

impl PipelineStage for NormalizeStage {
    type SrcCtx = NormalizeContext;
    type DstCtx = Node;

    fn name(&self) -> &'static str {
        STAGE_INTRINSIC_NORMALIZE
    }

    fn run(
        &self,
        context: NormalizeContext,
        diagnostics: &mut PipelineDiagnostics,
    ) -> Result<Node, PipelineError> {
        let mut ast = context.ast;
        if let Some(normalizer) = context.normalizer.as_ref() {
            if let Err(err) =
                fp_core::intrinsics::normalize_intrinsics_with(&mut ast, &**normalizer)
            {
                diagnostics.push(
                    Diagnostic::error(format!("Intrinsic normalization failed: {}", err))
                        .with_source_context(STAGE_INTRINSIC_NORMALIZE),
                );
                return Err(PipelineError::new(
                    STAGE_INTRINSIC_NORMALIZE,
                    "Intrinsic normalization failed",
                ));
            }
            return Ok(ast);
        }

        match fp_core::intrinsics::normalize_intrinsics(&mut ast) {
            Ok(()) => Ok(ast),
            Err(err) => {
                diagnostics.push(
                    Diagnostic::error(format!("Intrinsic normalization failed: {}", err))
                        .with_source_context(STAGE_INTRINSIC_NORMALIZE),
                );
                Err(PipelineError::new(
                    STAGE_INTRINSIC_NORMALIZE,
                    format!("Intrinsic normalization failed: {}", err),
                ))
            }
        }
    }
}

impl Pipeline {
    pub(crate) fn stage_normalize_intrinsics(
        &mut self,
        ast: &mut Node,
        options: &PipelineOptions,
    ) -> Result<(), CliError> {
        if !ast_has_std(ast) {
            let mut diagnostics = PipelineDiagnostics::default();
            diagnostics.set_display_options(diag::display_options(options));
            let mut merged = ast.clone();
            for std_path in runtime_std_paths() {
                let source = fs::read_to_string(&std_path).map_err(|err| {
                    CliError::Io(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!("failed to read std module {}: {err}", std_path.display()),
                    ))
                })?;
                let language = self.resolve_language(options, Some(&std_path));
                let frontend = self.frontends.get(&language).cloned().ok_or_else(|| {
                    CliError::Io(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!("unsupported source language: {}", language),
                    ))
                })?;
                let mut std_node =
                    self.parse_with_frontend(&frontend, &source, Some(&std_path), options)?;
                if let NodeKind::File(file) = std_node.kind().clone() {
                    let base_dir = std_path.parent().unwrap_or_else(|| Path::new("."));
                    let mut loader = FileModuleLoader::new(self, options, &frontend);
                    let items = loader.resolve_items(&file.items, base_dir)?;
                    std_node = Node::file(File { path: file.path, items });
                }
                merged = merge_std_module(
                    merged,
                    std_node,
                    &mut diagnostics,
                    STAGE_INTRINSIC_NORMALIZE,
                )
                .map_err(|_| Self::stage_failure(STAGE_INTRINSIC_NORMALIZE))?;
            }
            *ast = merged;
        }
        let lang_items = fp_core::lang::collect_lang_items(ast);
        fp_core::lang::register_threadlocal_lang_items(lang_items);
        let stage = NormalizeStage;
        let context = NormalizeContext {
            ast: ast.clone(),
            normalizer: self.intrinsic_normalizer.clone(),
        };
        let next_ast =
            self.run_pipeline_stage(STAGE_INTRINSIC_NORMALIZE, stage, context, options)?;
        *ast = next_ast;
        Ok(())
    }
}

fn ast_has_std(ast: &Node) -> bool {
    let NodeKind::File(file) = ast.kind() else {
        return false;
    };
    file.items.iter().any(|item| {
        matches!(
            item.kind(),
            ItemKind::Module(module) if module.name.as_str() == "std"
        )
    })
}
