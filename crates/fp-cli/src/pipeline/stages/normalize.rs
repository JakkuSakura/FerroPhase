use super::super::*;
use fp_pipeline::{PipelineDiagnostics, PipelineError, PipelineStage};
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
        &self,
        ast: &mut Node,
        options: &PipelineOptions,
    ) -> Result<(), CliError> {
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
