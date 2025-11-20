use super::super::*;

impl Pipeline {
    pub(crate) fn stage_normalize_intrinsics(
        &self,
        ast: &mut Node,
        manager: &DiagnosticManager,
    ) -> Result<(), CliError> {
        if let Some(normalizer) = self.intrinsic_normalizer.as_ref() {
            // Always run the shared intrinsic normalization pass, delegating
            // language-specific hooks (e.g., macro lowering) to the provided
            // frontend normalizer.
            if let Err(err) =
                fp_optimize::passes::normalize_intrinsics_with(ast, Some(&**normalizer))
            {
                manager.add_diagnostic(
                    Diagnostic::error(format!("Intrinsic normalization failed: {}", err))
                        .with_source_context(STAGE_INTRINSIC_NORMALIZE),
                );
                return Err(Self::stage_failure(STAGE_INTRINSIC_NORMALIZE));
            }
            return Ok(());
        }

        match normalize_intrinsics(ast) {
            Ok(()) => Ok(()),
            Err(err) => {
                manager.add_diagnostic(
                    Diagnostic::error(format!("Intrinsic normalization failed: {}", err))
                        .with_source_context(STAGE_INTRINSIC_NORMALIZE),
                );
                Err(Self::stage_failure(STAGE_INTRINSIC_NORMALIZE))
            }
        }
    }
}
