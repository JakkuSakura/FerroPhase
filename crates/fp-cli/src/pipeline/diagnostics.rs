use fp_core::diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticManager};

use crate::diagnostics::render_core_diagnostic_with_source;

use super::PipelineOptions;

pub(crate) fn display_options(options: &PipelineOptions) -> DiagnosticDisplayOptions {
    DiagnosticDisplayOptions::new(options.debug.verbose)
}

pub(crate) fn emit(
    diagnostics: &[Diagnostic],
    stage_context: Option<&str>,
    options: &PipelineOptions,
) {
    if diagnostics.is_empty() {
        return;
    }
    let opts = display_options(options);
    let mut fallback = Vec::new();
    for diag in diagnostics {
        if !render_core_diagnostic_with_source(diag) {
            fallback.push(diag.clone());
        }
    }
    if !fallback.is_empty() {
        DiagnosticManager::emit(&fallback, stage_context, &opts);
    }
}
