use fp_core::diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticManager};

use crate::config::PipelineOptions;

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
    DiagnosticManager::emit(diagnostics, stage_context, &opts);
}
