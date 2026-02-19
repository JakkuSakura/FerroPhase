use crate::diagnostics::render_core_diagnostic;
use fp_core::diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticLevel};

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
    let verbose_info = options.debug.verbose;
    for diag in diagnostics {
        if matches!(diag.level, DiagnosticLevel::Info) && !verbose_info {
            continue;
        }
        let diag = if diag.source_context.is_none() {
            if let Some(context) = stage_context {
                diag.clone().with_source_context(context.to_string())
            } else {
                diag.clone()
            }
        } else {
            diag.clone()
        };
        render_core_diagnostic(&diag);
    }
}
