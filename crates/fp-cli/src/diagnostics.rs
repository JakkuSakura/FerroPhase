//! Diagnostic and error reporting utilities.

use crate::CliError;
use fp_core::diagnostics::Diagnostic as CoreDiagnostic;
use fp_core::error::Error as CoreError;

/// Configure plain diagnostic output.
pub fn setup_error_reporting() -> crate::Result<()> {
    fp_core::diagnostics::set_diagnostics_tracing(false);
    fp_core::diagnostics::set_diagnostic_renderer(render_core_diagnostic_for_core);
    Ok(())
}

/// Render a CLI error using plain text output.
pub fn render_cli_error(error: &CliError) -> bool {
    match error {
        CliError::Core(core) => render_core_error(core),
        _ => false,
    }
}

fn render_core_error(error: &CoreError) -> bool {
    match error {
        CoreError::Diagnostic(diag) => render_core_diagnostic(diag),
        CoreError::SyntaxError(_, message) => {
            eprintln!("syntax error: {message:?}");
            true
        }
        _ => false,
    }
}

pub(crate) fn render_core_diagnostic(diag: &CoreDiagnostic) -> bool {
    eprintln!("{diag:?}");
    true
}

fn render_core_diagnostic_for_core(diag: &fp_core::diagnostics::Diagnostic<String>) -> bool {
    eprintln!("{diag:?}");
    true
}
