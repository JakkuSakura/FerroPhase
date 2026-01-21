//! Diagnostic and error reporting utilities

#![allow(unused_assignments)] // miette derive generates unused assignments in macro-expanded code.

use crate::{CliError, Result};
use fp_core::diagnostics::{Diagnostic as CoreDiagnostic, DiagnosticLevel};
use fp_core::error::Error as CoreError;
use miette::{Diagnostic, LabeledSpan, NamedSource, Report, Severity, SourceCode, SourceSpan};
use std::fmt::{Display, Formatter};
use thiserror::Error;

type OptionalSourceSpan = Option<SourceSpan>;

/// Set up enhanced error reporting with miette
pub fn setup_error_reporting() -> Result<()> {
    miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .terminal_links(true)
                .unicode(true)
                .context_lines(3)
                .build(),
        )
    }))
    .map_err(|e| crate::CliError::Config(format!("Failed to setup error reporting: {}", e)))?;

    Ok(())
}

/// Enhanced diagnostic error for FerroPhase CLI
#[derive(Error, Debug, Diagnostic)]
pub enum FerroPhaseError {
    #[error("Syntax error in FerroPhase code")]
    #[diagnostic(
        code(ferrophase::syntax_error),
        help("Check your syntax against the FerroPhase language reference")
    )]
    SyntaxError {
        src: String,
        #[label("syntax error here")]
        err_span: SourceSpan,
    },

    #[error("Type error in FerroPhase code")]
    #[diagnostic(
        code(ferrophase::type_error),
        help("Ensure all types are correctly specified and compatible")
    )]
    TypeError {
        src: String,
        #[label("type error here")]
        err_span: SourceSpan,
        message: String,
    },

    #[error("Compilation error")]
    #[diagnostic(
        code(ferrophase::compilation_error),
        help("Check the compilation logs for more details")
    )]
    CompilationError {
        message: String,
        src: Option<String>,
        #[label("error occurred here")]
        err_span: OptionalSourceSpan,
    },

    #[error("Project configuration error")]
    #[diagnostic(
        code(ferrophase::config_error),
        help("Check your configuration file or use magnet for package manifests")
    )]
    ConfigError {
        message: String,
        config_src: Option<String>,
        #[label("configuration error")]
        err_span: OptionalSourceSpan,
    },
}

pub fn syntax_error(src: String, span: SourceSpan) -> FerroPhaseError {
    FerroPhaseError::SyntaxError {
        src,
        err_span: span,
    }
}

pub fn type_error(src: String, span: SourceSpan, message: String) -> FerroPhaseError {
    FerroPhaseError::TypeError {
        src,
        err_span: span,
        message,
    }
}

/// Helper function to create a compilation error
pub fn compilation_error(message: String) -> FerroPhaseError {
    FerroPhaseError::CompilationError {
        message,
        src: None,
        err_span: None,
    }
}

/// Helper function to create a compilation error with source context
pub fn compilation_error_with_source(
    message: String,
    src: String,
    span: SourceSpan,
) -> FerroPhaseError {
    FerroPhaseError::CompilationError {
        message,
        src: Some(src),
        err_span: Some(span),
    }
}

/// Helper function to create a configuration error
pub fn config_error(message: String) -> FerroPhaseError {
    FerroPhaseError::ConfigError {
        message,
        config_src: None,
        err_span: None,
    }
}

/// Pretty print diagnostics with context
pub fn print_diagnostic(error: &dyn Diagnostic) {
    eprintln!("{error:?}");
}

pub fn render_cli_error(error: &CliError) -> bool {
    match error {
        CliError::Core(core) => render_core_error(core),
        _ => false,
    }
}

fn render_core_error(error: &CoreError) -> bool {
    match error {
        CoreError::Diagnostic(diag) => render_core_diagnostic(diag),
        CoreError::SyntaxError(span, _) => {
            let diag = CoreDiagnostic::error("syntax error".to_string()).with_span(*span);
            render_core_diagnostic(&diag)
        }
        _ => false,
    }
}

pub(crate) fn render_core_diagnostic(diag: &CoreDiagnostic) -> bool {
    let report = core_diagnostic_to_report(diag);
    eprintln!("{:?}", report);
    true
}

fn core_diagnostic_to_report(diag: &CoreDiagnostic) -> Report {
    let (source, span) = core_diagnostic_source(diag);
    let help = diag.suggestions.join("; ").trim().to_string();
    let message = if let Some(context) = diag.source_context.as_deref() {
        format!("[{}] {}", context, diag.message)
    } else {
        diag.message.to_string()
    };

    let error = CoreMietteDiagnostic {
        message,
        code: diag.code.clone(),
        severity: map_severity(diag.level),
        source,
        span,
        help: if help.is_empty() { None } else { Some(help) },
    };

    Report::new(error)
}

fn core_diagnostic_source(diag: &CoreDiagnostic) -> (Option<NamedSource<String>>, Option<SourceSpan>) {
    let Some(span) = diag.span else {
        return (None, None);
    };
    let Some(file) = fp_core::source_map::source_map().file(span.file) else {
        return (None, None);
    };
    let len = span.hi.saturating_sub(span.lo).max(1);
    let source_span = SourceSpan::new((span.lo as usize).into(), len as usize);
    let source = NamedSource::new(file.path.display().to_string(), file.source.to_string());
    (Some(source), Some(source_span))
}

fn map_severity(level: DiagnosticLevel) -> Severity {
    match level {
        DiagnosticLevel::Error => Severity::Error,
        DiagnosticLevel::Warning => Severity::Warning,
        DiagnosticLevel::Info => Severity::Advice,
    }
}

#[derive(Debug)]
struct CoreMietteDiagnostic {
    message: String,
    code: Option<String>,
    severity: Severity,
    source: Option<NamedSource<String>>,
    span: Option<SourceSpan>,
    help: Option<String>,
}

impl Display for CoreMietteDiagnostic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for CoreMietteDiagnostic {}

impl Diagnostic for CoreMietteDiagnostic {
    fn code<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.code.as_ref().map(|code| Box::new(code.clone()) as Box<dyn Display>)
    }

    fn severity(&self) -> Option<Severity> {
        Some(self.severity)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn Display + 'a>> {
        self.help
            .as_ref()
            .map(|help| Box::new(help.clone()) as Box<dyn Display>)
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        self.source
            .as_ref()
            .map(|source| source as &dyn SourceCode)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        let span = self.span?;
        let label = LabeledSpan::new_with_span(Some("here".to_string()), span);
        Some(Box::new(std::iter::once(label)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_syntax_error_creation() {
        let src = "fn main() { let x = ".to_string();
        let span = miette::SourceSpan::new(16.into(), 1);
        let error = syntax_error(src.clone(), span);

        match error {
            FerroPhaseError::SyntaxError {
                src: error_src,
                err_span,
            } => {
                assert_eq!(error_src, src);
                assert_eq!(err_span, span);
            }
            _ => panic!("Expected SyntaxError"),
        }
    }

    #[test]
    fn test_type_error_creation() {
        let src = "let x: i32 = \"hello\";".to_string();
        let span = miette::SourceSpan::new(13.into(), 7);
        let message = "Cannot assign string to integer variable".to_string();
        let error = type_error(src.clone(), span, message.clone());

        match error {
            FerroPhaseError::TypeError {
                src: error_src,
                err_span,
                message: error_msg,
            } => {
                assert_eq!(error_src, src);
                assert_eq!(err_span, span);
                assert_eq!(error_msg, message);
            }
            _ => panic!("Expected TypeError"),
        }
    }
}
