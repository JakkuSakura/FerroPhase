//! Diagnostic and error reporting utilities

#![allow(unused_assignments)] // miette derive generates unused assignments in macro-expanded code.

use crate::Result;
#[cfg(not(feature = "bootstrap"))]
use miette::Diagnostic;
#[cfg(not(feature = "bootstrap"))]
use thiserror::Error;

#[cfg(feature = "bootstrap")]
type SourceSpan = (usize, usize);
#[cfg(not(feature = "bootstrap"))]
type SourceSpan = miette::SourceSpan;

#[cfg(feature = "bootstrap")]
type OptionalSourceSpan = Option<(usize, usize)>;
#[cfg(not(feature = "bootstrap"))]
type OptionalSourceSpan = Option<miette::SourceSpan>;

/// Set up enhanced error reporting with miette
#[cfg(not(feature = "bootstrap"))]
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

#[cfg(feature = "bootstrap")]
pub fn setup_error_reporting() -> Result<()> {
    Ok(())
}

/// Enhanced diagnostic error for FerroPhase CLI
#[cfg_attr(feature = "bootstrap", derive(Debug))]
#[cfg_attr(not(feature = "bootstrap"), derive(Error, Debug, Diagnostic))]
pub enum FerroPhaseError {
    #[cfg_attr(not(feature = "bootstrap"), error("Syntax error in FerroPhase code"))]
    #[cfg_attr(
        not(feature = "bootstrap"),
        diagnostic(
            code(ferrophase::syntax_error),
            help("Check your syntax against the FerroPhase language reference")
        )
    )]
    SyntaxError {
        #[cfg_attr(not(feature = "bootstrap"), source_code)]
        src: String,
        #[cfg_attr(not(feature = "bootstrap"), label("syntax error here"))]
        err_span: SourceSpan,
    },

    #[cfg_attr(not(feature = "bootstrap"), error("Type error in FerroPhase code"))]
    #[cfg_attr(
        not(feature = "bootstrap"),
        diagnostic(
            code(ferrophase::type_error),
            help("Ensure all types are correctly specified and compatible")
        )
    )]
    TypeError {
        #[cfg_attr(not(feature = "bootstrap"), source_code)]
        src: String,
        #[cfg_attr(not(feature = "bootstrap"), label("type error here"))]
        err_span: SourceSpan,
        message: String,
    },

    #[cfg_attr(not(feature = "bootstrap"), error("Compilation error"))]
    #[cfg_attr(
        not(feature = "bootstrap"),
        diagnostic(
            code(ferrophase::compilation_error),
            help("Check the compilation logs for more details")
        )
    )]
    CompilationError {
        message: String,
        src: Option<String>,
        #[cfg_attr(not(feature = "bootstrap"), label("error occurred here"))]
        err_span: OptionalSourceSpan,
    },

    #[cfg_attr(not(feature = "bootstrap"), error("Project configuration error"))]
    #[cfg_attr(
        not(feature = "bootstrap"),
        diagnostic(
            code(ferrophase::config_error),
            help("Check your configuration file or use magnet for package manifests")
        )
    )]
    ConfigError {
        message: String,
        config_src: Option<String>,
        #[cfg_attr(not(feature = "bootstrap"), label("configuration error"))]
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
#[cfg(not(feature = "bootstrap"))]
pub fn print_diagnostic(error: &dyn Diagnostic) {
    eprintln!("{:?}", error);
}

#[cfg(feature = "bootstrap")]
pub fn print_diagnostic(error: &FerroPhaseError) {
    eprintln!("{:?}", error);
}

#[cfg(all(test, not(feature = "bootstrap")))]
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
