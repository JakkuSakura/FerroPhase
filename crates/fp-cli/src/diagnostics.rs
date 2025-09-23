//! Diagnostic and error reporting utilities

use crate::Result;
use miette::Diagnostic;
use thiserror::Error;

/// Set up enhanced error reporting with miette
pub fn setup_error_reporting() -> Result<()> {
    // Install miette as the global error handler
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
        #[source_code]
        src: String,
        #[label("syntax error here")]
        err_span: miette::SourceSpan,
    },

    #[error("Type error in FerroPhase code")]
    #[diagnostic(
        code(ferrophase::type_error),
        help("Ensure all types are correctly specified and compatible")
    )]
    TypeError {
        #[source_code]
        src: String,
        #[label("type error here")]
        err_span: miette::SourceSpan,
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
        err_span: Option<miette::SourceSpan>,
    },

    #[error("Project configuration error")]
    #[diagnostic(
        code(ferrophase::config_error),
        help("Check your Ferrophase.toml file for correct syntax and values")
    )]
    ConfigError {
        message: String,
        config_src: Option<String>,
        #[label("configuration error")]
        err_span: Option<miette::SourceSpan>,
    },
}

/// Helper function to create a syntax error with source context
pub fn syntax_error(src: String, span: miette::SourceSpan) -> FerroPhaseError {
    FerroPhaseError::SyntaxError {
        src,
        err_span: span,
    }
}

/// Helper function to create a type error with source context
pub fn type_error(src: String, span: miette::SourceSpan, message: String) -> FerroPhaseError {
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
    span: miette::SourceSpan,
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
    eprintln!("{:?}", error);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_syntax_error_creation() {
        let src = "fn main() { let x = ".to_string();
        let span = miette::SourceSpan::new(16.into(), 1.into());
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
        let span = miette::SourceSpan::new(13.into(), 7.into());
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
