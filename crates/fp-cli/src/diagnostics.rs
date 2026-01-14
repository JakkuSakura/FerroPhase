//! Diagnostic and error reporting utilities

#![allow(unused_assignments)] // miette derive generates unused assignments in macro-expanded code.

use crate::{CliError, Result};
use fp_core::diagnostics::Diagnostic as CoreDiagnostic;
use fp_core::error::Error as CoreError;
use fp_core::source_map::{LineSpan, SourceFile};
use miette::Diagnostic;
use termwiz::caps::Capabilities;
use termwiz::cell::{AttributeChange, CellAttributes};
use termwiz::color::{AnsiColor, ColorAttribute};
use termwiz::surface::Change;
use termwiz::terminal::{new_terminal, Terminal};
use thiserror::Error;

type SourceSpan = miette::SourceSpan;

type OptionalSourceSpan = Option<miette::SourceSpan>;

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
    eprintln!("{:?}", error);
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

fn render_core_diagnostic(diag: &CoreDiagnostic) -> bool {
    let Some(span) = diag.span else {
        return false;
    };
    let Some(file) = fp_core::source_map::source_map().file(span.file) else {
        return false;
    };
    let Some(line_span) = file.span_on_line(span) else {
        return false;
    };
    if let Err(err) = render_with_termwiz(diag, &file, &line_span) {
        eprintln!("failed to render diagnostic: {err}");
        return false;
    }
    true
}

fn render_with_termwiz(
    diag: &CoreDiagnostic,
    file: &SourceFile,
    line_span: &LineSpan,
) -> termwiz::Result<()> {
    let caps = Capabilities::new_from_env()?;
    let mut terminal = new_terminal(caps)?;

    let (line, col) = file.line_col(diag.span.unwrap().lo);
    let plain_lines = format_rustc_plain(
        &diag.message.to_string(),
        &file.path.display().to_string(),
        line,
        col,
        line_span,
    );
    let mut changes = Vec::new();
    changes.push(Change::Text("\r".to_string()));
    changes.push(Change::ClearToEndOfLine(ColorAttribute::Default));
    changes.push(Change::Text("\r\n".to_string()));

    if let Some(header) = plain_lines.first() {
        changes.push(Change::Attribute(AttributeChange::Foreground(
            AnsiColor::Red.into(),
        )));
        changes.push(Change::Text(header.clone()));
        changes.push(Change::AllAttributes(CellAttributes::default()));
        changes.push(Change::Text("\r\n".to_string()));
    }

    if let Some(location) = plain_lines.get(1) {
        changes.push(Change::Attribute(AttributeChange::Foreground(
            AnsiColor::Blue.into(),
        )));
        changes.push(Change::Text(location.clone()));
        changes.push(Change::AllAttributes(CellAttributes::default()));
        changes.push(Change::Text("\r\n".to_string()));
    }

    for line in plain_lines.iter().skip(2).take(2) {
        changes.push(Change::Text(line.clone()));
        changes.push(Change::Text("\r\n".to_string()));
    }

    if let Some(caret_line) = plain_lines.get(4) {
        let caret_start = caret_line.find('^').unwrap_or(caret_line.len());
        let (prefix, rest) = caret_line.split_at(caret_start);
        let carets = rest.trim_end_matches(' ');
        let suffix = &rest[carets.len()..];
        changes.push(Change::Text(prefix.to_string()));
        changes.push(Change::Attribute(AttributeChange::Foreground(
            AnsiColor::Red.into(),
        )));
        changes.push(Change::Text(carets.to_string()));
        changes.push(Change::AllAttributes(CellAttributes::default()));
        changes.push(Change::Text(suffix.to_string()));
        changes.push(Change::Text("\r\n".to_string()));
    }

    terminal.render(&changes)?;
    terminal.flush()?;
    Ok(())
}

fn format_rustc_plain(
    message: &str,
    path: &str,
    line: usize,
    col: usize,
    line_span: &LineSpan,
) -> Vec<String> {
    let header = format!("error: {message}");
    let location = format!("  --> {path}:{line}:{col}");
    let line_no = line_span.line;
    let gutter_width = line_no.to_string().len();
    let gutter = " ".repeat(gutter_width);
    let gutter_bar = format!("{gutter} |");
    let source_line = format!(" {line_no:>width$} | {}", line_span.text, width = gutter_width);
    let caret_len = (line_span.col_end.saturating_sub(line_span.col_start)).max(1);
    let caret_pad = " ".repeat(line_span.col_start.saturating_sub(1));
    let caret_line = format!("{gutter} | {caret_pad}{}", "^".repeat(caret_len));
    vec![header, location, gutter_bar, source_line, caret_line]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rustc_plain_format_has_expected_shape() {
        let line_span = LineSpan {
            line: 1,
            col_start: 14,
            col_end: 17,
            text: "macro_rules! opt_bail {".to_string(),
        };
        let lines = format_rustc_plain(
            "failed to parse items (file mode): parse error: expected symbol",
            "/tmp/error.rs",
            1,
            14,
            &line_span,
        );
        let rendered = lines.join("\n");
        let expected = "\
error: failed to parse items (file mode): parse error: expected symbol
  --> /tmp/error.rs:1:14
  |
 1 | macro_rules! opt_bail {
  |              ^^^";
        assert_eq!(rendered, expected);
    }

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
