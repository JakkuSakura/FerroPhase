use fp_core::span::Span;

/// Builds a typing-stage diagnostic through the shared
/// `fp_core::diagnostics::Diagnostic` type directly — no separate
/// `TypingDiagnostic` type; typing diagnostics use the same common
/// diagnostics manager every other pipeline stage does.
pub fn typing_diagnostic(
    message: impl Into<String>,
    span: Option<Span>,
) -> fp_core::diagnostics::Diagnostic<String> {
    let mut diagnostic = fp_core::diagnostics::Diagnostic::error(message.into())
        .with_source_context("typing".to_string());
    if let Some(span) = span {
        diagnostic = diagnostic.with_span(span);
    }
    diagnostic
}
