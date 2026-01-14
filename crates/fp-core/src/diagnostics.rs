use crate::span::Span;
use once_cell::sync::Lazy;
use std::fmt::{Display, Formatter};
use std::sync::{Arc, Mutex};

/// Runtime configuration for emitting diagnostics.
#[derive(Clone)]
pub struct DiagnosticDisplayOptions {
    pub verbose_info: bool,
}

impl DiagnosticDisplayOptions {
    pub fn new(verbose_info: bool) -> Self {
        Self { verbose_info }
    }
}

impl Default for DiagnosticDisplayOptions {
    fn default() -> Self {
        DiagnosticDisplayOptions::new(false)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Info,
    Warning,
    Error,
}

#[derive(Clone)]
pub struct Diagnostic<T = String>
where
    T: Clone + Display,
{
    pub level: DiagnosticLevel,
    pub message: T,
    pub span: Option<Span>,
    pub suggestions: Vec<String>,
    pub source_context: Option<String>,
    pub code: Option<String>,
}

impl<T> Diagnostic<T>
where
    T: Clone + Display,
{
    pub fn error(message: T) -> Self {
        Self {
            level: DiagnosticLevel::Error,
            message,
            span: None,
            suggestions: Vec::new(),
            source_context: None,
            code: None,
        }
    }

    pub fn warning(message: T) -> Self {
        Self {
            level: DiagnosticLevel::Warning,
            message,
            span: None,
            suggestions: Vec::new(),
            source_context: None,
            code: None,
        }
    }

    pub fn info(message: T) -> Self {
        Self {
            level: DiagnosticLevel::Info,
            message,
            span: None,
            suggestions: Vec::new(),
            source_context: None,
            code: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestions.push(suggestion.into());
        self
    }

    pub fn with_suggestions(mut self, suggestions: Vec<String>) -> Self {
        self.suggestions.extend(suggestions);
        self
    }

    pub fn with_source_context(mut self, context: impl Into<String>) -> Self {
        self.source_context = Some(context.into());
        self
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn as_string_diagnostic(&self) -> Diagnostic<String> {
        Diagnostic {
            level: self.level,
            message: self.message.to_string(),
            span: self.span.clone(),
            suggestions: self.suggestions.clone(),
            source_context: self.source_context.clone(),
            code: self.code.clone(),
        }
    }
}

impl<T> std::fmt::Debug for Diagnostic<T>
where
    T: Clone + Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Diagnostic")
            .field("level", &self.level)
            .field("message", &self.message.to_string())
            .field("span", &self.span)
            .field("suggestions", &self.suggestions)
            .field("source_context", &self.source_context)
            .field("code", &self.code)
            .finish()
    }
}

impl<T> Display for Diagnostic<T>
where
    T: Clone + Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)?;

        if let Some(code) = &self.code {
            write!(f, " [{}]", code)?;
        }

        if let Some(span) = self.span {
            write!(f, " [span {}:{}-{}]", span.file, span.lo, span.hi)?;
        }

        if !self.suggestions.is_empty() {
            let hints = self.suggestions.join("; ");
            write!(f, " (hints: {})", hints)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct DiagnosticManager {
    diagnostics: Arc<Mutex<Vec<Diagnostic>>>,
}

impl DiagnosticManager {
    pub fn new() -> Self {
        Self {
            diagnostics: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn error(&self, diagnostic: Diagnostic) {
        self.add_diagnostic(diagnostic);
    }

    pub fn add_diagnostic(&self, diagnostic: Diagnostic) {
        if let Ok(mut diagnostics) = self.diagnostics.lock() {
            diagnostics.push(diagnostic);
        }
    }

    pub fn add_diagnostics(&self, mut new_diagnostics: Vec<Diagnostic>) {
        if let Ok(mut diagnostics) = self.diagnostics.lock() {
            diagnostics.append(&mut new_diagnostics);
        }
    }

    pub fn get_diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics
            .lock()
            .map(|d| d.clone())
            .unwrap_or_default()
    }

    pub fn snapshot(&self) -> usize {
        self.diagnostics.lock().map(|d| d.len()).unwrap_or(0)
    }

    pub fn diagnostics_since(&self, index: usize) -> Vec<Diagnostic> {
        self.diagnostics
            .lock()
            .map(|d| d[index.min(d.len())..].to_vec())
            .unwrap_or_default()
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .lock()
            .map(|d| d.iter().any(|diag| diag.level == DiagnosticLevel::Error))
            .unwrap_or(false)
    }

    pub fn clear(&self) {
        if let Ok(mut diagnostics) = self.diagnostics.lock() {
            diagnostics.clear();
        }
    }

    /// Emit diagnostics using the provided template and options. The fallback context is used
    /// when a diagnostic does not specify a source context.
    pub fn emit<M>(
        diagnostics: &[Diagnostic<M>],
        fallback_context: Option<&str>,
        options: &DiagnosticDisplayOptions,
    ) where
        M: Clone + Display,
    {
        if diagnostics.is_empty() {
            return;
        }

        for diagnostic in diagnostics {
            // Skip info diagnostics unless verbose mode is enabled
            if matches!(diagnostic.level, DiagnosticLevel::Info) && !options.verbose_info {
                continue;
            }

            let context = diagnostic.source_context.as_deref().or(fallback_context);

            let message = &diagnostic.message;

            // Format message with suggestions if present
            let mut full_message = if !diagnostic.suggestions.is_empty() {
                let suggestions = diagnostic.suggestions.join("; ");
                format!("{} (hint: {})", message, suggestions)
            } else {
                message.to_string()
            };

            if let Some(span) = diagnostic.span {
                full_message.push_str(&format!(" [span {}:{}-{}]", span.file, span.lo, span.hi));
            }

            emit_tracing(&diagnostic.level, context, &full_message);
        }
    }
}

static GLOBAL_DIAGNOSTIC_MANAGER: Lazy<Arc<DiagnosticManager>> =
    Lazy::new(|| Arc::new(DiagnosticManager::new()));

pub fn diagnostic_manager() -> Arc<DiagnosticManager> {
    GLOBAL_DIAGNOSTIC_MANAGER.clone()
}

pub fn report_error(message: impl Into<String>) -> crate::error::Error {
    report_diagnostic_impl(None, message.into(), DiagnosticLevel::Error)
}

pub fn report_error_with_context(
    context: impl Into<String>,
    message: impl Into<String>,
) -> crate::error::Error {
    report_diagnostic_impl(Some(context.into()), message.into(), DiagnosticLevel::Error)
}

pub fn report_warning(message: impl Into<String>) {
    report_diagnostic_trace(None, message.into(), DiagnosticLevel::Warning);
}

pub fn report_warning_with_context(context: impl Into<String>, message: impl Into<String>) {
    report_diagnostic_trace(
        Some(context.into()),
        message.into(),
        DiagnosticLevel::Warning,
    );
}

pub fn report_info(message: impl Into<String>) {
    report_diagnostic_trace(None, message.into(), DiagnosticLevel::Info);
}

pub fn report_info_with_context(context: impl Into<String>, message: impl Into<String>) {
    report_diagnostic_trace(Some(context.into()), message.into(), DiagnosticLevel::Info);
}

fn report_diagnostic_impl(
    context: Option<String>,
    message: String,
    level: DiagnosticLevel,
) -> crate::error::Error {
    let mut diagnostic = match level {
        DiagnosticLevel::Error => Diagnostic::error(message.clone()),
        DiagnosticLevel::Warning => Diagnostic::warning(message.clone()),
        DiagnosticLevel::Info => Diagnostic::info(message.clone()),
    };

    if let Some(ctx) = context.as_ref() {
        diagnostic = diagnostic.with_source_context(ctx.clone());
    }

    emit_tracing(&level, context.as_deref(), &message);

    diagnostic_manager().error(diagnostic.clone());
    crate::error::Error::diagnostic(diagnostic)
}

fn report_diagnostic_trace(context: Option<String>, message: String, level: DiagnosticLevel) {
    let mut diagnostic = match level {
        DiagnosticLevel::Error => Diagnostic::error(message.clone()),
        DiagnosticLevel::Warning => Diagnostic::warning(message.clone()),
        DiagnosticLevel::Info => Diagnostic::info(message.clone()),
    };

    if let Some(ctx) = context.as_ref() {
        diagnostic = diagnostic.with_source_context(ctx.clone());
    }

    emit_tracing(&level, context.as_deref(), &message);
    diagnostic_manager().add_diagnostic(diagnostic);
}

fn emit_tracing(level: &DiagnosticLevel, context: Option<&str>, message: &str) {
    let msg = if let Some(ctx) = context {
        format!("[{}] {}", ctx, message)
    } else {
        message.to_string()
    };

    match level {
        DiagnosticLevel::Error => tracing::error!("{}", msg),
        DiagnosticLevel::Warning => tracing::warn!("{}", msg),
        DiagnosticLevel::Info => tracing::info!("{}", msg),
    }
}

#[macro_export]
macro_rules! emit_error {
    ($manager:expr, $context:expr, $($arg:tt)*) => {
        $manager.add_diagnostic(
            $crate::diagnostics::Diagnostic::error(format!($($arg)*))
                .with_source_context($context.to_string())
        )
    };
}
