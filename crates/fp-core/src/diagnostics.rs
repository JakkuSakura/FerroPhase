use crate::span::Span;
use once_cell::sync::Lazy;
use std::fmt::{Display, Formatter};
use std::sync::atomic::{AtomicBool, Ordering};
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
            if let Some(snippet) = span.snippet() {
                write!(f, " ({snippet:?})")?;
            }
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
}

/// Identity equality on the shared `Arc`, not a deep comparison of
/// accumulated diagnostics — this only exists so `HirPackage` (which holds
/// one of these) can keep deriving `PartialEq` for its own, unrelated
/// structural fields; `Diagnostic` itself isn't `PartialEq`, and diagnostics
/// are mutated through shared handles precisely so every clone sees the
/// same underlying log, so "same log" is the only equality that makes sense
/// here.
impl PartialEq for DiagnosticManager {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.diagnostics, &other.diagnostics)
    }
}

impl Default for DiagnosticManager {
    fn default() -> Self {
        Self::new()
    }
}

impl DiagnosticManager {
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

    pub fn truncate(&self, len: usize) {
        if let Ok(mut diagnostics) = self.diagnostics.lock() {
            diagnostics.truncate(len);
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

        let renderer = GLOBAL_DIAGNOSTIC_RENDERER
            .lock()
            .ok()
            .and_then(|guard| *guard);

        for diagnostic in diagnostics {
            // Skip info diagnostics unless verbose mode is enabled
            if matches!(diagnostic.level, DiagnosticLevel::Info) && !options.verbose_info {
                continue;
            }

            let context = diagnostic.source_context.as_deref().or(fallback_context);

            // Format message with suggestions if present
            let mut full_message = diagnostic.message.to_string();
            if !diagnostic.suggestions.is_empty() {
                let suggestions = diagnostic.suggestions.join("; ");
                full_message = format!("{} (hint: {})", full_message, suggestions);
            }

            if let Some(render) = renderer {
                let mut render_diag = diagnostic.as_string_diagnostic();
                render_diag.message = full_message.clone();
                render_diag.suggestions.clear();
                if render(&render_diag) {
                    continue;
                }
            }

            if let Some(span) = diagnostic.span {
                full_message.push_str(&format!(" [span {}:{}-{}]", span.file, span.lo, span.hi));
                if let Some(snippet) = span.snippet() {
                    full_message.push_str(&format!(" ({snippet:?})"));
                }
            }

            Self::emit_tracing(&diagnostic.level, context, &full_message);
        }
    }
}

type DiagnosticRenderer = fn(&Diagnostic<String>) -> bool;

static GLOBAL_DIAGNOSTIC_RENDERER: Lazy<Mutex<Option<DiagnosticRenderer>>> =
    Lazy::new(|| Mutex::new(None));
static EMIT_TRACING: Lazy<AtomicBool> = Lazy::new(|| AtomicBool::new(true));

pub fn set_diagnostic_renderer(renderer: DiagnosticRenderer) {
    if let Ok(mut guard) = GLOBAL_DIAGNOSTIC_RENDERER.lock() {
        *guard = Some(renderer);
    }
}

pub fn set_diagnostics_tracing(enabled: bool) {
    EMIT_TRACING.store(enabled, Ordering::Relaxed);
}

impl DiagnosticManager {
    pub fn report_error(message: impl Into<String>) -> crate::error::Error {
        let message = message.into();
        Self::emit_tracing(&DiagnosticLevel::Error, None, &message);
        crate::error::Error::diagnostic(Diagnostic::error(message))
    }

    pub fn report_error_with_context(
        context: impl Into<String>,
        message: impl Into<String>,
    ) -> crate::error::Error {
        let context = context.into();
        let message = message.into();
        Self::emit_tracing(&DiagnosticLevel::Error, Some(&context), &message);
        crate::error::Error::diagnostic(Diagnostic::error(message).with_source_context(context))
    }

    pub fn report_warning(message: impl Into<String>) {
        let message = message.into();
        Self::emit_tracing(&DiagnosticLevel::Warning, None, &message);
    }

    pub fn report_warning_with_context(context: impl Into<String>, message: impl Into<String>) {
        let context = context.into();
        let message = message.into();
        Self::emit_tracing(&DiagnosticLevel::Warning, Some(&context), &message);
    }

    pub fn report_info(message: impl Into<String>) {
        let message = message.into();
        Self::emit_tracing(&DiagnosticLevel::Info, None, &message);
    }

    pub fn report_info_with_context(context: impl Into<String>, message: impl Into<String>) {
        let context = context.into();
        let message = message.into();
        Self::emit_tracing(&DiagnosticLevel::Info, Some(&context), &message);
    }

    fn emit_tracing(level: &DiagnosticLevel, context: Option<&str>, message: &str) {
        if !EMIT_TRACING.load(Ordering::Relaxed) {
            return;
        }
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
