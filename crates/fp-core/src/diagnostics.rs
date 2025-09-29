use crate::span::Span;
use once_cell::sync::Lazy;
use std::fmt::{Display, Formatter};
use std::sync::{Arc, Mutex};

/// Context provided to diagnostic template renderers while producing output lines.
pub struct DiagnosticRenderContext<'a> {
    pub context: &'a str,
    pub verbose_info: bool,
}

/// Trait for converting a diagnostic into human-readable output according to a template.
pub trait DiagnosticTemplateRenderer: Send + Sync {
    fn render(
        &self,
        diagnostic: &Diagnostic,
        ctx: &DiagnosticRenderContext<'_>,
    ) -> Option<Vec<String>>;
}

/// Built-in templates supported by the diagnostic manager.
#[derive(Clone)]
pub enum DiagnosticTemplate {
    Pretty,
    Plain,
    Custom(Arc<dyn DiagnosticTemplateRenderer>),
}

impl DiagnosticTemplate {
    fn render(
        &self,
        diagnostic: &Diagnostic,
        ctx: &DiagnosticRenderContext<'_>,
    ) -> Option<Vec<String>> {
        match self {
            DiagnosticTemplate::Pretty => render_pretty(diagnostic, ctx),
            DiagnosticTemplate::Plain => render_plain(diagnostic, ctx),
            DiagnosticTemplate::Custom(renderer) => renderer.render(diagnostic, ctx),
        }
    }
}

/// Runtime configuration for emitting diagnostics.
#[derive(Clone)]
pub struct DiagnosticDisplayOptions {
    pub template: DiagnosticTemplate,
    pub verbose_info: bool,
}

impl DiagnosticDisplayOptions {
    pub fn with_template(template: DiagnosticTemplate, verbose_info: bool) -> Self {
        Self {
            template,
            verbose_info,
        }
    }

    pub fn pretty(verbose_info: bool) -> Self {
        Self::with_template(DiagnosticTemplate::Pretty, verbose_info)
    }

    pub fn plain(verbose_info: bool) -> Self {
        Self::with_template(DiagnosticTemplate::Plain, verbose_info)
    }
}

impl Default for DiagnosticDisplayOptions {
    fn default() -> Self {
        DiagnosticDisplayOptions::pretty(false)
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

        if !self.suggestions.is_empty() {
            let hints = self.suggestions.join("; ");
            write!(f, " (hints: {})", hints)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct DiagnosticReport<T, M = String>
where
    M: Clone + Display,
{
    pub value: Option<T>,
    pub diagnostics: Vec<Diagnostic<M>>,
}

impl<T, M> DiagnosticReport<T, M>
where
    M: Clone + Display,
{
    pub fn success(value: T) -> Self {
        Self {
            value: Some(value),
            diagnostics: Vec::new(),
        }
    }

    pub fn success_with_diagnostics(value: T, diagnostics: Vec<Diagnostic<M>>) -> Self {
        Self {
            value: Some(value),
            diagnostics,
        }
    }

    pub fn failure(diagnostics: Vec<Diagnostic<M>>) -> Self {
        Self {
            value: None,
            diagnostics,
        }
    }

    pub fn into_result(self) -> Result<(T, Vec<Diagnostic<M>>), Vec<Diagnostic<M>>> {
        match self.value {
            Some(value) => Ok((value, self.diagnostics)),
            None => Err(self.diagnostics),
        }
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
            let context = diagnostic
                .source_context
                .as_deref()
                .or(fallback_context)
                .unwrap_or("pipeline");

            let render_ctx = DiagnosticRenderContext {
                context,
                verbose_info: options.verbose_info,
            };

            let printable = diagnostic.as_string_diagnostic();

            if let Some(lines) = options.template.render(&printable, &render_ctx) {
                for line in lines {
                    eprintln!("{}", line);
                }
            }
        }
    }
}

static GLOBAL_DIAGNOSTIC_MANAGER: Lazy<Arc<DiagnosticManager>> =
    Lazy::new(|| Arc::new(DiagnosticManager::new()));

pub fn diagnostic_manager() -> Arc<DiagnosticManager> {
    GLOBAL_DIAGNOSTIC_MANAGER.clone()
}

pub fn report_error(message: impl Into<String>) -> crate::error::Error {
    let diagnostic = Diagnostic::error(message.into());
    println!("[diagnostic] {}", diagnostic.message);
    diagnostic_manager().error(diagnostic.clone());
    crate::error::Error::diagnostic(diagnostic)
}

fn render_pretty<M>(
    diagnostic: &Diagnostic<M>,
    ctx: &DiagnosticRenderContext<'_>,
) -> Option<Vec<String>>
where
    M: Clone + Display,
{
    if matches!(diagnostic.level, DiagnosticLevel::Info) && !ctx.verbose_info {
        return None;
    }

    let prefix = match diagnostic.level {
        DiagnosticLevel::Error => "❌",
        DiagnosticLevel::Warning => "⚠️ ",
        DiagnosticLevel::Info => "ℹ️ ",
    };

    let header = match diagnostic.code.as_ref() {
        Some(code) => format!(
            "{} [{}] {} ({})",
            prefix, ctx.context, diagnostic.message, code
        ),
        None => format!("{} [{}] {}", prefix, ctx.context, diagnostic.message),
    };

    let mut lines = vec![header];

    if let Some(span) = &diagnostic.span {
        lines.push(format!("   at {}", span.to_string()));
    }

    for suggestion in &diagnostic.suggestions {
        lines.push(format!("   💡 {}", suggestion));
    }

    Some(lines)
}

fn render_plain<M>(
    diagnostic: &Diagnostic<M>,
    ctx: &DiagnosticRenderContext<'_>,
) -> Option<Vec<String>>
where
    M: Clone + Display,
{
    if matches!(diagnostic.level, DiagnosticLevel::Info) && !ctx.verbose_info {
        return None;
    }

    let level = match diagnostic.level {
        DiagnosticLevel::Error => "ERROR",
        DiagnosticLevel::Warning => "WARNING",
        DiagnosticLevel::Info => "INFO",
    };

    let header = match diagnostic.code.as_ref() {
        Some(code) => format!(
            "[{}] {}: {} ({})",
            ctx.context, level, diagnostic.message, code
        ),
        None => format!("[{}] {}: {}", ctx.context, level, diagnostic.message),
    };

    let mut lines = vec![header];

    if let Some(span) = &diagnostic.span {
        lines.push(format!("   at {}", span.to_string()));
    }

    for suggestion in &diagnostic.suggestions {
        lines.push(format!("   suggestion: {}", suggestion));
    }

    Some(lines)
}

#[macro_export]
macro_rules! diagnostic_error {
    ($context:expr, $($arg:tt)*) => {
        $crate::diagnostics::DiagnosticReport::failure(
            vec![$crate::diagnostics::Diagnostic::error(format!($($arg)*))
                .with_source_context($context)],
        )
    };
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
