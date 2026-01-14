use fp_core::diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticManager};
use std::error::Error;
use std::fmt;

#[derive(Clone)]
pub struct PipelineDiagnostics {
    pub items: Vec<Diagnostic>,
    display_options: DiagnosticDisplayOptions,
}

impl PipelineDiagnostics {
    pub fn new(display_options: DiagnosticDisplayOptions) -> Self {
        Self {
            items: Vec::new(),
            display_options,
        }
    }

    pub fn set_display_options(&mut self, display_options: DiagnosticDisplayOptions) {
        self.display_options = display_options;
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.items.push(diagnostic);
    }

    pub fn emit_stage(&mut self, stage: &'static str) {
        if self.items.is_empty() {
            return;
        }
        DiagnosticManager::emit(&self.items, Some(stage), &self.display_options);
        self.items.clear();
    }

    pub fn extend(&mut self, diagnostics: Vec<Diagnostic>) {
        if diagnostics.is_empty() {
            return;
        }
        self.items.extend(diagnostics);
    }
}

impl Default for PipelineDiagnostics {
    fn default() -> Self {
        Self::new(DiagnosticDisplayOptions::default())
    }
}

#[derive(Debug)]
pub struct PipelineError {
    pub stage: &'static str,
    pub message: String,
}

impl PipelineError {
    pub fn new(stage: &'static str, message: impl Into<String>) -> Self {
        Self {
            stage,
            message: message.into(),
        }
    }
}

impl fmt::Display for PipelineError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.stage, self.message)
    }
}

impl Error for PipelineError {}
