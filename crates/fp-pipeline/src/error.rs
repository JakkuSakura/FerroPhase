use crate::config::PipelineOptions;
use fp_core::diagnostics::{Diagnostic, DiagnosticDisplayOptions, DiagnosticManager};
use std::error::Error;
use std::fmt;

#[derive(Debug, Default, Clone)]
pub struct PipelineDiagnostics {
    pub items: Vec<Diagnostic>,
}

impl PipelineDiagnostics {
    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.items.push(diagnostic);
    }

    pub fn emit_stage(&mut self, stage: &'static str, options: &PipelineOptions) {
        if self.items.is_empty() {
            return;
        }
        let opts = DiagnosticDisplayOptions::new(options.debug.verbose);
        DiagnosticManager::emit(&self.items, Some(stage), &opts);
        self.items.clear();
    }

    pub fn extend(&mut self, diagnostics: Vec<Diagnostic>) {
        if diagnostics.is_empty() {
            return;
        }
        self.items.extend(diagnostics);
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
