#[derive(Clone, Copy)]
pub enum TypingDiagnosticLevel {
    Error,
    Warning,
}

pub struct TypingDiagnostic {
    pub level: TypingDiagnosticLevel,
    pub message: String,
}

impl TypingDiagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self { level: TypingDiagnosticLevel::Error, message: message.into() }
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self { level: TypingDiagnosticLevel::Warning, message: message.into() }
    }
}

pub struct TypingOutcome {
    pub diagnostics: Vec<TypingDiagnostic>,
    pub has_errors: bool,
}

