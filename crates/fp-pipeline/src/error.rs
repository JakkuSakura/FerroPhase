use std::error::Error;
use std::fmt;

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
