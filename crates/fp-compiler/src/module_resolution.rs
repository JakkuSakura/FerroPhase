use std::path::Path;

use fp_core::module::resolution::ModuleResolutionContext;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleResolutionError {
    message: String,
}

impl ModuleResolutionError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for ModuleResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for ModuleResolutionError {}

impl From<String> for ModuleResolutionError {
    fn from(message: String) -> Self {
        Self { message }
    }
}

impl From<&str> for ModuleResolutionError {
    fn from(message: &str) -> Self {
        Self::new(message)
    }
}

pub trait CompilerModuleResolver: Send + Sync {
    fn resolve_context(&self, input: &Path) -> Result<ModuleResolutionContext, ModuleResolutionError>;
}
