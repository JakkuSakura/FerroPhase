use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GoAsmTarget {
    Amd64,
    Arm64,
}

impl GoAsmTarget {
    pub fn parse(value: &str) -> Option<Self> {
        match value.trim().to_ascii_lowercase().as_str() {
            "goasm_amd64" | "goasm_x86_64" | "amd64" | "x86_64" => Some(Self::Amd64),
            "goasm_arm64" | "goasm_aarch64" | "arm64" | "aarch64" => Some(Self::Arm64),
            _ => None,
        }
    }

    pub fn resolve(target_triple: Option<&str>) -> Self {
        match target_triple.map(|triple| triple.to_ascii_lowercase()) {
            Some(triple) if triple.contains("amd64") || triple.contains("x86_64") => Self::Amd64,
            Some(triple) if triple.contains("arm64") || triple.contains("aarch64") => Self::Arm64,
            _ if cfg!(target_arch = "x86_64") => Self::Amd64,
            _ => Self::Arm64,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GoAsmConfig {
    pub output_path: PathBuf,
    pub target: Option<GoAsmTarget>,
    pub target_triple: Option<String>,
}

impl GoAsmConfig {
    pub fn new(output_path: impl Into<PathBuf>) -> Self {
        Self {
            output_path: output_path.into(),
            target: None,
            target_triple: None,
        }
    }

    pub fn with_target(mut self, target: Option<GoAsmTarget>) -> Self {
        self.target = target;
        self
    }

    pub fn with_target_triple(mut self, target_triple: Option<String>) -> Self {
        self.target_triple = target_triple;
        self
    }
}
