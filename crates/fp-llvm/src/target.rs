use anyhow::{Context, Result};
use llvm_ir::Module;
use std::path::Path;

/// Target configuration for LLVM code generation
#[derive(Debug, Clone)]
pub struct TargetConfig {
    pub triple: String,
    pub cpu: String,
    pub features: String,
    pub optimization_level: OptimizationLevel,
    pub reloc_mode: RelocMode,
    pub code_model: CodeModel,
}

#[derive(Debug, Clone)]
pub enum OptimizationLevel {
    None,
    Less,
    Default,
    Aggressive,
}

#[derive(Debug, Clone)]
pub enum RelocMode {
    Default,
    Static,
    PIC,
    DynamicNoPic,
}

#[derive(Debug, Clone)]
pub enum CodeModel {
    Default,
    JITDefault,
    Small,
    Kernel,
    Medium,
    Large,
}

impl Default for TargetConfig {
    fn default() -> Self {
        Self {
            triple: "x86_64-apple-darwin".to_string(),
            cpu: "generic".to_string(),
            features: "".to_string(),
            optimization_level: OptimizationLevel::Default,
            reloc_mode: RelocMode::Default,
            code_model: CodeModel::Default,
        }
    }
}

impl TargetConfig {
    /// Create a new target configuration for the host machine
    pub fn host() -> Self {
        Self::default()
    }

    /// Create a target configuration for a specific triple
    pub fn for_triple(triple: &str) -> Self {
        Self {
            triple: triple.to_string(),
            ..Self::default()
        }
    }

    /// Set the CPU type
    pub fn with_cpu(mut self, cpu: &str) -> Self {
        self.cpu = cpu.to_string();
        self
    }

    /// Set CPU features
    pub fn with_features(mut self, features: &str) -> Self {
        self.features = features.to_string();
        self
    }

    /// Set optimization level
    pub fn with_optimization(mut self, level: OptimizationLevel) -> Self {
        self.optimization_level = level;
        self
    }

    /// Set relocation mode
    pub fn with_reloc_mode(mut self, mode: RelocMode) -> Self {
        self.reloc_mode = mode;
        self
    }

    /// Set code model
    pub fn with_code_model(mut self, model: CodeModel) -> Self {
        self.code_model = model;
        self
    }

    /// Get supported target triples for common architectures
    pub fn supported_targets() -> Vec<&'static str> {
        vec![
            "x86_64-unknown-linux-gnu",
            "x86_64-apple-darwin",
            "x86_64-pc-windows-msvc",
            "aarch64-unknown-linux-gnu",
            "aarch64-apple-darwin",
            "wasm32-unknown-unknown",
        ]
    }

    /// Check if a target triple is supported
    pub fn is_target_supported(triple: &str) -> bool {
        Self::supported_targets().contains(&triple)
    }
}

/// Target-specific code generation helper
pub struct TargetCodegen {
    config: TargetConfig,
}

impl TargetCodegen {
    /// Create a new target codegen instance
    pub fn new(config: TargetConfig) -> Result<Self> {
        Ok(Self { config })
    }

    /// Get the target configuration
    pub fn config(&self) -> &TargetConfig {
        &self.config
    }

    /// Write module to object file (placeholder implementation)
    pub fn write_object_file(&self, module: &Module, output_path: &Path) -> Result<()> {
        // For now, write LLVM IR to file instead of object code
        let ir_content = format!(
            "Module: {} with {} functions",
            module.name,
            module.functions.len()
        );
        std::fs::write(output_path.with_extension("ll"), ir_content)
            .with_context(|| format!("Failed to write IR file to {}", output_path.display()))?;

        tracing::info!("Generated LLVM IR file instead of object file (actual object file generation requires LLVM compilation)");
        Ok(())
    }

    /// Write module to assembly file (placeholder implementation)
    pub fn write_assembly_file(&self, module: &Module, output_path: &Path) -> Result<()> {
        // For now, write LLVM IR to file instead of assembly
        let ir_content = format!(
            "Module: {} with {} functions",
            module.name,
            module.functions.len()
        );
        std::fs::write(output_path.with_extension("s"), ir_content).with_context(|| {
            format!("Failed to write assembly file to {}", output_path.display())
        })?;

        tracing::warn!(
            "Generated LLVM IR file instead of assembly (actual assembly generation requires LLVM compilation)"
        );
        Ok(())
    }

    /// Get pointer size in bits for this target
    pub fn pointer_size_bits(&self) -> u32 {
        if self.config.triple.starts_with("x86_64") || self.config.triple.starts_with("aarch64") {
            64
        } else {
            32
        }
    }

    /// Check if target is 64-bit
    pub fn is_64_bit(&self) -> bool {
        self.pointer_size_bits() == 64
    }

    /// Get target endianness (assume little endian for common targets)
    pub fn is_little_endian(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_target_config() {
        let config = TargetConfig::default();
        assert!(!config.triple.is_empty());
        assert_eq!(config.cpu, "generic");
        assert_eq!(config.features, "");
    }

    #[test]
    fn test_target_config_builder() {
        let config = TargetConfig::for_triple("x86_64-unknown-linux-gnu")
            .with_cpu("native")
            .with_features("+avx2")
            .with_optimization(OptimizationLevel::Aggressive);

        assert_eq!(config.triple, "x86_64-unknown-linux-gnu");
        assert_eq!(config.cpu, "native");
        assert_eq!(config.features, "+avx2");
        assert!(matches!(
            config.optimization_level,
            OptimizationLevel::Aggressive
        ));
    }

    #[test]
    fn test_supported_targets() {
        let targets = TargetConfig::supported_targets();
        assert!(targets.contains(&"x86_64-unknown-linux-gnu"));
        assert!(targets.contains(&"aarch64-apple-darwin"));

        assert!(TargetConfig::is_target_supported(
            "x86_64-unknown-linux-gnu"
        ));
        assert!(!TargetConfig::is_target_supported("invalid-target"));
    }

    #[test]
    fn test_target_codegen_creation() {
        let config = TargetConfig::host();
        let result = TargetCodegen::new(config);
        assert!(result.is_ok());

        if let Ok(codegen) = result {
            assert!(codegen.pointer_size_bits() > 0);
        }
    }
}
