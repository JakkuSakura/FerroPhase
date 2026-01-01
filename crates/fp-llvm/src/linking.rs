use anyhow::{anyhow, bail, Context, Result};
use inkwell::module::Module;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Linker configuration for generating executables and libraries
#[derive(Debug, Clone)]
pub struct LinkerConfig {
    pub output_path: PathBuf,
    pub output_type: OutputType,
    pub link_libraries: Vec<String>,
    pub library_paths: Vec<PathBuf>,
    pub framework_paths: Vec<PathBuf>,
    pub frameworks: Vec<String>,
    pub static_libraries: Vec<PathBuf>,
    pub dynamic_libraries: Vec<PathBuf>,
    pub linker_args: Vec<String>,
    pub strip_symbols: bool,
    pub optimize_for_size: bool,
}

/// Type of output to generate
#[derive(Debug, Clone, PartialEq)]
pub enum OutputType {
    Executable,
    StaticLibrary,
    DynamicLibrary,
}

impl Default for LinkerConfig {
    fn default() -> Self {
        Self {
            output_path: PathBuf::from("output"),
            output_type: OutputType::Executable,
            link_libraries: Vec::new(),
            library_paths: Vec::new(),
            framework_paths: Vec::new(),
            frameworks: Vec::new(),
            static_libraries: Vec::new(),
            dynamic_libraries: Vec::new(),
            linker_args: Vec::new(),
            strip_symbols: false,
            optimize_for_size: false,
        }
    }
}

impl LinkerConfig {
    /// Create a new linker configuration for an executable
    pub fn executable(output_path: impl Into<PathBuf>) -> Self {
        Self {
            output_path: output_path.into(),
            output_type: OutputType::Executable,
            ..Self::default()
        }
    }

    /// Create a new linker configuration for a static library
    pub fn static_library(output_path: impl Into<PathBuf>) -> Self {
        Self {
            output_path: output_path.into(),
            output_type: OutputType::StaticLibrary,
            ..Self::default()
        }
    }

    /// Create a new linker configuration for a dynamic library
    pub fn dynamic_library(output_path: impl Into<PathBuf>) -> Self {
        Self {
            output_path: output_path.into(),
            output_type: OutputType::DynamicLibrary,
            ..Self::default()
        }
    }

    /// Add a library to link
    pub fn with_library(mut self, lib: impl Into<String>) -> Self {
        self.link_libraries.push(lib.into());
        self
    }

    /// Add a library search path
    pub fn with_library_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.library_paths.push(path.into());
        self
    }

    /// Add a framework (macOS specific)
    pub fn with_framework(mut self, framework: impl Into<String>) -> Self {
        self.frameworks.push(framework.into());
        self
    }

    /// Add a framework search path (macOS specific)
    pub fn with_framework_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.framework_paths.push(path.into());
        self
    }

    /// Add a static library file
    pub fn with_static_library(mut self, path: impl Into<PathBuf>) -> Self {
        self.static_libraries.push(path.into());
        self
    }

    /// Add a dynamic library file
    pub fn with_dynamic_library(mut self, path: impl Into<PathBuf>) -> Self {
        self.dynamic_libraries.push(path.into());
        self
    }

    /// Add custom linker arguments
    pub fn with_linker_args(mut self, args: Vec<String>) -> Self {
        self.linker_args.extend(args);
        self
    }

    /// Enable symbol stripping
    pub fn with_stripped_symbols(mut self) -> Self {
        self.strip_symbols = true;
        self
    }

    /// Optimize for size
    pub fn with_size_optimization(mut self) -> Self {
        self.optimize_for_size = true;
        self
    }
}

/// LLVM-based linker that generates executables and libraries
pub struct LlvmLinker {
    config: LinkerConfig,
}

impl LlvmLinker {
    /// Create a new LLVM linker with the given configuration
    pub fn new(config: LinkerConfig) -> Self {
        Self { config }
    }

    /// Link object files into the final output
    pub fn link(&self, object_files: &[&Path]) -> Result<()> {
        if object_files.is_empty() {
            bail!("No object files provided for linking");
        }

        match self.config.output_type {
            OutputType::Executable => self.link_executable(object_files),
            OutputType::StaticLibrary => self.link_static_library(object_files),
            OutputType::DynamicLibrary => self.link_dynamic_library(object_files),
        }
    }

    /// Link an executable
    fn link_executable(&self, object_files: &[&Path]) -> Result<()> {
        let mut cmd = self.get_linker_command()?;

        // Add output path
        cmd.arg("-o").arg(&self.config.output_path);

        // Add object files
        for obj_file in object_files {
            cmd.arg(obj_file);
        }

        // Add library paths
        for lib_path in &self.config.library_paths {
            cmd.arg(format!("-L{}", lib_path.display()));
        }

        // Add libraries
        for lib in &self.config.link_libraries {
            cmd.arg(format!("-l{}", lib));
        }

        // Add static libraries
        for static_lib in &self.config.static_libraries {
            cmd.arg(static_lib);
        }

        // Add dynamic libraries
        for dynamic_lib in &self.config.dynamic_libraries {
            cmd.arg(dynamic_lib);
        }

        // Add framework paths (macOS)
        for framework_path in &self.config.framework_paths {
            cmd.arg(format!("-F{}", framework_path.display()));
        }

        // Add frameworks (macOS)
        for framework in &self.config.frameworks {
            cmd.arg("-framework").arg(framework);
        }

        // Add optimization flags
        if self.config.optimize_for_size {
            cmd.arg("-Os");
        }

        // Add symbol stripping
        if self.config.strip_symbols {
            cmd.arg("-s");
        }

        // Add custom linker arguments
        for arg in &self.config.linker_args {
            cmd.arg(arg);
        }

        self.execute_command(cmd, "linking executable")
    }

    /// Link a static library
    fn link_static_library(&self, object_files: &[&Path]) -> Result<()> {
        let mut cmd = Command::new("ar");
        cmd.arg("rcs").arg(&self.config.output_path);

        // Add object files
        for obj_file in object_files {
            cmd.arg(obj_file);
        }

        self.execute_command(cmd, "creating static library")
    }

    /// Link a dynamic library
    fn link_dynamic_library(&self, object_files: &[&Path]) -> Result<()> {
        let mut cmd = self.get_linker_command()?;

        // Add shared library flag
        cmd.arg("-shared");

        // Add output path
        cmd.arg("-o").arg(&self.config.output_path);

        // Add object files
        for obj_file in object_files {
            cmd.arg(obj_file);
        }

        // Add library paths
        for lib_path in &self.config.library_paths {
            cmd.arg(format!("-L{}", lib_path.display()));
        }

        // Add libraries
        for lib in &self.config.link_libraries {
            cmd.arg(format!("-l{}", lib));
        }

        // Add custom linker arguments
        for arg in &self.config.linker_args {
            cmd.arg(arg);
        }

        self.execute_command(cmd, "creating dynamic library")
    }

    /// Execute a command and check for errors
    fn execute_command(&self, mut cmd: Command, operation: &str) -> Result<()> {
        let output = cmd.output().context("Failed to execute linker")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("Linker error during {}: {}", operation, stderr));
        }

        Ok(())
    }

    /// Get the appropriate linker command based on platform
    fn get_linker_command(&self) -> Result<Command> {
        let linker = if cfg!(target_os = "windows") {
            "link"
        } else if cfg!(target_os = "macos") {
            "clang"
        } else {
            "ld"
        };

        let cmd = Command::new(linker);
        Ok(cmd)
    }

    /// Check if the linker is available
    pub fn is_available() -> bool {
        Self::check_linker_availability()
    }

    /// Check if necessary linker tools are available
    fn check_linker_availability() -> bool {
        // Check for basic linker tools
        let linkers = if cfg!(target_os = "windows") {
            vec!["link"]
        } else {
            vec!["ld", "ar"]
        };

        for linker in linkers {
            if which::which(linker).is_err() {
                return false;
            }
        }

        true
    }
}

/// High-level interface for linking LLVM modules
pub struct ModuleLinker {
    #[allow(dead_code)]
    linker_config: LinkerConfig,
    temp_dir: Option<PathBuf>,
}

impl ModuleLinker {
    /// Create a new module linker
    pub fn new(linker_config: LinkerConfig) -> Self {
        Self {
            linker_config,
            temp_dir: None,
        }
    }

    /// Set a temporary directory for intermediate files
    pub fn with_temp_dir(mut self, temp_dir: PathBuf) -> Self {
        self.temp_dir = Some(temp_dir);
        self
    }

    /// Link LLVM modules to create the final output
    pub fn link_modules(
        &self,
        modules: &[&Module<'_>],
        _target_codegen: &crate::target::TargetCodegen,
    ) -> Result<PathBuf> {
        let module_names: Vec<_> = modules
            .iter()
            .map(|module| module.get_name().to_str().unwrap_or("module").to_string())
            .collect();
        Err(anyhow!(
            "LLVM module linking is not implemented. Modules: {:?}. Provide a real linker implementation instead of writing a stub.",
            module_names
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_linker_config_creation() {
        let config = LinkerConfig::executable("test_output");
        assert_eq!(config.output_path, PathBuf::from("test_output"));
        assert_eq!(config.output_type, OutputType::Executable);
    }

    #[test]
    fn test_linker_config_builder() {
        let config = LinkerConfig::executable("test")
            .with_library("m")
            .with_library_path("/usr/lib")
            .with_stripped_symbols()
            .with_size_optimization();

        assert!(config.link_libraries.contains(&"m".to_string()));
        assert!(config.library_paths.contains(&PathBuf::from("/usr/lib")));
        assert!(config.strip_symbols);
        assert!(config.optimize_for_size);
    }

    #[test]
    fn test_static_library_config() {
        let config = LinkerConfig::static_library("libtest.a");
        assert_eq!(config.output_type, OutputType::StaticLibrary);
        assert_eq!(config.output_path, PathBuf::from("libtest.a"));
    }

    #[test]
    fn test_dynamic_library_config() {
        let config = LinkerConfig::dynamic_library("libtest.so");
        assert_eq!(config.output_type, OutputType::DynamicLibrary);
        assert_eq!(config.output_path, PathBuf::from("libtest.so"));
    }

    #[test]
    fn test_linker_availability() {
        // This test might fail on some systems, but it's useful for CI
        let available = LlvmLinker::is_available();
        tracing::info!("Linker availability: {}", available);
    }
}
