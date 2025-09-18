use crate::config::PipelineOptions;
use crate::CliError;
use std::process::Command;

/// Binary compilation utilities using llc + lld
pub struct BinaryCompiler;

impl BinaryCompiler {
    /// Run llc to compile LLVM IR to object file
    pub fn run_llc(ll_path: &std::path::Path, obj_path: &std::path::Path, options: &PipelineOptions) -> Result<String, CliError> {
        let mut cmd = Command::new("llc");
        cmd.arg("-filetype=obj")
           .arg(ll_path)
           .arg("-o")
           .arg(obj_path);
        
        // Add target architecture (minimal process as specified)
        #[cfg(target_arch = "x86_64")]
        cmd.arg("-march=x86_64");
        
        #[cfg(target_arch = "aarch64")]
        cmd.arg("-march=arm64");
        
        // Add optimization if requested
        if options.optimization_level > 0 {
            cmd.arg("-O2");
        }
        
        if options.debug.verbose {
            cmd.arg("-v");
        }
        
        let output = cmd.output()
            .map_err(|e| CliError::Compilation(format!("Failed to run llc: {}. Make sure LLVM is installed.", e)))?;
        
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(CliError::Compilation(format!("llc failed: {}", stderr)));
        }
        
        Ok(format!("llc: {} -> {}", ll_path.display(), obj_path.display()))
    }
    
    /// Link object file to binary (tries clang first, then lld, then ld)
    pub fn link_binary(obj_path: &std::path::Path, binary_path: &std::path::Path, options: &PipelineOptions) -> Result<String, CliError> {
        // Try clang first (easiest and most compatible)
        if std::process::Command::new("clang").arg("--version").output().is_ok() {
            return Self::run_clang(obj_path, binary_path, options);
        }
        
        // Try to find lld in common locations
        let lld_cmd = if std::process::Command::new("lld").arg("--version").output().is_ok() {
            "lld"
        } else if std::process::Command::new("/opt/homebrew/bin/lld").arg("--version").output().is_ok() {
            "/opt/homebrew/bin/lld"
        } else if std::process::Command::new("/opt/homebrew/Cellar/lld@19/19.1.7/bin/lld").arg("--version").output().is_ok() {
            "/opt/homebrew/Cellar/lld@19/19.1.7/bin/lld"
        } else {
            // Fallback to system ld instead of lld
            return Self::run_system_ld(obj_path, binary_path, options);
        };
        
        let mut cmd = Command::new(lld_cmd);
        
        // Minimal lld linking process as specified
        #[cfg(target_os = "linux")]
        {
            cmd.arg("-flavor").arg("gnu")
               .arg(obj_path)
               .arg("-o").arg(binary_path)
               .arg("-lc")
               .arg("--dynamic-linker").arg("/lib64/ld-linux-x86-64.so.2");
        }
        
        #[cfg(target_os = "macos")]
        {
            cmd.arg("-flavor").arg("darwin")
               .arg(obj_path)
               .arg("-o").arg(binary_path)
               .arg("-lSystem")
               .arg("-platform_version").arg("macos").arg("14.0").arg("0");
            
            #[cfg(target_arch = "aarch64")]
            cmd.arg("-arch").arg("arm64");
            
            #[cfg(target_arch = "x86_64")]
            cmd.arg("-arch").arg("x86_64");
        }
        
        if options.debug.verbose {
            cmd.arg("-v");
        }
        
        let output = cmd.output()
            .map_err(|e| CliError::Compilation(format!("Failed to run lld: {}. Make sure LLVM is installed.", e)))?;
        
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(CliError::Compilation(format!("lld failed: {}", stderr)));
        }
        
        // Make binary executable
        Self::make_executable(binary_path)?;
        
        Ok(format!("lld: {} -> {}", obj_path.display(), binary_path.display()))
    }
    
    /// Run clang to link object file to binary (easiest and most compatible)
    pub fn run_clang(obj_path: &std::path::Path, binary_path: &std::path::Path, options: &PipelineOptions) -> Result<String, CliError> {
        let mut cmd = Command::new("clang");
        
        // Simple clang linking - it handles all the platform details
        cmd.arg(obj_path)
           .arg("-o").arg(binary_path);
        
        // Add optimization if requested
        if options.optimization_level > 0 {
            cmd.arg("-O2");
        }
        
        if options.debug.verbose {
            cmd.arg("-v");
        }
        
        let output = cmd.output()
            .map_err(|e| CliError::Compilation(format!("Failed to run clang: {}. Make sure clang is installed.", e)))?;
        
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(CliError::Compilation(format!("clang failed: {}", stderr)));
        }
        
        // Make binary executable
        Self::make_executable(binary_path)?;
        
        Ok(format!("clang: {} -> {}", obj_path.display(), binary_path.display()))
    }
    
    /// Run system ld to link object file to binary (minimal fallback when lld is not available)
    pub fn run_system_ld(obj_path: &std::path::Path, binary_path: &std::path::Path, options: &PipelineOptions) -> Result<String, CliError> {
        let mut cmd = Command::new("ld");
        
        // Minimal linking process
        #[cfg(target_os = "linux")]
        {
            cmd.arg(obj_path)
               .arg("-o").arg(binary_path)
               .arg("-lc")
               .arg("-dynamic-linker").arg("/lib64/ld-linux-x86-64.so.2");
        }
        
        #[cfg(target_os = "macos")]
        {
            cmd.arg(obj_path)
               .arg("-o").arg(binary_path)
               .arg("-lSystem")
               .arg("-macosx_version_min").arg("14.0");
            
            #[cfg(target_arch = "aarch64")]
            cmd.arg("-arch").arg("arm64");
            
            #[cfg(target_arch = "x86_64")]
            cmd.arg("-arch").arg("x86_64");
        }
        
        if options.debug.verbose {
            cmd.arg("-v");
        }
        
        let output = cmd.output()
            .map_err(|e| CliError::Compilation(format!("Failed to run ld: {}. System linker not available.", e)))?;
        
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(CliError::Compilation(format!("ld failed: {}", stderr)));
        }
        
        // Make binary executable
        Self::make_executable(binary_path)?;
        
        Ok(format!("ld: {} -> {}", obj_path.display(), binary_path.display()))
    }
    
    /// Make a binary executable on Unix systems
    fn make_executable(binary_path: &std::path::Path) -> Result<(), CliError> {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(binary_path)
                .map_err(|e| CliError::Io(e))?
                .permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(binary_path, perms)
                .map_err(|e| CliError::Io(e))?;
        }
        Ok(())
    }
}