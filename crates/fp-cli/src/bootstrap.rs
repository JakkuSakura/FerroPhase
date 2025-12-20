use crate::config::{
    DebugOptions, ErrorToleranceOptions, PipelineOptions, PipelineTarget, RuntimeConfig,
};
use crate::pipeline::{Pipeline, PipelineOutput};
use crate::{CliError, Result};
use std::io::{self, Write};
use std::path::{Path, PathBuf};

fn read_env_path(key: &str) -> Option<PathBuf> {
    std::env::var_os(key).map(PathBuf::from)
}

pub fn bootstrap_compile_from_env() -> Result<()> {
    // Expect environment variables set by the bootstrap script
    let snapshot = read_env_path("FP_BOOTSTRAP_SNAPSHOT")
        .ok_or_else(|| CliError::Compilation("FP_BOOTSTRAP_SNAPSHOT not set".to_string()))?;
    let output = read_env_path("FP_BOOTSTRAP_OUTPUT");
    bootstrap_compile(&snapshot, output.as_deref())
}

pub fn bootstrap_compile(snapshot: &Path, output: Option<&Path>) -> Result<()> {
    // Configure pipeline for a minimal, deterministic LLVM emit without Clap/tracing
    let target = PipelineTarget::Llvm;
    let base_path = output
        .map(Path::to_path_buf)
        .unwrap_or_else(|| snapshot.with_extension("ll"));

    let options = PipelineOptions {
        target,
        runtime: RuntimeConfig {
            runtime_type: "literal".to_string(),
            options: std::collections::HashMap::new(),
        },
        source_language: None,
        optimization_level: 0,
        save_intermediates: true,
        base_path: Some(base_path.clone()),
        debug: DebugOptions {
            print_ast: false,
            print_passes: false,
            verbose: false,
        },
        error_tolerance: ErrorToleranceOptions {
            enabled: true,
            max_errors: 1000,
            show_all_errors: false,
            continue_on_error: true,
        },
        release: true,
        execute_main: false,
        bootstrap_mode: true,
        emit_bootstrap_snapshot: false,
        disabled_stages: Vec::new(),
    };

    let mut pipeline = Pipeline::new();

    // Use the blocking snapshot path intended for bootstrap; this avoids Tokio.
    let output_ir = pipeline.execute_compilation_from_snapshot_blocking(snapshot, options)?;

    match output_ir {
        PipelineOutput::Code(code) => {
            if let Some(out) = output {
                if let Some(parent) = out.parent() {
                    std::fs::create_dir_all(parent).map_err(CliError::Io)?;
                }
                std::fs::write(out, code.as_bytes()).map_err(CliError::Io)?;
            } else {
                print!("{}", code);
                let _ = io::stdout().flush();
            }
            Ok(())
        }
        other => Err(CliError::Compilation(format!(
            "bootstrap expected LLVM code, got {:?}",
            other
        ))),
    }
}
