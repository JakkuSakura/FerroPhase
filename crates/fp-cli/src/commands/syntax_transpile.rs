//! AST-level transpilation command.
//!
//! This command performs parsing + (optional) const-eval/type-enrichment preparation and then
//! serializes the resulting AST into a target language. It does not lower through MIR/LIR.

use crate::{
    CliError, Result,
    cli::CliConfig,
    pipeline::{Pipeline, TranspilePreparationOptions},
};
use clap::Args;
use console::style;
use fp_core::ast::{AstSerializer, Node};
use fp_lang::PrettyAstSerializer;
use fp_csharp::CSharpSerializer;
use fp_python::PythonSerializer;
use fp_sycl::SyclSerializer;
use fp_typescript::{JavaScriptSerializer, TypeScriptSerializer};
use fp_wit::WorldMode;
use fp_wit::{WitOptions, WitSerializer};
use fp_zig::ZigSerializer;
use std::path::{Path, PathBuf};
use tracing::{info, info_span};

/// Arguments for the syntax-transpile command (also used by Clap)
#[derive(Debug, Clone, Args)]
pub struct SyntaxTranspileArgs {
    /// Input file(s) to transpile
    #[arg(required = true)]
    pub input: Vec<PathBuf>,

    /// Target language (javascript, typescript, python, rust, ...)
    #[arg(short, long, default_value = "typescript")]
    pub target: String,

    /// Output file or directory
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Perform const evaluation before transpilation
    #[arg(long, default_value_t = true)]
    pub const_eval: bool,

    /// Preserve struct types and methods
    #[arg(long, default_value_t = true)]
    pub preserve_structs: bool,

    /// Generate type definitions (for TypeScript)
    #[arg(long)]
    pub type_defs: bool,

    /// Pretty print output
    #[arg(long, default_value_t = true)]
    pub pretty: bool,

    /// Include source maps
    #[arg(long)]
    pub source_maps: bool,

    /// Generate a single WIT world instead of per-package worlds
    #[arg(long)]
    pub single_world: bool,
}

/// Execute the syntax-transpile command.
pub async fn syntax_transpile_command(args: SyntaxTranspileArgs, config: &CliConfig) -> Result<()> {
    info!("Starting syntax-transpilation to target: {}", args.target);

    crate::commands::validate_paths_exist(&args.input, false, "syntax-transpile")?;

    // Watch mode removed: always perform a single pass
    syntax_transpile_once(args, config).await
}

async fn syntax_transpile_once(args: SyntaxTranspileArgs, config: &CliConfig) -> Result<()> {
    let progress = crate::commands::setup_progress_bar(args.input.len());

    for input_file in &args.input {
        progress.set_message(format!("Transpiling {}", input_file.display()));

        let output_file = crate::languages::backend::resolve_output_path(
            input_file,
            args.output.as_ref(),
            &args.target,
        )?;
        syntax_transpile_file(input_file, &output_file, &args, config).await?;

        progress.inc(1);
    }

    progress.finish_with_message(format!(
        "{} Transpiled {} file(s) to {}",
        style("✓").green(),
        args.input.len(),
        args.target
    ));

    Ok(())
}

async fn syntax_transpile_file(
    input: &Path,
    output: &Path,
    args: &SyntaxTranspileArgs,
    _config: &CliConfig,
) -> Result<()> {
    let span = info_span!(
        "syntax-transpile.file",
        input = %input.display(),
        output = %output.display(),
        target = %args.target
    );
    let _enter = span.enter();

    info!(
        "Transpiling: {} -> {} ({})",
        input.display(),
        output.display(),
        args.target
    );

    if is_package_manifest(input) || is_tsconfig(input) {
        return Err(CliError::Compilation(
            "fp syntax-transpile only accepts source files; use magnet for package manifests"
                .to_string(),
        ));
    }

    // Parse source
    let mut pipeline = Pipeline::new();

    use crate::languages::frontend::{LanguageSource, detect_language_source_by_path};
    let detected = detect_language_source_by_path(input);
    let is_wit_input = matches!(detected, Some(LanguageSource::Wit));
    let is_typescript_input = matches!(
        detected,
        Some(LanguageSource::TypeScript | LanguageSource::JavaScript)
    );

    let source = std::fs::read_to_string(input).map_err(CliError::Io)?;
    info!(path = %input.display(), "Parsing source file");
    let mut ast = pipeline.parse_source_public(&source, Some(input))?;
    let base_path = None;

    let prep_options = TranspilePreparationOptions {
        run_const_eval: args.const_eval,
        save_intermediates: false,
        base_path,
    };
    if !is_wit_input && !is_typescript_input {
        pipeline.prepare_for_transpile(&mut ast, &prep_options)?;
    }

    // Resolve target using shared backend mapping
    let target = crate::languages::backend::parse_language_target(&args.target)?;
    let wit_options = if matches!(target, crate::languages::backend::LanguageTarget::Wit) {
        Some(build_wit_options(input, args.single_world))
    } else {
        None
    };
    let result = syntax_transpile_node(&ast, target, args.type_defs, wit_options)?;

    // Write output
    std::fs::write(output, result.code).map_err(CliError::Io)?;
    if let Some(defs) = result.type_defs {
        let mut defs_path = output.to_path_buf();
        if let Some(stem) = defs_path.file_stem().and_then(|s| s.to_str()) {
            defs_path.set_file_name(format!("{}.d.ts", stem));
        } else {
            defs_path.set_extension("d.ts");
        }
        std::fs::write(defs_path, defs).map_err(CliError::Io)?;
    }
    info!("Generated: {}", output.display());

    Ok(())
}

#[derive(Debug, Default, Clone)]
struct BackendTranspileResult {
    code: String,
    type_defs: Option<String>,
}

fn syntax_transpile_node(
    node: &Node,
    target: crate::languages::backend::LanguageTarget,
    emit_type_defs: bool,
    wit_options: Option<WitOptions>,
) -> Result<BackendTranspileResult> {
    match target {
        crate::languages::backend::LanguageTarget::TypeScript => {
            let serializer = TypeScriptSerializer::new(emit_type_defs);
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult {
                code,
                type_defs: serializer.take_type_defs(),
            })
        }
        crate::languages::backend::LanguageTarget::JavaScript => {
            let serializer = JavaScriptSerializer;
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult {
                code,
                type_defs: None,
            })
        }
        crate::languages::backend::LanguageTarget::CSharp => {
            let serializer = CSharpSerializer;
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult {
                code,
                type_defs: None,
            })
        }
        crate::languages::backend::LanguageTarget::Python => {
            let serializer = PythonSerializer;
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult {
                code,
                type_defs: None,
            })
        }
        crate::languages::backend::LanguageTarget::Zig => {
            let serializer = ZigSerializer;
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult {
                code,
                type_defs: None,
            })
        }
        crate::languages::backend::LanguageTarget::Sycl => {
            let serializer = SyclSerializer;
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult {
                code,
                type_defs: None,
            })
        }
        crate::languages::backend::LanguageTarget::Rust => {
            let serializer = PrettyAstSerializer::new();
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult {
                code,
                type_defs: None,
            })
        }
        crate::languages::backend::LanguageTarget::Wit => {
            let serializer = match wit_options {
                Some(options) => WitSerializer::with_options(options),
                None => WitSerializer::new(),
            };
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult {
                code,
                type_defs: None,
            })
        }
    }
}

fn build_wit_options(input: &Path, single_world: bool) -> WitOptions {
    let namespace = input
        .parent()
        .and_then(|dir| dir.file_name())
        .and_then(|os| os.to_str())
        .map(sanitize_wit_component)
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| "ferrophase".to_string());

    let interface = input
        .file_stem()
        .and_then(|stem| stem.to_str())
        .map(sanitize_wit_component)
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| "module".to_string());

    let mut options = WitOptions::default();
    options.package = format!("{namespace}:{interface}");
    options.root_interface = interface.clone();
    if single_world {
        options.world_mode = WorldMode::Single {
            world_name: interface,
        };
    }
    options
}

fn sanitize_wit_component(raw: &str) -> String {
    let mut result = String::new();
    for ch in raw.chars() {
        match ch {
            'a'..='z' | '0'..='9' => result.push(ch),
            'A'..='Z' => result.push(ch.to_ascii_lowercase()),
            '_' | '-' => result.push('_'),
            '/' | ':' | '.' | '@' => result.push('_'),
            _ => {}
        }
    }
    if result.is_empty() {
        result.push_str("module");
    }
    if result
        .chars()
        .next()
        .map(|ch| ch.is_ascii_digit())
        .unwrap_or(false)
    {
        result.insert(0, '_');
    }
    result
}

fn is_package_manifest(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .map(|name| {
            let lower = name.to_ascii_lowercase();
            matches!(
                lower.as_str(),
                "cargo.toml" | "package.json" | "magnet.toml"
            )
        })
        .unwrap_or(false)
}

fn is_tsconfig(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .map(|name| {
            let lower = name.to_ascii_lowercase();
            lower == "tsconfig.json" || lower.ends_with(".tsconfig.json")
        })
        .unwrap_or(false)
}
