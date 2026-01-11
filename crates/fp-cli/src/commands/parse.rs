use crate::{Result, cli::CliConfig, pipeline::Pipeline};
use clap::{ArgAction, Args, ValueEnum, ValueHint};
use fp_core::pretty::{PrettyOptions, pretty};
use fp_typescript::frontend::TsParseMode;
use serde_json;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Copy, Clone, Debug, ValueEnum)]
pub enum ParseModeArg {
    Strict,
    Loose,
}

impl From<ParseModeArg> for TsParseMode {
    fn from(value: ParseModeArg) -> Self {
        match value {
            ParseModeArg::Strict => TsParseMode::Strict,
            ParseModeArg::Loose => TsParseMode::Loose,
        }
    }
}

#[derive(Debug, Clone, Args)]
pub struct ParseArgs {
    /// Expression to parse
    #[arg(short, long, conflicts_with = "files")]
    pub expr: Option<String>,
    /// Files containing code to parse
    #[arg(value_hint = ValueHint::FilePath)]
    pub files: Vec<PathBuf>,
    /// Parse mode for TypeScript sources
    #[arg(long = "parse-mode", default_value_t = ParseModeArg::Strict, value_enum)]
    pub parse_mode: ParseModeArg,
    /// Resolve and parse imported modules recursively (disable with --no-resolve)
    #[arg(long = "no-resolve", action = ArgAction::SetFalse, default_value_t = true)]
    pub resolve_imports: bool,
    /// Path to persist the parsed AST snapshot as JSON
    #[arg(long, value_hint = ValueHint::FilePath)]
    pub snapshot: Option<PathBuf>,
}

pub async fn parse_command(mut args: ParseArgs, _config: &CliConfig) -> Result<()> {
    if args.snapshot.is_some() {
        let source_count = args.files.len() + usize::from(args.expr.is_some());
        if source_count != 1 {
            return Err(crate::CliError::Compilation(
                "--snapshot expects exactly one source (file or --expr)".to_string(),
            ));
        }
        if args.resolve_imports {
            return Err(crate::CliError::Compilation(
                "--snapshot cannot be combined with --no-resolve=false".to_string(),
            ));
        }
    }

    if let Some(expr) = args.expr {
        if !args.files.is_empty() {
            return Err(crate::CliError::Compilation(
                "Cannot specify both --expr and file paths".to_string(),
            ));
        }
        let mut pipeline = Pipeline::new();
        pipeline.set_typescript_parse_mode(args.parse_mode.into());
        return parse_expression(&mut pipeline, expr, args.snapshot.take());
    }

    if args.files.is_empty() {
        return Err(crate::CliError::Compilation(
            "Must specify either --expr or at least one path".to_string(),
        ));
    }

    crate::commands::validate_paths_exist(&args.files, true, "parse")?;

    let mut pipeline = Pipeline::new();
    pipeline.set_typescript_parse_mode(args.parse_mode.into());
    let mut snapshot = args.snapshot.take();
    for (index, path) in args.files.into_iter().enumerate() {
        let snapshot_for_file = if index == 0 { snapshot.take() } else { None };
        parse_path(
            &path,
            &mut pipeline,
            args.parse_mode.into(),
            args.resolve_imports,
            snapshot_for_file,
        )?;
    }
    Ok(())
}

fn parse_expression(
    pipeline: &mut Pipeline,
    expr: String,
    snapshot: Option<PathBuf>,
) -> Result<()> {
    let ast = pipeline.parse_source_public(&expr, None)?;
    let print_ast = snapshot.is_none();
    persist_snapshot(&ast, snapshot.as_deref())?;
    let mut pretty_opts = PrettyOptions::default();
    pretty_opts.show_types = false;
    pretty_opts.show_spans = false;
    if print_ast {
        println!("{}", pretty(&ast, pretty_opts));
    }
    Ok(())
}

fn parse_path(
    path: &Path,
    pipeline: &mut Pipeline,
    mode: TsParseMode,
    resolve_imports: bool,
    snapshot: Option<PathBuf>,
) -> Result<()> {
    if is_package_manifest(path) || is_tsconfig(path) {
        return Err(crate::CliError::Compilation(
            "fp parse only accepts source files; use magnet for package manifests".to_string(),
        ));
    }
    parse_file(path, pipeline, mode, resolve_imports, snapshot)
}

fn parse_file(
    path: &Path,
    pipeline: &mut Pipeline,
    mode: TsParseMode,
    resolve_imports: bool,
    snapshot: Option<PathBuf>,
) -> Result<()> {
    let source = fs::read_to_string(path).map_err(|err| {
        crate::CliError::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("Failed to read file {}: {err}", path.display()),
        ))
    })?;

    let ast = match pipeline.parse_source_public(&source, Some(path)) {
        Ok(ast) => ast,
        Err(err) => {
            if matches!(mode, TsParseMode::Loose) {
                eprintln!(
                    "Warning: failed to parse {} ({}). Skipping.",
                    path.display(),
                    err
                );
                return Ok(());
            }
            return Err(err);
        }
    };
    let mut pretty_opts = PrettyOptions::default();
    pretty_opts.show_types = false;
    pretty_opts.show_spans = false;
    let print_opts = pretty_opts.clone();
    let print_ast = snapshot.is_none();
    persist_snapshot(&ast, snapshot.as_deref())?;
    if print_ast {
        println!("File {}:", path.display());
        println!("{}", pretty(&ast, print_opts));
    }

    if resolve_imports {
        return Err(crate::CliError::Compilation(
            "--resolve is not supported in fp parse; use magnet for import resolution".to_string(),
        ));
    }
    Ok(())
}

fn persist_snapshot(ast: &fp_core::ast::Node, snapshot: Option<&Path>) -> Result<()> {
    if let Some(path) = snapshot {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).map_err(|err| {
                crate::CliError::Io(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!(
                        "Failed to create snapshot directory {}: {err}",
                        parent.display()
                    ),
                ))
            })?;
        }
        let json = serde_json::to_string_pretty(ast).map_err(|err| {
            crate::CliError::Compilation(format!("Failed to serialize AST to JSON: {err}"))
        })?;
        fs::write(path, json).map_err(|err| {
            crate::CliError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Failed to write snapshot {}: {err}", path.display()),
            ))
        })?;
        println!("Wrote AST snapshot to {}", path.display());
    }
    Ok(())
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
