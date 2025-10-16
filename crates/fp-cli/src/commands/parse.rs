use crate::{Result, cli::CliConfig, pipeline::Pipeline};
use fp_core::{
    package::provider::{ModuleSource, PackageProvider},
    pretty::{PrettyOptions, pretty},
};
use fp_typescript::{frontend::TsParseMode, package::TypeScriptPackageProvider};
use pathdiff::diff_paths;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct ParseArgs {
    /// Expression to parse
    pub expr: Option<String>,
    /// Files containing code to parse
    pub files: Vec<PathBuf>,
    /// Parse mode for TypeScript sources
    pub parse_mode: TsParseMode,
    /// Resolve and parse imported modules recursively
    pub resolve_imports: bool,
}

pub async fn parse_command(args: ParseArgs, _config: &CliConfig) -> Result<()> {
    if let Some(expr) = args.expr {
        if !args.files.is_empty() {
            return Err(crate::CliError::Compilation(
                "Cannot specify both --expr and file paths".to_string(),
            ));
        }
        let mut pipeline = Pipeline::new();
        pipeline.set_typescript_parse_mode(args.parse_mode);
        return parse_expression(&mut pipeline, expr);
    }

    if args.files.is_empty() {
        return Err(crate::CliError::Compilation(
            "Must specify either --expr or at least one path".to_string(),
        ));
    }

    let mut pipeline = Pipeline::new();
    pipeline.set_typescript_parse_mode(args.parse_mode);
    for path in args.files {
        parse_path(&path, &mut pipeline, args.parse_mode, args.resolve_imports)?;
    }
    Ok(())
}

fn parse_expression(pipeline: &mut Pipeline, expr: String) -> Result<()> {
    let ast = pipeline.parse_source_public(&expr, None)?;
    let mut pretty_opts = PrettyOptions::default();
    pretty_opts.show_types = false;
    pretty_opts.show_spans = false;
    println!("{}", pretty(&ast, pretty_opts));
    Ok(())
}

fn parse_path(
    path: &Path,
    pipeline: &mut Pipeline,
    mode: TsParseMode,
    resolve_imports: bool,
) -> Result<()> {
    if is_tsconfig(path) {
        parse_tsconfig(path, pipeline, mode)
    } else if is_typescript_package_manifest(path) {
        parse_typescript_package(path, pipeline, mode)
    } else {
        parse_file(path, pipeline, mode, resolve_imports)
    }
}

fn is_typescript_package_manifest(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .map(|name| name == "package.json")
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

fn parse_file(
    path: &Path,
    pipeline: &mut Pipeline,
    mode: TsParseMode,
    resolve_imports: bool,
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
    println!("File {}:", path.display());
    println!("{}", pretty(&ast, print_opts));

    if resolve_imports {
        if let Some(frontend) = pipeline.typescript_frontend() {
            let outcome = frontend
                .parse_dependencies(path, &source, true)
                .map_err(|err| crate::CliError::Compilation(err.to_string()))?;

            for warning in &outcome.warnings {
                eprintln!("{warning}");
            }

            if !outcome.modules.is_empty() {
                let anchor = path
                    .parent()
                    .map(Path::to_path_buf)
                    .unwrap_or_else(|| PathBuf::from("."));
                let count = outcome.modules.len();
                for (module_path, node) in outcome.modules {
                    let display_path = display_relative(&module_path, &anchor);
                    println!("File {} (resolved import):", display_path);
                    println!("{}", pretty(&node, pretty_opts.clone()));
                }
                println!(
                    "Resolved {count} imported module{} for {}",
                    if count == 1 { "" } else { "s" },
                    path.display()
                );
            }
        }
    }
    Ok(())
}

fn parse_tsconfig(path: &Path, pipeline: &mut Pipeline, mode: TsParseMode) -> Result<()> {
    let config_root = path
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));
    let manifest = config_root.join("package.json");

    if manifest.exists() {
        println!(
            "Detected package manifest at {} – using package module discovery.",
            manifest.display()
        );
        return parse_typescript_package(&manifest, pipeline, mode);
    }

    Err(crate::CliError::Compilation(format!(
        "TypeScript parsing from tsconfig currently requires a package manifest at {}",
        manifest.display()
    )))
}

fn parse_typescript_package(path: &Path, pipeline: &mut Pipeline, mode: TsParseMode) -> Result<()> {
    let package_root = path
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));

    let provider = TypeScriptPackageProvider::new(package_root.clone());
    provider.refresh().map_err(|err| {
        crate::CliError::Compilation(format!("Failed to load TypeScript package: {err}"))
    })?;

    let package_ids = provider.list_packages().map_err(|err| {
        crate::CliError::Compilation(format!("Failed to enumerate TypeScript packages: {err}"))
    })?;

    if package_ids.is_empty() {
        println!(
            "No TypeScript modules found in package {}",
            package_root.display()
        );
        return Ok(());
    }

    let package_id = &package_ids[0];
    let module_ids = provider.modules_for_package(package_id).map_err(|err| {
        crate::CliError::Compilation(format!("Failed to list TypeScript modules: {err}"))
    })?;

    if module_ids.is_empty() {
        println!(
            "No TypeScript modules found in package {}",
            package_root.display()
        );
        return Ok(());
    }

    let mut pretty_opts = PrettyOptions::default();
    pretty_opts.show_types = false;
    pretty_opts.show_spans = false;

    let mut parsed = 0usize;
    for module_id in module_ids {
        let descriptor = provider.load_module_descriptor(&module_id).map_err(|err| {
            crate::CliError::Compilation(format!("Failed to load module descriptor: {err}"))
        })?;

        let mut disk_path = descriptor.source.to_path_buf();
        if !disk_path.is_absolute() {
            disk_path = package_root.join(disk_path);
        }

        let source = fs::read_to_string(&disk_path).map_err(|err| {
            crate::CliError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Failed to read module {}: {err}", disk_path.display()),
            ))
        })?;

        let ast = match pipeline.parse_source_public(&source, Some(disk_path.as_path())) {
            Ok(ast) => ast,
            Err(err) => {
                if matches!(mode, TsParseMode::Loose) {
                    eprintln!(
                        "Warning: failed to parse module {} ({}). Skipping.",
                        descriptor.id.as_str(),
                        err
                    );
                    continue;
                }
                return Err(err);
            }
        };
        println!("Module {} ({}):", descriptor.id.as_str(), descriptor.source);
        println!("{}", pretty(&ast, pretty_opts.clone()));
        parsed += 1;
    }

    println!(
        "Parsed {parsed} TypeScript modules from package {}",
        package_root.display()
    );

    Ok(())
}

fn display_relative(path: &Path, anchor: &Path) -> String {
    diff_paths(path, anchor)
        .unwrap_or_else(|| path.to_path_buf())
        .to_string_lossy()
        .to_string()
}
