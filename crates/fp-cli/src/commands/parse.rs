use crate::{Result, cli::CliConfig, pipeline::Pipeline};
use fp_core::{
    package::provider::{ModuleSource, PackageProvider},
    pretty::{PrettyOptions, pretty},
};
use fp_typescript::{
    frontend::{TsParseMode, collect_import_references},
    package::TypeScriptPackageProvider,
};
use std::collections::{HashSet, VecDeque};
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
    if is_typescript_package_manifest(path) {
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
    println!("File {}:", path.display());
    println!("{}", pretty(&ast, pretty_opts));

    if resolve_imports && is_typescript_source_file(path) {
        resolve_typescript_imports(path, &source, pipeline, mode)?;
    }
    Ok(())
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

fn resolve_typescript_imports(
    root_path: &Path,
    root_source: &str,
    pipeline: &mut Pipeline,
    mode: TsParseMode,
) -> Result<()> {
    let anchor = root_path
        .parent()
        .map(normalize_path)
        .unwrap_or_else(|| normalize_path(root_path));

    let mut visited = HashSet::new();
    visited.insert(normalize_path(root_path));

    let mut pending = VecDeque::new();
    enqueue_imports(root_path, root_source, &mut pending, &mut visited);

    if pending.is_empty() {
        return Ok(());
    }

    let mut pretty_opts = PrettyOptions::default();
    pretty_opts.show_types = false;
    pretty_opts.show_spans = false;

    let mut resolved_count = 0usize;
    while let Some(dep_path) = pending.pop_front() {
        let source = match fs::read_to_string(&dep_path) {
            Ok(source) => source,
            Err(err) => {
                eprintln!(
                    "Warning: failed to read resolved import {} ({err})",
                    dep_path.display()
                );
                continue;
            }
        };

        let ast = match pipeline.parse_source_public(&source, Some(dep_path.as_path())) {
            Ok(ast) => ast,
            Err(err) => {
                if matches!(mode, TsParseMode::Loose) {
                    eprintln!(
                        "Warning: failed to parse resolved import {} ({}). Skipping.",
                        dep_path.display(),
                        err
                    );
                    continue;
                }
                return Err(err);
            }
        };

        let display_path = display_relative(&dep_path, &anchor);
        println!("File {} (resolved import):", display_path);
        println!("{}", pretty(&ast, pretty_opts.clone()));
        resolved_count += 1;

        enqueue_imports(&dep_path, &source, &mut pending, &mut visited);
    }

    if resolved_count > 0 {
        println!(
            "Resolved {resolved_count} imported module{} for {}",
            if resolved_count == 1 { "" } else { "s" },
            root_path.display()
        );
    }

    Ok(())
}

fn enqueue_imports(
    current_path: &Path,
    source: &str,
    pending: &mut VecDeque<PathBuf>,
    visited: &mut HashSet<PathBuf>,
) {
    let references = collect_import_references(source, Some(current_path));
    let base_dir = current_path.parent().unwrap_or_else(|| Path::new("."));

    for reference in references {
        if reference.is_type_only || !is_local_spec(&reference.spec) {
            continue;
        }
        match resolve_import_spec(base_dir, &reference.spec) {
            Some(candidate) => {
                let normalized = normalize_path(candidate.as_path());
                if visited.insert(normalized.clone()) && is_typescript_source_file(&normalized) {
                    pending.push_back(normalized);
                }
            }
            None => {
                eprintln!(
                    "Warning: unable to resolve import '{}' from {}",
                    reference.spec,
                    current_path.display()
                );
            }
        }
    }
}

fn resolve_import_spec(base_dir: &Path, spec: &str) -> Option<PathBuf> {
    let spec = strip_query_fragment(spec);
    let spec = if spec != "/" {
        spec.trim_end_matches('/')
    } else {
        spec
    };

    let raw_path = if spec.starts_with('/') {
        PathBuf::from(spec)
    } else {
        base_dir.join(spec)
    };

    for candidate in candidate_import_paths(&raw_path) {
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}

fn candidate_import_paths(base: &Path) -> Vec<PathBuf> {
    const FILE_EXTS: &[&str] = &["ts", "tsx", "mts", "cts"];
    const INDEX_FILES: &[&str] = &["index.ts", "index.tsx", "index.mts", "index.cts"];

    let mut candidates = Vec::new();
    candidates.push(base.to_path_buf());

    if base.extension().is_none() {
        for ext in FILE_EXTS {
            candidates.push(base.with_extension(ext));
        }
        for index in INDEX_FILES {
            candidates.push(base.join(index));
        }
    }

    candidates
}

fn strip_query_fragment(spec: &str) -> &str {
    spec.split(|ch| ch == '?' || ch == '#')
        .next()
        .unwrap_or(spec)
}

fn is_local_spec(spec: &str) -> bool {
    spec.starts_with("./") || spec.starts_with("../") || spec.starts_with('/')
}

fn is_typescript_source_file(path: &Path) -> bool {
    match path.extension().and_then(|ext| ext.to_str()) {
        Some(ext) => {
            let ext_lower = ext.to_ascii_lowercase();
            matches!(ext_lower.as_str(), "ts" | "tsx" | "mts" | "cts")
                && !path
                    .file_name()
                    .and_then(|name| name.to_str())
                    .map(|name| {
                        name.ends_with(".d.ts")
                            || name.ends_with(".d.mts")
                            || name.ends_with(".d.cts")
                    })
                    .unwrap_or(false)
        }
        None => false,
    }
}

fn normalize_path(path: &Path) -> PathBuf {
    match path.canonicalize() {
        Ok(path) => path,
        Err(_) => path.to_path_buf(),
    }
}

fn display_relative(path: &Path, anchor: &Path) -> String {
    pathdiff::diff_paths(path, anchor)
        .unwrap_or_else(|| path.to_path_buf())
        .to_string_lossy()
        .to_string()
}
