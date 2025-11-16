//! Simplified transpilation command using the new unified transpiler

use crate::{
    CliError, Result,
    cli::CliConfig,
    pipeline::{Pipeline, TranspilePreparationOptions},
};
use fp_core::ast::Node;
use fp_csharp::CSharpSerializer;
use fp_python::PythonSerializer;
use fp_rust::printer::RustPrinter;
use fp_typescript::{JavaScriptSerializer, TypeScriptSerializer};
use fp_wit::{WitOptions, WitSerializer};
use fp_zig::ZigSerializer;
use console::style;
use fp_core::ast::{Ident, Item, Module as AstModule, NodeKind, Visibility};
use fp_rust::{parse_cargo_workspace, printer::RustPrinter};
use fp_wit::{WitOptions, WorldMode};
use pathdiff::diff_paths;
use serde_json::Value as JsonValue;
use std::collections::BTreeMap;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;
use tracing::{error, info, info_span};

/// Arguments for the transpile command
#[derive(Debug, Clone)]
pub struct TranspileArgs {
    pub input: Vec<PathBuf>,
    pub target: String,
    pub output: Option<PathBuf>,
    pub const_eval: bool,
    pub preserve_structs: bool,
    pub type_defs: bool,
    pub pretty: bool,
    pub source_maps: bool,
    pub single_world: bool,
    pub resolve_imports: bool,
}

/// Execute the transpile command
pub async fn transpile_command(args: TranspileArgs, config: &CliConfig) -> Result<()> {
    info!("Starting transpilation to target: {}", args.target);

    crate::commands::validate_paths_exist(&args.input, false)?;

    // Watch mode removed: always perform a single pass
    transpile_once(args, config).await
}

async fn transpile_once(args: TranspileArgs, config: &CliConfig) -> Result<()> {
    let progress = crate::commands::setup_progress_bar(args.input.len());

    for input_file in &args.input {
        progress.set_message(format!("Transpiling {}", input_file.display()));

        let output_file =
            determine_transpile_output_path(input_file, args.output.as_ref(), &args.target)?;
        transpile_file(input_file, &output_file, &args, config).await?;

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

async fn transpile_file(
    input: &Path,
    output: &Path,
    args: &TranspileArgs,
    _config: &CliConfig,
) -> Result<()> {
    let span = info_span!(
        "transpile.file",
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

    let package_info = find_package_info(input);

    // Parse source
    let mut pipeline = Pipeline::new();
    let is_cargo_manifest = input
        .file_name()
        .and_then(|name| name.to_str())
        .map(|name| name.eq_ignore_ascii_case("Cargo.toml"))
        .unwrap_or(false);

    use crate::languages::frontend::{detect_language_source_by_path, LanguageSource};
    let detected = detect_language_source_by_path(input);
    let is_wit_input = matches!(detected, Some(LanguageSource::Wit));
    let is_typescript_input = matches!(
        detected,
        Some(LanguageSource::TypeScript | LanguageSource::JavaScript)
    );

    let mut source_cache: Option<String> = None;
    let module_root_path = package_info
        .as_ref()
        .map(|info| info.root.clone())
        .or_else(|| input.parent().map(|dir| dir.to_path_buf()))
        .unwrap_or_else(|| PathBuf::from("."));
    let module_root_path = canonicalize_path(&module_root_path);

    let (mut ast, base_path) = if is_cargo_manifest {
        let node = parse_cargo_workspace(input)?;
        pipeline.set_serializer(Arc::new(RustPrinter::new_with_rustfmt()));
        (node, input.parent().map(|dir| dir.to_path_buf()))
    } else {
        let source = std::fs::read_to_string(input).map_err(CliError::Io)?;
        info!(path = %input.display(), "Parsing source file");
        let node = pipeline.parse_source_public(&source, Some(input))?;
        source_cache = Some(source);
        (node, None)
    };

    let mut module_bucket = if is_typescript_input {
        Some(BTreeMap::<String, (Ident, Vec<Item>)>::new())
    } else {
        None
    };

    if args.resolve_imports && is_typescript_input {
        if let (Some(ref source), Some(frontend)) =
            (source_cache.as_ref(), pipeline.typescript_frontend())
        {
            let resolve_span = info_span!(
                "typescript.resolve",
                root = %input.display(),
                mode = ?frontend.parse_mode()
            );
            let _resolve_enter = resolve_span.enter();

            let outcome = frontend
                .parse_dependencies(input, source, true)
                .map_err(|err| CliError::Compilation(err.to_string()))?;
            for warning in outcome.warnings {
                eprintln!("{warning}");
            }
            if !outcome.modules.is_empty() {
                let resolved_modules_count = outcome.modules.len();
                if let Some(bucket) = module_bucket.as_mut() {
                    for (module_path, node) in outcome.modules {
                        tracing::debug!(path = %module_path.display(), "Parsing resolved import");
                        if let NodeKind::File(dep_file) = node.kind() {
                            merge_module_items(
                                bucket,
                                module_ident_from_path(&module_root_path, &module_path),
                                dep_file.items.clone(),
                            );
                        }
                    }
                }
                info!(
                    count = resolved_modules_count,
                    "Resolved {} imported module{} for {}",
                    resolved_modules_count,
                    if resolved_modules_count == 1 { "" } else { "s" },
                    input.display()
                );
            }
        }
    }

    if is_typescript_input {
        if let Some(mut bucket) = module_bucket {
            if let NodeKind::File(root_file) = ast.kind_mut() {
                let original_items = std::mem::take(&mut root_file.items);
                merge_module_items(
                    &mut bucket,
                    module_ident_from_path(&module_root_path, input),
                    original_items,
                );

                let merged_items: Vec<Item> = bucket
                    .into_iter()
                    .map(|(_, (ident, items))| build_module_item(ident, items))
                    .collect();

                info!(
                    modules = merged_items.len(),
                    "Collected {} module{} for {}",
                    merged_items.len(),
                    if merged_items.len() == 1 { "" } else { "s" },
                    input.display()
                );

                root_file.items = merged_items;
            }
        }
    }

    let prep_options = TranspilePreparationOptions {
        run_const_eval: args.const_eval,
        save_intermediates: false,
        base_path,
    };
    if !is_cargo_manifest && !is_wit_input && !is_typescript_input {
        pipeline.prepare_for_transpile(&mut ast, &prep_options)?;
    }

    // Resolve target using shared backend mapping
    let target = crate::languages::backend::parse_language_target(&args.target)?;
    let wit_options = if matches!(target, crate::languages::backend::LanguageTarget::Wit) {
        Some(build_wit_options(input, package_info.as_ref(), args.single_world))
    } else {
        None
    };
    let result = transpile_node(&ast, target, args.type_defs, wit_options)?;

    // Write output
    std::fs::write(output, result.code).map_err(|e| CliError::Io(e))?;
    if let Some(defs) = result.type_defs {
        let mut defs_path = output.to_path_buf();
        if let Some(stem) = defs_path.file_stem().and_then(|s| s.to_str()) {
            defs_path.set_file_name(format!("{}.d.ts", stem));
        } else {
            defs_path.set_extension("d.ts");
        }
        std::fs::write(defs_path, defs).map_err(|e| CliError::Io(e))?;
    }
    info!("Generated: {}", output.display());

    Ok(())
}

// Local backend dispatcher (was previously in languages/backend.rs)
#[derive(Debug, Default, Clone)]
struct BackendTranspileResult {
    code: String,
    type_defs: Option<String>,
}

fn transpile_node(
    node: &Node,
    target: crate::languages::backend::LanguageTarget,
    emit_type_defs: bool,
    wit_options: Option<WitOptions>,
) -> Result<BackendTranspileResult> {
    use crate::CliError;
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
            Ok(BackendTranspileResult { code, type_defs: None })
        }
        crate::languages::backend::LanguageTarget::CSharp => {
            let serializer = CSharpSerializer;
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult { code, type_defs: None })
        }
        crate::languages::backend::LanguageTarget::Python => {
            let serializer = PythonSerializer;
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult { code, type_defs: None })
        }
        crate::languages::backend::LanguageTarget::Zig => {
            let serializer = ZigSerializer;
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult { code, type_defs: None })
        }
        crate::languages::backend::LanguageTarget::Rust => {
            let serializer = RustPrinter::new_with_rustfmt();
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult { code, type_defs: None })
        }
        crate::languages::backend::LanguageTarget::Wit => {
            let serializer = match wit_options {
                Some(options) => WitSerializer::with_options(options),
                None => WitSerializer::new(),
            };
            let code = serializer
                .serialize_node(node)
                .map_err(|e| CliError::Transpile(e.to_string()))?;
            Ok(BackendTranspileResult { code, type_defs: None })
        }
    }
}

// Utility functions
#[derive(Debug, Clone)]
struct PackageInfo {
    name: String,
    root: PathBuf,
}

fn find_package_info(input: &Path) -> Option<PackageInfo> {
    let mut current = input.parent();
    while let Some(dir) = current {
        let manifest = dir.join("package.json");
        if manifest.exists() {
            if let Ok(contents) = std::fs::read_to_string(&manifest) {
                if let Ok(json) = serde_json::from_str::<JsonValue>(&contents) {
                    if let Some(name) = json.get("name").and_then(|value| value.as_str()) {
                        return Some(PackageInfo {
                            name: name.to_string(),
                            root: dir.to_path_buf(),
                        });
                    }
                }
            }
        }
        current = dir.parent();
    }
    None
}

fn build_wit_options(
    input: &Path,
    package_info: Option<&PackageInfo>,
    single_world: bool,
) -> WitOptions {
    let namespace = package_info
        .map(|info| sanitize_wit_component(&info.name))
        .filter(|name| !name.is_empty())
        .or_else(|| {
            input
                .parent()
                .and_then(|dir| dir.file_name())
                .and_then(|os| os.to_str())
                .map(sanitize_wit_component)
                .filter(|name| !name.is_empty())
        })
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

fn sanitize_module_segment(raw: &str) -> String {
    let mut ident = String::new();
    for ch in raw.chars() {
        match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => ident.push(ch),
            '-' | '.' | '@' | ' ' => ident.push('_'),
            _ => ident.push('_'),
        }
    }
    if ident.is_empty() {
        ident.push('_');
    }
    if ident
        .chars()
        .next()
        .map(|ch| ch.is_ascii_digit())
        .unwrap_or(false)
    {
        ident.insert(0, '_');
    }
    ident
}

fn canonicalize_path(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

fn module_ident_from_path(module_root: &Path, module_path: &Path) -> Ident {
    let module = canonicalize_path(module_path);
    let relative = diff_paths(&module, module_root).unwrap_or(module.clone());
    let mut segments = Vec::new();
    let mut components = relative.components().peekable();

    while let Some(component) = components.next() {
        match component {
            Component::Normal(os_component) => {
                let is_last = components.peek().is_none();
                let raw = os_component.to_string_lossy();
                let segment = if is_last {
                    Path::new(raw.as_ref())
                        .file_stem()
                        .and_then(|stem| stem.to_str())
                        .map(sanitize_module_segment)
                        .unwrap_or_else(|| sanitize_module_segment(raw.as_ref()))
                } else {
                    sanitize_module_segment(raw.as_ref())
                };
                if !segment.is_empty() {
                    segments.push(segment);
                }
            }
            Component::ParentDir => segments.push("super".to_string()),
            Component::CurDir => {}
            Component::RootDir | Component::Prefix(_) => {}
        }
    }

    if segments.is_empty() {
        Ident::new("module")
    } else {
        Ident::new(segments.join("_"))
    }
}

fn merge_module_items(
    bucket: &mut BTreeMap<String, (Ident, Vec<Item>)>,
    ident: Ident,
    mut items: Vec<Item>,
) {
    let key = ident.as_str().to_string();
    let entry = bucket
        .entry(key)
        .or_insert_with(|| (ident.clone(), Vec::new()));
    entry.1.append(&mut items);
}

fn build_module_item(ident: Ident, items: Vec<Item>) -> Item {
    Item::from(AstModule {
        name: ident,
        items,
        visibility: Visibility::Public,
    })
}

fn determine_transpile_output_path(
    input: &Path,
    output: Option<&PathBuf>,
    target: &str,
) -> Result<PathBuf> {
    crate::languages::backend::resolve_output_path(input, output, target)
}

// Progress bar helper moved to commands::common
