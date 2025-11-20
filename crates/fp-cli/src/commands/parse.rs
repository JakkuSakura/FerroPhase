use crate::{Result, cli::CliConfig, pipeline::Pipeline};
use fp_core::{
    ast::{File, Item, ItemKind, Module, Node},
    package::provider::{ModuleSource, PackageProvider},
    pretty::{PrettyOptions, pretty},
    workspace::WorkspaceDocument,
};
use fp_rust::{parser::RustParser, workspace::summarize_cargo_workspace};
use fp_typescript::{frontend::TsParseMode, package::TypeScriptPackageProvider};
use pathdiff::diff_paths;
use serde_json::{self, to_value};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use tracing::warn;

/// Remove un-serializable raw macro definition items (e.g., `macro_rules!`) from a Rust file AST.
///
/// During Stage 0 snapshotting we must not emit `AnyBox` payloads because they cannot be
/// deserialized later (by design). The Rust frontend currently represents macro definitions
/// as `ItemKind::Any(RawItemMacro)`. We conservatively drop those items from the snapshot
/// to ensure snapshots can be reloaded in Stage 1.
#[cfg(any(test, feature = "bootstrap"))]
fn prune_raw_item_macros(file: &mut File) {
    use fp_core::ast::ItemKind;

    // Recursively walk and prune at item level
    fn prune_items(items: &mut Vec<Item>) {
        // First, recurse into children to prune nested modules/impls
        for item in items.iter_mut() {
            match item.kind_mut() {
                ItemKind::Module(module) => {
                    prune_items(&mut module.items);
                }
                ItemKind::Impl(impl_block) => {
                    prune_items(&mut impl_block.items);
                }
                _ => {}
            }
        }

        // Then, filter out raw item macros
        items.retain(|item| match item.kind() {
            ItemKind::Any(any) => {
                // If this Any payload is a Rust RawItemMacro, drop it from the snapshot
                // to avoid serializing untyped AnyBox content.
                if any.downcast_ref::<fp_rust::RawItemMacro>().is_some() {
                    return false;
                }
                true
            }
            _ => true,
        });
    }

    prune_items(&mut file.items);
}

/// Replace any expression-level `AnyBox` nodes with a harmless unit value so that
/// the serialized snapshot contains only portable, deserializable nodes.
#[cfg(any(test, feature = "bootstrap"))]
fn scrub_any_expressions(file: &mut File) {
    use fp_core::ast::{
        BlockStmt, Expr, ExprAssign, ExprBinOp, ExprBlock, ExprCast, ExprClosure, ExprField,
        ExprIf, ExprIndex, ExprInvoke, ExprKind, ExprLet, ExprMatch, ExprParen, ExprRange,
        ExprReference, ExprSelect, ExprStruct, ExprStructural, ExprTry, ExprTuple, ExprUnOp,
        ItemKind, Value,
    };

    fn scrub_block(block: &mut ExprBlock) {
        for stmt in &mut block.stmts {
            match stmt {
                BlockStmt::Expr(expr_stmt) => scrub_expr(expr_stmt.expr.as_mut()),
                BlockStmt::Let(stmt_let) => {
                    if let Some(init) = stmt_let.init.as_mut() {
                        scrub_expr(init);
                    }
                    if let Some(diverge) = stmt_let.diverge.as_mut() {
                        scrub_expr(diverge);
                    }
                }
                BlockStmt::Item(item) => scrub_item(item.as_mut()),
                BlockStmt::Noop => {}
                BlockStmt::Any(_) => {
                    // Replace with Noop to avoid AnyBox in the snapshot
                    *stmt = BlockStmt::Noop;
                }
            }
        }
    }

    fn scrub_expr(expr: &mut Expr) {
        match expr.kind_mut() {
            ExprKind::Any(_) => {
                *expr = Expr::new(ExprKind::Value(Box::new(Value::unit())));
            }
            ExprKind::Block(block) => scrub_block(block),
            ExprKind::If(ExprIf { cond, then, elze }) => {
                scrub_expr(cond.as_mut());
                scrub_expr(then.as_mut());
                if let Some(e) = elze.as_mut() {
                    scrub_expr(e);
                }
            }
            ExprKind::Loop(inner) => scrub_expr(inner.body.as_mut()),
            ExprKind::While(inner) => {
                scrub_expr(inner.cond.as_mut());
                scrub_expr(inner.body.as_mut());
            }
            ExprKind::Match(ExprMatch { cases }) => {
                for case in cases {
                    scrub_expr(case.cond.as_mut());
                    scrub_expr(case.body.as_mut());
                }
            }
            ExprKind::Let(ExprLet { expr: e, .. }) => scrub_expr(e.as_mut()),
            ExprKind::Assign(ExprAssign { target, value }) => {
                scrub_expr(target.as_mut());
                scrub_expr(value.as_mut());
            }
            ExprKind::Cast(ExprCast { expr: e, .. }) => scrub_expr(e.as_mut()),
            ExprKind::Invoke(ExprInvoke { target, args }) => {
                match target {
                    fp_core::ast::ExprInvokeTarget::Expr(inner) => scrub_expr(inner.as_mut()),
                    fp_core::ast::ExprInvokeTarget::Method(ExprSelect { obj, .. }) => {
                        scrub_expr(obj.as_mut())
                    }
                    fp_core::ast::ExprInvokeTarget::Closure(clos) => scrub_expr(clos.body.as_mut()),
                    _ => {}
                }
                for arg in args.iter_mut() {
                    scrub_expr(arg);
                }
            }
            ExprKind::Await(inner) => scrub_expr(inner.base.as_mut()),
            ExprKind::Select(ExprSelect { obj, .. }) => scrub_expr(obj.as_mut()),
            ExprKind::Struct(ExprStruct { fields, name: _ }) => {
                for ExprField { value, .. } in fields.iter_mut() {
                    if let Some(v) = value.as_mut() {
                        scrub_expr(v);
                    }
                }
            }
            ExprKind::Structural(ExprStructural { fields }) => {
                for ExprField { value, .. } in fields.iter_mut() {
                    if let Some(v) = value.as_mut() {
                        scrub_expr(v);
                    }
                }
            }
            ExprKind::Array(arr) => {
                for v in arr.values.iter_mut() {
                    scrub_expr(v);
                }
            }
            ExprKind::ArrayRepeat(rep) => {
                scrub_expr(rep.elem.as_mut());
                scrub_expr(rep.len.as_mut());
            }
            ExprKind::Tuple(ExprTuple { values }) => {
                for v in values.iter_mut() {
                    scrub_expr(v);
                }
            }
            ExprKind::BinOp(ExprBinOp { lhs, rhs, .. }) => {
                scrub_expr(lhs.as_mut());
                scrub_expr(rhs.as_mut());
            }
            ExprKind::UnOp(ExprUnOp { val, .. }) => scrub_expr(val.as_mut()),
            ExprKind::Reference(ExprReference { referee, .. }) => scrub_expr(referee.as_mut()),
            ExprKind::Dereference(inner) => scrub_expr(inner.referee.as_mut()),
            ExprKind::Index(ExprIndex { obj, index }) => {
                scrub_expr(obj.as_mut());
                scrub_expr(index.as_mut());
            }
            ExprKind::Splat(s) => scrub_expr(s.iter.as_mut()),
            ExprKind::SplatDict(s) => scrub_expr(s.dict.as_mut()),
            ExprKind::Try(ExprTry { expr: e }) => scrub_expr(e.as_mut()),
            ExprKind::Closure(ExprClosure { body, .. }) => scrub_expr(body.as_mut()),
            ExprKind::Closured(inner) => scrub_expr(inner.expr.as_mut()),
            ExprKind::Paren(ExprParen { expr: e }) => scrub_expr(e.as_mut()),
            ExprKind::FormatString(fmt) => {
                for a in fmt.args.iter_mut() {
                    scrub_expr(a);
                }
                for kw in fmt.kwargs.iter_mut() {
                    scrub_expr(&mut kw.value);
                }
            }
            ExprKind::Quote(q) => scrub_block(&mut q.block),
            ExprKind::Splice(s) => scrub_expr(s.token.as_mut()),
            ExprKind::Item(item) => scrub_item(item.as_mut()),
            ExprKind::Value(_) => {}
            ExprKind::IntrinsicCall(call) => match &mut call.payload {
                fp_core::intrinsics::IntrinsicCallPayload::Args { args } => {
                    for a in args.iter_mut() {
                        scrub_expr(a);
                    }
                }
                fp_core::intrinsics::IntrinsicCallPayload::Format { template } => {
                    for a in template.args.iter_mut() {
                        scrub_expr(a);
                    }
                    for kw in template.kwargs.iter_mut() {
                        scrub_expr(&mut kw.value);
                    }
                }
            },
            ExprKind::IntrinsicContainer(coll) => match coll {
                fp_core::ast::ExprIntrinsicContainer::VecElements { elements } => {
                    for e in elements.iter_mut() {
                        scrub_expr(e);
                    }
                }
                fp_core::ast::ExprIntrinsicContainer::VecRepeat { elem, len } => {
                    scrub_expr(elem.as_mut());
                    scrub_expr(len.as_mut());
                }
                fp_core::ast::ExprIntrinsicContainer::HashMapEntries { entries } => {
                    for entry in entries.iter_mut() {
                        scrub_expr(&mut entry.key);
                        scrub_expr(&mut entry.value);
                    }
                }
            },
            ExprKind::Range(ExprRange {
                start, end, step, ..
            }) => {
                if let Some(s) = start.as_mut() {
                    scrub_expr(s);
                }
                if let Some(e) = end.as_mut() {
                    scrub_expr(e);
                }
                if let Some(st) = step.as_mut() {
                    scrub_expr(st);
                }
            }
            ExprKind::Macro(_) | ExprKind::Locator(_) | ExprKind::Id(_) => {}
        }
    }

    fn scrub_item(item: &mut Item) {
        match item.kind_mut() {
            ItemKind::Module(m) => {
                for it in m.items.iter_mut() {
                    scrub_item(it);
                }
            }
            ItemKind::Impl(imp) => {
                for it in imp.items.iter_mut() {
                    scrub_item(it);
                }
            }
            ItemKind::DefFunction(fun) => scrub_expr(fun.body.as_mut()),
            ItemKind::DefConst(dc) => scrub_expr(dc.value.as_mut()),
            ItemKind::DefStatic(ds) => scrub_expr(ds.value.as_mut()),
            ItemKind::Expr(e) => scrub_expr(e),
            ItemKind::Any(_) => {
                // Leave other Any items in place for now (pruning of macro defs handled separately).
            }
            _ => {}
        }
    }

    // Operate directly on file items for clarity
    for item in file.items.iter_mut() {
        scrub_item(item);
    }
}

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
    /// Path to persist the parsed AST snapshot as JSON
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
        pipeline.set_typescript_parse_mode(args.parse_mode);
        return parse_expression(&mut pipeline, expr, args.snapshot.take());
    }

    if args.files.is_empty() {
        return Err(crate::CliError::Compilation(
            "Must specify either --expr or at least one path".to_string(),
        ));
    }

    let mut pipeline = Pipeline::new();
    pipeline.set_typescript_parse_mode(args.parse_mode);
    let mut snapshot = args.snapshot.take();
    for (index, path) in args.files.into_iter().enumerate() {
        let snapshot_for_file = if index == 0 { snapshot.take() } else { None };
        parse_path(
            &path,
            &mut pipeline,
            args.parse_mode,
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
    if is_cargo_manifest(path) {
        if resolve_imports {
            return Err(crate::CliError::Compilation(
                "--snapshot of a Cargo workspace does not support import resolution".to_string(),
            ));
        }
        parse_cargo_workspace_manifest(path, snapshot)
    } else if is_tsconfig(path) {
        if snapshot.is_some() {
            return Err(crate::CliError::Compilation(
                "--snapshot is only supported for direct source files".to_string(),
            ));
        }
        parse_tsconfig(path, pipeline, mode)
    } else if is_typescript_package_manifest(path) {
        if snapshot.is_some() {
            return Err(crate::CliError::Compilation(
                "--snapshot is only supported for direct source files".to_string(),
            ));
        }
        parse_typescript_package(path, pipeline, mode)
    } else {
        parse_file(path, pipeline, mode, resolve_imports, snapshot)
    }
}

fn is_typescript_package_manifest(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .map(|name| name == "package.json")
        .unwrap_or(false)
}

fn is_cargo_manifest(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .map(|name| name.eq_ignore_ascii_case("Cargo.toml"))
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

fn parse_cargo_workspace_manifest(path: &Path, snapshot: Option<PathBuf>) -> Result<()> {
    let mut document = summarize_cargo_workspace(path).map_err(|err| {
        crate::CliError::Compilation(format!(
            "Failed to parse Cargo workspace {}: {err}",
            path.display()
        ))
    })?;

    let workspace_root = path
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));

    if let Some(snapshot_path) = snapshot.as_ref() {
        let module_dir = snapshot_path.with_extension("modules");
        if let Some(parent) = module_dir.parent() {
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
        fs::create_dir_all(&module_dir).map_err(|err| {
            crate::CliError::Io(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!(
                    "Failed to create module snapshot directory {}: {err}",
                    module_dir.display()
                ),
            ))
        })?;

        let snapshot_root = snapshot_path
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."));

        let mut parser = RustParser::new();
        let mut module_cache: HashMap<PathBuf, Vec<Item>> = HashMap::new();

        for package in &mut document.packages {
            for module in &mut package.modules {
                let module_source = PathBuf::from(&module.path);
                let absolute_path = if module_source.is_absolute() {
                    module_source
                } else {
                    workspace_root.join(&module_source)
                };

                let source = fs::read_to_string(&absolute_path).map_err(|err| {
                    crate::CliError::Io(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!("Failed to read module {}: {err}", absolute_path.display()),
                    ))
                })?;

                let mut file = parser.parse_file(&source, &absolute_path).map_err(|err| {
                    crate::CliError::Compilation(format!(
                        "Failed to parse module {}: {err}",
                        absolute_path.display()
                    ))
                })?;

                expand_rust_modules(&mut parser, &mut file, &absolute_path, &mut module_cache)?;
                let module_node = Node::file(file);
                let module_name = sanitize_module_filename(&module.id);
                let module_snapshot_path = module_dir.join(format!("{module_name}.ast.json"));
                write_node_snapshot(&module_snapshot_path, &module_node)?;

                let relative_snapshot = module_snapshot_path
                    .strip_prefix(&snapshot_root)
                    .unwrap_or(&module_snapshot_path)
                    .to_string_lossy()
                    .replace('\\', "/");
                module.snapshot = Some(relative_snapshot);
                let embedded_ast = to_value(&module_node).map_err(|err| {
                    crate::CliError::Compilation(format!(
                        "Failed to serialize module AST for {}: {err}",
                        absolute_path.display()
                    ))
                })?;
                module.ast = Some(embedded_ast);
            }
        }
    }

    let node = fp_core::ast::Node::workspace(document.clone());

    let print_ast = snapshot.is_none();
    persist_snapshot(&node, snapshot.as_deref())?;

    if print_ast {
        print_workspace_summary(path, &document);
    }

    Ok(())
}

fn print_workspace_summary(manifest: &Path, document: &WorkspaceDocument) {
    println!("Workspace {}:", manifest.display());
    if document.packages.is_empty() {
        println!("  (no packages discovered)");
        return;
    }
    for package in &document.packages {
        let version = package
            .version
            .as_deref()
            .map(|v| format!(" {}", v))
            .unwrap_or_default();
        println!("  - {}{}", package.name, version);
        println!("    manifest: {}", package.manifest_path);
        println!("    root: {}", package.root);
        if !package.modules.is_empty() {
            println!("    modules:");
            for module in &package.modules {
                println!("      * {} ({})", module.id, module.path);
            }
        }
        if !package.features.is_empty() {
            println!("    features: {}", package.features.join(", "));
        }
        if !package.dependencies.is_empty() {
            let rendered_deps = package
                .dependencies
                .iter()
                .map(|dep| {
                    dep.kind
                        .as_deref()
                        .map(|kind| format!("{} ({kind})", dep.name))
                        .unwrap_or_else(|| dep.name.clone())
                })
                .collect::<Vec<_>>()
                .join(", ");
            println!("    dependencies: {}", rendered_deps);
        }
    }
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

fn write_node_snapshot(path: &Path, node: &Node) -> Result<()> {
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
    let json = serde_json::to_string_pretty(node).map_err(|err| {
        crate::CliError::Compilation(format!("Failed to serialize AST to JSON: {err}"))
    })?;
    fs::write(path, json).map_err(|err| {
        crate::CliError::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("Failed to write snapshot {}: {err}", path.display()),
        ))
    })?;
    Ok(())
}

fn sanitize_module_filename(id: &str) -> String {
    let mut name = String::with_capacity(id.len());
    for ch in id.chars() {
        if ch.is_ascii_alphanumeric() {
            name.push(ch);
        } else {
            name.push('_');
        }
    }
    if name.is_empty() {
        name.push_str("module");
    }
    name
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

fn expand_rust_modules(
    parser: &mut RustParser,
    file: &mut File,
    file_path: &Path,
    cache: &mut HashMap<PathBuf, Vec<Item>>,
) -> Result<()> {
    let canonical = file_path
        .canonicalize()
        .unwrap_or_else(|_| file_path.to_path_buf());
    let base_dir = canonical
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));
    expand_item_chunk(parser, &mut file.items, &base_dir, cache)
}

fn expand_item_chunk(
    parser: &mut RustParser,
    items: &mut Vec<Item>,
    base_dir: &Path,
    cache: &mut HashMap<PathBuf, Vec<Item>>,
) -> Result<()> {
    for item in items.iter_mut() {
        if let ItemKind::Module(module) = item.kind_mut() {
            expand_module(parser, module, base_dir, cache)?;
        }
    }
    Ok(())
}

fn expand_module(
    parser: &mut RustParser,
    module: &mut Module,
    base_dir: &Path,
    cache: &mut HashMap<PathBuf, Vec<Item>>,
) -> Result<()> {
    if module.items.is_empty() {
        if let Some(module_path) = resolve_module_path(base_dir, module.name.as_str()) {
            let canonical = module_path
                .canonicalize()
                .unwrap_or_else(|_| module_path.clone());

            if let Some(cached) = cache.get(&canonical) {
                module.items = cached.clone();
                return Ok(());
            }

            let source = fs::read_to_string(&module_path).map_err(|err| {
                crate::CliError::Io(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Failed to read module {}: {err}", module_path.display()),
                ))
            })?;

            let mut file = parser.parse_file(&source, &canonical).map_err(|err| {
                crate::CliError::Compilation(format!(
                    "Failed to parse module {}: {err}",
                    canonical.display()
                ))
            })?;

            let next_base = canonical
                .parent()
                .map(Path::to_path_buf)
                .unwrap_or_else(|| base_dir.to_path_buf());

            expand_item_chunk(parser, &mut file.items, &next_base, cache)?;
            cache.insert(canonical, file.items.clone());
            module.items = file.items;
            return Ok(());
        } else {
            warn!(
                module = %module.name.as_str(),
                base = %base_dir.display(),
                "Rust module declaration has no corresponding file"
            );
            return Ok(());
        }
    }

    expand_item_chunk(parser, &mut module.items, base_dir, cache)
}

fn resolve_module_path(base_dir: &Path, module_name: &str) -> Option<PathBuf> {
    let direct = base_dir.join(format!("{module_name}.rs"));
    if direct.exists() {
        return Some(direct);
    }
    let nested = base_dir.join(module_name).join("mod.rs");
    if nested.exists() {
        return Some(nested);
    }
    None
}
