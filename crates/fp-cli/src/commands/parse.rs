use crate::{Result, cli::CliConfig, pipeline::Pipeline};
use clap::{ArgAction, Args, ValueEnum, ValueHint};
#[cfg(any(test, feature = "bootstrap"))]
use fp_core::ast::{File, Item, ItemKind, Module, Node};
use fp_core::pretty::{PrettyOptions, pretty};
use fp_typescript::frontend::TsParseMode;
use serde_json;
use std::fs;
use std::path::{Path, PathBuf};

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
        BlockStmt, Expr, ExprAssign, ExprAsync, ExprBinOp, ExprBlock, ExprCast, ExprClosure,
        ExprField, ExprIf, ExprIndex, ExprInvoke, ExprKind, ExprLet, ExprMatch, ExprParen,
        ExprRange, ExprReference, ExprSelect, ExprStruct, ExprStructural, ExprTry, ExprTuple,
        ExprUnOp, ItemKind, Value,
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
            ExprKind::Match(ExprMatch {
                scrutinee, cases, ..
            }) => {
                if let Some(expr) = scrutinee.as_mut() {
                    scrub_expr(expr.as_mut());
                }
                for case in cases {
                    scrub_expr(case.cond.as_mut());
                    if let Some(guard) = case.guard.as_mut() {
                        scrub_expr(guard.as_mut());
                    }
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
            ExprKind::Struct(ExprStruct { fields, update, .. }) => {
                for ExprField { value, .. } in fields.iter_mut() {
                    if let Some(v) = value.as_mut() {
                        scrub_expr(v);
                    }
                }
                if let Some(expr) = update.as_mut() {
                    scrub_expr(expr);
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
            ExprKind::Async(ExprAsync { expr: e }) => scrub_expr(e.as_mut()),
            ExprKind::For(for_expr) => {
                scrub_expr(for_expr.iter.as_mut());
                scrub_expr(for_expr.body.as_mut());
            }
            ExprKind::Closure(ExprClosure { body, .. }) => scrub_expr(body.as_mut()),
            ExprKind::Closured(inner) => scrub_expr(inner.expr.as_mut()),
            ExprKind::Paren(ExprParen { expr: e }) => scrub_expr(e.as_mut()),
            ExprKind::FormatString(_) => {}
            ExprKind::Quote(q) => scrub_block(&mut q.block),
            ExprKind::Splice(s) => scrub_expr(s.token.as_mut()),
            ExprKind::Item(item) => scrub_item(item.as_mut()),
            ExprKind::Value(_) => {}
            ExprKind::IntrinsicCall(call) => {
                for a in call.args.iter_mut() {
                    scrub_expr(a);
                }
                for kw in call.kwargs.iter_mut() {
                    scrub_expr(&mut kw.value);
                }
            }
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
