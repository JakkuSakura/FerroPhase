mod attr;
mod expr;
mod item;
pub mod macros;
mod pat;
mod ty;

pub use ty::parse_type;

use fp_core::ast::*;
use fp_core::ast::{Ident, Locator, ParameterPath, ParameterPathSegment, Path};
use fp_core::bail;
use itertools::Itertools;

use eyre::{eyre, Context};
use quote::ToTokens;
use fp_core::diagnostics::{Diagnostic, DiagnosticLevel, DiagnosticManager};
use fp_core::error::Result;
use std::fs;
use std::path::{Path as FsPath, PathBuf};
use std::process::Command;
use std::sync::{Arc, OnceLock};
use syn::parse_str;

pub fn parse_ident(i: syn::Ident) -> Ident {
    Ident::new(i.to_string())
}
pub fn parse_path(p: syn::Path) -> Result<Path> {
    Ok(Path {
        segments: p
            .segments
            .into_iter()
            .map(|x| {
                let ident = parse_ident(x.ident);
                if matches!(
                    x.arguments,
                    syn::PathArguments::None
                        | syn::PathArguments::AngleBracketed(_)
                        | syn::PathArguments::Parenthesized(_)
                ) {
                    Ok(ident)
                } else {
                    bail!("Does not support path arguments: {:?}", x.arguments)
                }
            })
            .try_collect()?,
    })
}
#[allow(dead_code)]
fn parse_locator(p: syn::Path) -> Result<Locator> {
    if let Ok(path) = parse_path(p.clone()) {
        return Ok(Locator::path(path));
    }
    // Fallback to parameter path parsing without diagnostics context.
    // Callers that need diagnostics should use `RustParser::parse_locator`.
    let parser = RustParser::new();
    let path = parser.parse_parameter_path(p.clone())?;
    Ok(Locator::parameter_path(path))
}

fn parse_vis(v: syn::Visibility) -> Visibility {
    match v {
        syn::Visibility::Public(_) => Visibility::Public,
        syn::Visibility::Restricted(_) => Visibility::Public,
        syn::Visibility::Inherited => Visibility::Private,
    }
}
pub fn parse_module(m: syn::ItemMod) -> Result<Module> {
    RustParser::new().parse_module(m)
}
pub fn parse_value_fn(f: syn::ItemFn) -> Result<ValueFunction> {
    let parser = RustParser::new();
    parser.parse_value_fn(f)
}
const FRONTEND_CONTEXT: &str = "pipeline.frontend";

#[derive(Debug, Clone)]
pub struct RustParser {
    diagnostics: Arc<DiagnosticManager>,
    lossy_mode: bool,
}

impl RustParser {
    pub fn new() -> Self {
        let lossy_mode = detect_lossy_mode();
        Self::new_with_lossy(lossy_mode)
    }

    pub fn new_with_lossy(lossy_mode: bool) -> Self {
        Self {
            diagnostics: Arc::new(DiagnosticManager::new()),
            lossy_mode,
        }
    }

    pub fn diagnostics(&self) -> Arc<DiagnosticManager> {
        self.diagnostics.clone()
    }

    pub fn clear_diagnostics(&self) {
        self.diagnostics.clear();
    }

    fn record_diagnostic(&self, level: DiagnosticLevel, message: impl Into<String>) -> Diagnostic {
        let message = message.into();
        let diagnostic = match level {
            DiagnosticLevel::Error => Diagnostic::error(message),
            DiagnosticLevel::Warning => Diagnostic::warning(message),
            DiagnosticLevel::Info => Diagnostic::info(message),
        }
        .with_source_context(FRONTEND_CONTEXT.to_string());

        self.diagnostics.add_diagnostic(diagnostic.clone());
        diagnostic
    }

    fn push_error(&self, message: impl Into<String>) {
        self.record_diagnostic(DiagnosticLevel::Error, message);
    }

    pub(crate) fn error<T>(&self, message: impl Into<String>, fallback: T) -> Result<T> {
        self.push_error(message);
        Ok(fallback)
    }

    pub fn parse_file_recursively(&self, path: PathBuf) -> Result<File> {
        self.clear_diagnostics();
        let path = path
            .canonicalize()
            .with_context(|| format!("Could not find file: {}", path.display()))?;
        tracing::debug!("Parsing {}", path.display());

        if let Some(crate_root) = find_crate_root(path.as_path()) {
            match self.expand_with_cargo(crate_root.as_path(), path.as_path()) {
                Ok(expanded_file) => {
                    return match self.parse_file_content(path.clone(), expanded_file) {
                        Ok(file) => Ok(file),
                        Err(e) => self.error(
                            format!("Failed to parse expanded file {}: {}", path.display(), e),
                            File {
                                path: path.clone(),
                                items: Vec::new(),
                            },
                        ),
                    };
                }
                Err(err) => {
                    self.record_diagnostic(
                        DiagnosticLevel::Warning,
                        format!(
                            "Falling back to direct parse for {} (cargo expand failed: {})",
                            path.display(),
                            err
                        ),
                    );
                }
            }
        }

        let source = fs::read_to_string(&path)
            .with_context(|| format!("Could not read file: {}", path.display()))?;
        match syn::parse_file(&source) {
            Ok(syn_file) => match self.parse_file_content(path.clone(), syn_file) {
                Ok(file) => Ok(file),
                Err(e) => self.error(
                    format!("Failed to parse file {}: {}", path.display(), e),
                    File {
                        path: path.clone(),
                        items: Vec::new(),
                    },
                ),
            },
            Err(e) => self.error(
                format!("Failed to parse {} as file: {}", path.display(), e),
                File {
                    path: path.clone(),
                    items: Vec::new(),
                },
            ),
        }
    }
    pub fn parse_value(&self, code: syn::Expr) -> Result<Value> {
        self.expr_parser().parse_expr(code).map(Value::expr)
    }
    pub fn parse_expr(&self, code: syn::Expr) -> Result<Expr> {
        self.expr_parser().parse_expr(code)
    }

    // Parse an expression from raw tokens; supports semicolon-terminated macro
    // statements by parsing as a statement and extracting the expression or
    // macro invocation without lowering.
    pub fn parse_expr_tokens(
        &self,
        tokens: proc_macro2::TokenStream,
    ) -> Result<Expr> {
        if let Ok(expr) = syn::parse2::<syn::Expr>(tokens.clone()) {
            return self.parse_expr(expr);
        }

        if let Ok(stmt) = syn::parse2::<syn::Stmt>(tokens) {
            return match stmt {
                syn::Stmt::Expr(expr, _) => self.parse_expr(expr),
                syn::Stmt::Macro(raw_mac) => {
                    // Wrap as ExprMacro and record as AST macro (no lowering here)
                    let _expr_macro = syn::ExprMacro {
                        attrs: raw_mac.attrs.clone(),
                        mac: raw_mac.mac.clone(),
                    };
                    // Build macro invocation directly to avoid calling the private helper.
                    // Rebuild MacroInvocation inline
                    match crate::parser::parse_path(raw_mac.mac.path.clone()) {
                        Ok(path) => Ok(Expr::macro_invocation(MacroInvocation::new(
                            path,
                            match &raw_mac.mac.delimiter {
                                syn::MacroDelimiter::Paren(_) => MacroDelimiter::Parenthesis,
                                syn::MacroDelimiter::Brace(_) => MacroDelimiter::Brace,
                                syn::MacroDelimiter::Bracket(_) => MacroDelimiter::Bracket,
                            },
                            raw_mac.mac.tokens.to_string(),
                        ))),
                        Err(err) => self.error(
                            format!(
                                "failed to record macro invocation `{}`: {}",
                                raw_mac.mac.path.to_token_stream(),
                                err
                            ),
                            Expr::unit(),
                        ),
                    }
                }
                _ => self.error(
                    "unsupported statement form in parse_expr_tokens",
                    Expr::unit(),
                ),
            };
        }

        self.error("failed to parse tokens as expression", Expr::unit())
    }
    // Note: macros are recorded as AST (ExprKind::Macro) by the parser.
    // Lowering occurs later in the normalization stage.

    pub(crate) fn lower_expr_macro(&self, mac: syn::ExprMacro) -> Result<Expr> {
        self.expr_parser().lower_expr_macro(mac)
    }

    pub fn parse_block(&self, block: syn::Block) -> Result<ExprBlock> {
        self.expr_parser().parse_block(block)
    }

    pub fn parse_stmt(&self, stmt: syn::Stmt) -> Result<(BlockStmt, bool)> {
        self.expr_parser().parse_stmt(stmt)
    }

    pub fn parse_value_fn(&self, f: syn::ItemFn) -> Result<ValueFunction> {
        let sig = self.parse_fn_sig_internal(f.sig)?;
        let body = self.parse_block(*f.block)?;
        Ok(ValueFunction {
            sig,
            body: Expr::block(body).into(),
        })
    }

    pub fn parse_parameter_path(&self, p: syn::Path) -> Result<ParameterPath> {
        Ok(ParameterPath {
            segments: p
                .segments
                .into_iter()
                .map(|segment| {
                    let ident = parse_ident(segment.ident);
                    let args = match segment.arguments {
                        syn::PathArguments::AngleBracketed(a) => a
                            .args
                            .into_iter()
                            .map(|arg| match arg {
                                syn::GenericArgument::Type(t) => ty::parse_type(t),
                                syn::GenericArgument::Const(c) => {
                                    self.parse_expr(c).map(|expr| Ty::value(Value::expr(expr)))
                                }
                                other => self.error(
                                    format!("Does not support path arguments: {:?}", other),
                                    Ty::Any(TypeAny),
                                ),
                            })
                            .try_collect()?,
                        other => {
                            return self
                                .error(
                                    format!("Does not support path arguments: {:?}", other),
                                    Vec::<Ty>::new(),
                                )
                                .map(|args| ParameterPathSegment { ident, args });
                        }
                    };
                    Ok::<_, fp_core::Error>(ParameterPathSegment { ident, args })
                })
                .try_collect()?,
        })
    }

    pub fn parse_locator(&self, p: syn::Path) -> Result<Locator> {
        if let Ok(path) = parse_path(p.clone()) {
            return Ok(Locator::path(path));
        }
        let path = self.parse_parameter_path(p)?;
        Ok(Locator::parameter_path(path))
    }

    pub fn lossy_mode(&self) -> bool {
        self.lossy_mode
    }
    pub fn parse_item(&self, code: syn::Item) -> Result<Item> {
        self.parse_item_internal(code)
    }
    pub fn parse_items(&self, code: Vec<syn::Item>) -> Result<Vec<Item>> {
        code.into_iter()
            .map(|item| self.parse_item_internal(item))
            .try_collect()
    }
    pub fn parse_file_content(&self, path: PathBuf, code: syn::File) -> Result<File> {
        let items = code
            .items
            .into_iter()
            .map(|item| self.parse_item_internal(item))
            .try_collect()?;
        Ok(File { path, items })
    }

    pub fn parse_file(&mut self, source: &str, path: &std::path::Path) -> Result<File> {
        self.clear_diagnostics();
        let path_buf = path.to_path_buf();
        let syn_file = match syn::parse_file(source) {
            Ok(file) => file,
            Err(e) => {
                self.push_error(format!("Failed to parse {} as file: {}", path.display(), e));
                return Ok(File {
                    path: path_buf,
                    items: Vec::new(),
                });
            }
        };

        match self.parse_file_content(path_buf.clone(), syn_file) {
            Ok(file) => Ok(file),
            Err(e) => self.error(
                format!("Failed to lower file {}: {}", path.display(), e),
                File {
                    path: path_buf,
                    items: Vec::new(),
                },
            ),
        }
    }
    pub fn parse_module(&self, code: syn::ItemMod) -> Result<Module> {
        let items = if let Some((_, inner)) = code.content {
            inner
                .into_iter()
                .map(|item| self.parse_item_internal(item))
                .try_collect()?
        } else {
            Vec::new()
        };
        Ok(Module {
            name: parse_ident(code.ident),
            items,
            visibility: parse_vis(code.vis),
        })
    }
    pub fn parse_type(&self, code: syn::Type) -> Result<Ty> {
        ty::parse_type(code)
    }

    pub fn try_parse_as_file(&self, source: &str) -> Result<BExpr> {
        // Parse as a syn::File first, but be more permissive with errors
        let syn_file: syn::File =
            syn::parse_str(source).map_err(|e| eyre!("Failed to parse as file: {}", e))?;

        // Try to parse the file, but handle errors more gracefully for transpilation
        match self.parse_file_content(PathBuf::from("input.fp"), syn_file) {
            Ok(ast_file) => {
                // Find main function and const declarations
                let mut const_items = Vec::new();
                let mut main_body = None;

                for item in ast_file.items {
                    if let Some(func) = item.as_function() {
                        if func.name.name == "main" {
                            main_body = Some(func.body.clone());
                        }
                    } else {
                        // Keep const declarations and other items
                        const_items.push(BlockStmt::Item(Box::new(item)));
                    }
                }

                // If we found a main function, create a block with const items + main body
                if let Some(body) = main_body {
                    // Add the main body as the final expression
                    const_items.push(BlockStmt::Expr(BlockStmtExpr {
                        expr: body,
                        semicolon: None,
                    }));

                    Ok(Box::new(Expr::block(ExprBlock { stmts: const_items })))
                } else {
                    // No main function, create a minimal structure for transpilation
                    if const_items.is_empty() {
                        // Create an empty block for transpilation purposes
                        Ok(Box::new(Expr::block(ExprBlock { stmts: vec![] })))
                    } else {
                        // Just use all parsed items
                        Ok(Box::new(Expr::block(ExprBlock { stmts: const_items })))
                    }
                }
            }
            Err(e) => {
                // Log the original parsing error and propagate it
                tracing::error!("Failed to parse file content: {}", e);
                Err(e)
                    .with_context(|| "Failed to parse as file - unable to extract AST structure")?
            }
        }
    }

    pub fn try_parse_block_expression(&self, source: &str) -> Result<BExpr> {
        let wrapped_source = format!("{{\n{}\n}}", source);
        let syn_expr: syn::Expr = syn::parse_str(&wrapped_source)
            .map_err(|e| eyre!("Failed to parse as block: {}", e))?;

        let ast_expr = self
            .parse_expr(syn_expr)
            .map_err(|e| eyre!("Failed to convert to AST: {}", e))?;

        Ok(Box::new(ast_expr))
    }

    pub fn try_parse_simple_expression(&self, source: &str) -> Result<BExpr> {
        let syn_expr: syn::Expr =
            syn::parse_str(source).map_err(|e| eyre!("Failed to parse as expression: {}", e))?;

        let ast_expr = self
            .parse_expr(syn_expr)
            .map_err(|e| eyre!("Failed to convert to AST: {}", e))?;

        Ok(Box::new(ast_expr))
    }

    pub fn try_parse_structs_only(&self, source: &str) -> Result<BExpr> {
        // Try to parse individual items from the source, filtering out problematic ones
        let syn_file: syn::File =
            syn::parse_str(source).map_err(|e| eyre!("Failed to parse source: {}", e))?;

        let mut parsed_items = Vec::new();

        for item in syn_file.items {
            // Filter to only struct definitions and other safe items
            match item {
                syn::Item::Struct(_) | syn::Item::Enum(_) | syn::Item::Type(_) => {
                    let ast_item = self
                        .parse_item(item)
                        .with_context(|| "Failed to parse struct/enum/type item")?;
                    parsed_items.push(BlockStmt::Item(Box::new(ast_item)));
                }
                _ => {
                    // Skip other item types that might cause issues
                }
            }
        }

        Ok(Box::new(Expr::block(ExprBlock {
            stmts: parsed_items,
        })))
    }

    /// Parse FP-specific content that may contain structs with embedded functions
    /// and other FerroPhase-specific syntax extensions
    pub fn parse_fp_content(&self, content: &str) -> Result<Expr> {
        // First try to parse as items wrapped in a module
        let wrapped_content = format!("mod fp_block {{ {} }}", content);

        match syn::parse_str::<syn::File>(&wrapped_content) {
            Ok(file) => {
                // Extract the module content
                if let Some(syn::Item::Mod(module)) = file.items.into_iter().next() {
                    if let Some((_, items)) = module.content {
                        // Parse the items using the existing FP parser
                        let parsed_items = self.parse_items(items)?;

                        // Convert items to block statements
                        let stmts: Vec<BlockStmt> = parsed_items
                            .into_iter()
                            .map(|item| BlockStmt::Item(Box::new(item)))
                            .collect();

                        return Ok(Expr::block(ExprBlock { stmts }));
                    }
                }
            }
            Err(module_err) => {
                // If module parsing fails, try parsing as block statements or expressions
                let wrapped_content = format!("{{ {} }}", content);

                match syn::parse_str::<syn::Expr>(&wrapped_content) {
                    Ok(syn::Expr::Block(block_expr)) => {
                        return self.parse_block(block_expr.block).map(Expr::block);
                    }
                    Ok(other) => {
                        return self.parse_expr(other);
                    }
                    Err(expr_err) => {
                        // Both module and expression parsing failed - report both errors
                        self.push_error(format!(
                            "Failed to parse FP content as module: {}. Also failed as expression: {}. Content: {}",
                            module_err, expr_err, content
                        ));
                        return Ok(Expr::unit());
                    }
                }
            }
        }

        // If all parsing attempts fail, return a descriptive error placeholder
        self.push_error(format!("Failed to parse FP content: {}", content));
        Ok(Expr::unit())
    }
}

impl AstDeserializer for RustParser {
    fn deserialize_node(&self, code: &str) -> fp_core::error::Result<Node> {
        let code: syn::File = parse_str(code).map_err(|e| eyre!(e.to_string()))?;
        let path = PathBuf::from("__file__");
        Ok(self.parse_file_content(path, code).map(Node::file)?)
    }

    fn deserialize_expr(&self, code: &str) -> fp_core::error::Result<Expr> {
        let code: syn::Expr = parse_str(code).map_err(|e| eyre!(e.to_string()))?;
        Ok(self.parse_expr(code)?)
    }

    fn deserialize_item(&self, code: &str) -> fp_core::error::Result<Item> {
        let code: syn::Item = parse_str(code).map_err(|e| eyre!(e.to_string()))?;
        Ok(self.parse_item(code)?)
    }

    fn deserialize_file_load(&self, path: &std::path::Path) -> fp_core::error::Result<File> {
        Ok(self.parse_file_recursively(path.to_owned())?)
    }
    fn deserialize_type(&self, code: &str) -> fp_core::error::Result<Ty> {
        let code: syn::Type = parse_str(code).map_err(|e| eyre!(e.to_string()))?;
        Ok(self.parse_type(code)?)
    }
}

fn find_crate_root(path: &FsPath) -> Option<PathBuf> {
    let mut current = if path.is_file() {
        path.parent()?.to_path_buf()
    } else {
        path.to_path_buf()
    };

    loop {
        let manifest = current.join("Cargo.toml");
        if manifest.exists() {
            if manifest_has_package(&manifest).unwrap_or(false) {
                return Some(current);
            }
        }
        if !current.pop() {
            break;
        }
    }
    None
}

fn manifest_has_package(manifest: &FsPath) -> eyre::Result<bool> {
    let contents = fs::read_to_string(manifest)?;
    let parsed: toml::Value = toml::from_str(&contents)?;
    Ok(parsed
        .get("package")
        .and_then(|pkg| pkg.get("name"))
        .and_then(|name| name.as_str())
        .is_some())
}

#[derive(Clone, Debug)]
enum ExpandTarget {
    Lib,
    Bin(String),
}

impl RustParser {
    fn expand_with_cargo(
        &self,
        crate_root: &FsPath,
        source_path: &FsPath,
    ) -> eyre::Result<syn::File> {
        let manifest = crate_root.join("Cargo.toml");
        let package_name = read_package_name(manifest.as_path())?.ok_or_else(|| {
            eyre!(
                "manifest {} does not declare a [package]",
                manifest.display()
            )
        })?;

        let target = determine_expand_target(crate_root, source_path, &package_name);
        let expanded = run_cargo_expand(crate_root, &target)?;
        let syn_file = syn::parse_file(&expanded)?;
        Ok(syn_file)
    }
}

fn read_package_name(manifest: &FsPath) -> eyre::Result<Option<String>> {
    let contents = fs::read_to_string(manifest)?;
    let parsed: toml::Value = toml::from_str(&contents)?;
    Ok(parsed
        .get("package")
        .and_then(|pkg| pkg.get("name"))
        .and_then(|name| name.as_str())
        .map(|s| s.to_string()))
}

fn determine_expand_target(
    crate_root: &FsPath,
    source_path: &FsPath,
    package: &str,
) -> ExpandTarget {
    let relative = source_path.strip_prefix(crate_root).unwrap_or(source_path);
    if relative == FsPath::new("src/lib.rs") {
        ExpandTarget::Lib
    } else if relative == FsPath::new("src/main.rs") {
        ExpandTarget::Bin(package.to_string())
    } else if relative.starts_with(FsPath::new("src/bin")) {
        let name = relative
            .file_stem()
            .and_then(|stem| stem.to_str())
            .unwrap_or(package)
            .to_string();
        ExpandTarget::Bin(name)
    } else {
        ExpandTarget::Lib
    }
}

fn run_cargo_expand(crate_root: &FsPath, target: &ExpandTarget) -> eyre::Result<String> {
    let mut command = Command::new("cargo");
    command.arg("expand");
    match target {
        ExpandTarget::Lib => {}
        ExpandTarget::Bin(name) => {
            command.arg("--bin").arg(name);
        }
    }
    command.current_dir(crate_root);
    let output = command.output()?;
    if !output.status.success() {
        return Err(eyre!(
            "cargo expand failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    let stdout = String::from_utf8(output.stdout)?;
    Ok(stdout)
}

fn detect_lossy_mode() -> bool {
    static LOSSY: OnceLock<bool> = OnceLock::new();
    *LOSSY.get_or_init(|| {
        let env_true = |key: &str| {
            std::env::var(key).map(|val| {
                let trimmed = val.trim();
                !trimmed.is_empty() && !matches!(trimmed, "0" | "false" | "FALSE" | "False")
            })
        };

        if env_true("FERROPHASE_BOOTSTRAP").unwrap_or(false) {
            return true;
        }

        env_true("FERROPHASE_LOSSY").unwrap_or(false)
    })
}
