use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use fp_core::ast::{
    Expr, ExprBlock, File, FunctionParam, FunctionSignature, Ident, Item, ItemDeclConst,
    ItemDefConst, ItemDefFunction, ItemImport, ItemImportGroup, ItemImportPath, ItemImportRename,
    ItemImportTree, Node, NodeKind, Ty, Value, Visibility,
};
use fp_core::diagnostics::{Diagnostic, DiagnosticManager};
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::frontend::{FrontendResult, LanguageFrontend};
use swc_common::input::StringInput;
use swc_common::{sync::Lrc, FileName, SourceMap, DUMMY_SP};
use swc_ecma_ast::EsVersion;
use swc_ecma_ast::{
    Decl, ExportAll, ExportDecl, ExportSpecifier, Expr as TsExpr, FnDecl, ImportDecl,
    ImportSpecifier, Lit, Module, ModuleDecl, ModuleExportName, ModuleItem, NamedExport, Param,
    Pat, VarDecl, VarDeclKind,
};
use swc_ecma_parser::error::Error as SwcError;
use swc_ecma_parser::lexer::Lexer;
use swc_ecma_parser::{Parser, Syntax, TsSyntax};

use crate::ts::serializer::TypeScriptSerializer;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TsParseMode {
    Strict,
    Loose,
}

const LANGUAGE_KEY: &str = "typescript";
const EXTENSIONS: &[&str] = &["ts", "tsx"];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportReferenceKind {
    Import,
    ReExportAll,
    ReExportNamed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportReference {
    pub spec: String,
    pub is_type_only: bool,
    pub kind: ImportReferenceKind,
}

pub struct TypeScriptFrontend {
    serializer: Arc<TypeScriptSerializer>,
    parse_mode: RwLock<TsParseMode>,
}

impl TypeScriptFrontend {
    pub fn new(parse_mode: TsParseMode) -> Self {
        Self {
            serializer: Arc::new(TypeScriptSerializer::new(false)),
            parse_mode: RwLock::new(parse_mode),
        }
    }

    fn file_name(path: Option<&Path>) -> FileName {
        match path {
            Some(path) => FileName::Real(path.to_path_buf()),
            None => FileName::Custom("<typescript>".into()),
        }
    }

    fn file_path(path: Option<&Path>) -> PathBuf {
        path.map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("<typescript>"))
    }

    pub fn set_parse_mode(&self, mode: TsParseMode) {
        *self.parse_mode.write().unwrap() = mode;
    }

    pub fn parse_mode(&self) -> TsParseMode {
        *self.parse_mode.read().unwrap()
    }
}

impl LanguageFrontend for TypeScriptFrontend {
    fn language(&self) -> &'static str {
        LANGUAGE_KEY
    }

    fn extensions(&self) -> &'static [&'static str] {
        EXTENSIONS
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let mode = self.parse_mode();

        let (module_ast, parse_errors) = parse_module_internal(source, path);

        if !parse_errors.is_empty() {
            for err in parse_errors {
                let diagnostic = if matches!(mode, TsParseMode::Strict) {
                    Diagnostic::error(format!("{err:?}"))
                } else {
                    Diagnostic::warning(format!("{err:?}"))
                }
                .with_source_context("typescript frontend");
                diagnostics.add_diagnostic(diagnostic);
            }
            if matches!(mode, TsParseMode::Strict) {
                return Err(CoreError::from("TypeScript parse failed"));
            }
        }

        let items = lower_module(&module_ast);

        let file = File {
            path: Self::file_path(path),
            items,
        };
        let node = Node::from(NodeKind::File(file.clone()));

        Ok(FrontendResult {
            last: node.clone(),
            ast: node,
            serializer: self.serializer.clone(),
            intrinsic_normalizer: None,
            snapshot: None,
            diagnostics,
        })
    }
}

pub fn collect_import_references(source: &str, path: Option<&Path>) -> Vec<ImportReference> {
    let (module, _) = parse_module_internal(source, path);
    extract_import_references(&module)
}

fn parse_module_internal(source: &str, path: Option<&Path>) -> (Module, Vec<SwcError>) {
    let tsx = path
        .and_then(|p| p.extension().and_then(|ext| ext.to_str()))
        .map(|ext| ext.eq_ignore_ascii_case("tsx"))
        .unwrap_or(false);

    let cm: Lrc<SourceMap> = Default::default();
    let fm = cm.new_source_file(
        TypeScriptFrontend::file_name(path).into(),
        source.to_string(),
    );
    let syntax = Syntax::Typescript(TsSyntax {
        tsx,
        decorators: true,
        dts: false,
        no_early_errors: false,
        disallow_ambiguous_jsx_like: false,
    });
    let lexer = Lexer::new(syntax, EsVersion::EsNext, StringInput::from(&*fm), None);
    let mut parser = Parser::new_from(lexer);

    let module = parser.parse_module();
    let mut parse_errors = parser.take_errors();
    let module_ast = match module {
        Ok(module_ast) => module_ast,
        Err(err) => {
            parse_errors.push(err);
            Module {
                span: DUMMY_SP,
                body: Vec::new(),
                shebang: None,
            }
        }
    };

    (module_ast, parse_errors)
}

fn extract_import_references(module: &Module) -> Vec<ImportReference> {
    module
        .body
        .iter()
        .filter_map(|item| match item {
            ModuleItem::ModuleDecl(ModuleDecl::Import(import)) => Some(ImportReference {
                spec: import.src.value.to_string(),
                is_type_only: !import_has_runtime(import),
                kind: ImportReferenceKind::Import,
            }),
            ModuleItem::ModuleDecl(ModuleDecl::ExportAll(export)) => Some(ImportReference {
                spec: export.src.value.to_string(),
                is_type_only: export.type_only,
                kind: ImportReferenceKind::ReExportAll,
            }),
            ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(named)) => {
                named.src.as_ref().map(|src| ImportReference {
                    spec: src.value.to_string(),
                    is_type_only: named.type_only,
                    kind: ImportReferenceKind::ReExportNamed,
                })
            }
            _ => None,
        })
        .collect()
}

fn import_has_runtime(import: &ImportDecl) -> bool {
    if import.type_only {
        return false;
    }
    if import.specifiers.is_empty() {
        return true;
    }
    import.specifiers.iter().any(|spec| match spec {
        ImportSpecifier::Named(named) => !named.is_type_only,
        _ => true,
    })
}

fn lower_module(module: &Module) -> Vec<Item> {
    let mut items = Vec::new();
    for item in &module.body {
        match item {
            ModuleItem::ModuleDecl(decl) => items.extend(lower_module_decl(decl)),
            ModuleItem::Stmt(stmt) => {
                if let swc_ecma_ast::Stmt::Decl(decl) = stmt {
                    items.extend(lower_decl(decl, Visibility::Inherited));
                }
            }
        }
    }
    items
}

fn lower_module_decl(decl: &ModuleDecl) -> Vec<Item> {
    match decl {
        ModuleDecl::Import(import) => lower_import(import),
        ModuleDecl::ExportDecl(export) => lower_export_decl(export),
        ModuleDecl::ExportAll(export) => lower_export_all(export),
        ModuleDecl::ExportNamed(named) => lower_named_export(named),
        ModuleDecl::ExportDefaultDecl(default_decl) => lower_export_default_decl(default_decl),
        ModuleDecl::ExportDefaultExpr(_) => Vec::new(),
        ModuleDecl::TsImportEquals(_)
        | ModuleDecl::TsExportAssignment(_)
        | ModuleDecl::TsNamespaceExport(_) => Vec::new(),
    }
}

fn lower_import(import: &ImportDecl) -> Vec<Item> {
    if import.type_only {
        return Vec::new();
    }

    let mut path = ItemImportPath::new();
    let module_spec = import.src.value.to_string();
    for segment in module_spec_to_segments(&module_spec) {
        match segment {
            ModuleSegment::SelfMod => path.push(ItemImportTree::SelfMod),
            ModuleSegment::SuperMod => path.push(ItemImportTree::SuperMod),
            ModuleSegment::Root => path.push(ItemImportTree::Root),
            ModuleSegment::Ident(ident) => path.push(ItemImportTree::Ident(ident)),
        }
    }

    let mut group = ItemImportGroup::new();

    for spec in &import.specifiers {
        match spec {
            ImportSpecifier::Named(named) => {
                if named.is_type_only {
                    continue;
                }
                let local = sanitize_ident(&named.local.sym.to_string());
                if let Some(imported) = &named.imported {
                    let from = module_export_name_to_ident(imported);
                    if from.as_str() == local {
                        group.push(ItemImportTree::Ident(Ident::new(local)));
                    } else {
                        group.push(ItemImportTree::Rename(ItemImportRename {
                            from,
                            to: Ident::new(local),
                        }));
                    }
                } else {
                    group.push(ItemImportTree::Ident(Ident::new(local)));
                }
            }
            ImportSpecifier::Default(default_spec) => {
                let local = sanitize_ident(&default_spec.local.sym.to_string());
                group.push(ItemImportTree::Rename(ItemImportRename {
                    from: Ident::new("default"),
                    to: Ident::new(local),
                }));
            }
            ImportSpecifier::Namespace(namespace_spec) => {
                let local = sanitize_ident(&namespace_spec.local.sym.to_string());
                group.push(ItemImportTree::Rename(ItemImportRename {
                    from: Ident::new("*"),
                    to: Ident::new(local),
                }));
            }
        }
    }

    if !group.items.is_empty() {
        path.push(ItemImportTree::Group(group));
    }

    if path.segments.is_empty() {
        return Vec::new();
    }

    vec![Item::from(ItemImport {
        visibility: Visibility::Inherited,
        tree: ItemImportTree::Path(path),
    })]
}

fn lower_export_decl(export: &ExportDecl) -> Vec<Item> {
    lower_decl(&export.decl, Visibility::Public)
}

fn lower_export_all(export: &ExportAll) -> Vec<Item> {
    if export.type_only {
        return Vec::new();
    }

    let mut path = ItemImportPath::new();
    let module_spec = export.src.value.to_string();
    for segment in module_spec_to_segments(&module_spec) {
        match segment {
            ModuleSegment::SelfMod => path.push(ItemImportTree::SelfMod),
            ModuleSegment::SuperMod => path.push(ItemImportTree::SuperMod),
            ModuleSegment::Root => path.push(ItemImportTree::Root),
            ModuleSegment::Ident(ident) => path.push(ItemImportTree::Ident(ident)),
        }
    }
    path.push(ItemImportTree::Glob);

    vec![Item::from(ItemImport {
        visibility: Visibility::Public,
        tree: ItemImportTree::Path(path),
    })]
}

fn lower_named_export(export: &NamedExport) -> Vec<Item> {
    if export.type_only {
        return Vec::new();
    }

    let mut items = Vec::new();

    if let Some(src) = &export.src {
        let mut path = ItemImportPath::new();
        let module_spec = src.value.to_string();
        for segment in module_spec_to_segments(&module_spec) {
            match segment {
                ModuleSegment::SelfMod => path.push(ItemImportTree::SelfMod),
                ModuleSegment::SuperMod => path.push(ItemImportTree::SuperMod),
                ModuleSegment::Root => path.push(ItemImportTree::Root),
                ModuleSegment::Ident(ident) => path.push(ItemImportTree::Ident(ident)),
            }
        }

        let mut group = ItemImportGroup::new();
        for spec in &export.specifiers {
            match spec {
                ExportSpecifier::Named(named) => {
                    if named.is_type_only {
                        continue;
                    }
                    let from = module_export_name_to_ident(&named.orig);
                    let to = named
                        .exported
                        .as_ref()
                        .map(module_export_name_to_ident)
                        .unwrap_or_else(|| from.clone());
                    if from == to {
                        group.push(ItemImportTree::Ident(from));
                    } else {
                        group.push(ItemImportTree::Rename(ItemImportRename { from, to }));
                    }
                }
                ExportSpecifier::Namespace(namespace) => {
                    let to = module_export_name_to_ident(&namespace.name);
                    group.push(ItemImportTree::Rename(ItemImportRename {
                        from: Ident::new("*"),
                        to,
                    }));
                }
                ExportSpecifier::Default(default_spec) => {
                    group.push(ItemImportTree::Rename(ItemImportRename {
                        from: Ident::new("default"),
                        to: Ident::new(sanitize_ident(&default_spec.exported.sym.to_string())),
                    }));
                }
            }
        }

        if !group.items.is_empty() {
            path.push(ItemImportTree::Group(group));
        }

        if !path.segments.is_empty() {
            items.push(Item::from(ItemImport {
                visibility: Visibility::Public,
                tree: ItemImportTree::Path(path),
            }));
        }
    }

    items
}

fn lower_export_default_decl(default_decl: &swc_ecma_ast::ExportDefaultDecl) -> Vec<Item> {
    match &default_decl.decl {
        swc_ecma_ast::DefaultDecl::Fn(fn_expr) => {
            if let Some(identifier) = &fn_expr.ident {
                let fn_decl = FnDecl {
                    declare: false,
                    ident: identifier.clone(),
                    function: fn_expr.function.clone(),
                };
                lower_fn_decl(&fn_decl, Visibility::Public).map_or_else(Vec::new, |item| vec![item])
            } else {
                Vec::new()
            }
        }
        swc_ecma_ast::DefaultDecl::Class(_) | swc_ecma_ast::DefaultDecl::TsInterfaceDecl(_) => {
            Vec::new()
        }
    }
}

fn lower_decl(decl: &Decl, visibility: Visibility) -> Vec<Item> {
    match decl {
        Decl::Fn(fn_decl) => lower_fn_decl(fn_decl, visibility).into_iter().collect(),
        Decl::Var(var_decl) => lower_var_decl(var_decl, visibility),
        Decl::TsTypeAlias(_)
        | Decl::Class(_)
        | Decl::Using(_)
        | Decl::TsInterface(_)
        | Decl::TsEnum(_)
        | Decl::TsModule(_) => Vec::new(),
    }
}

fn lower_fn_decl(fn_decl: &FnDecl, visibility: Visibility) -> Option<Item> {
    let name = sanitize_ident(&fn_decl.ident.sym.to_string());
    if name.is_empty() {
        return None;
    }
    let name_ident = Ident::new(name);

    let mut sig = FunctionSignature::unit();
    sig.name = Some(name_ident.clone());
    sig.params = lower_function_params(&fn_decl.function.params);

    let body_expr = fn_decl
        .function
        .body
        .as_ref()
        .map(lower_function_body)
        .unwrap_or_else(Expr::unit);

    Some(Item::from(ItemDefFunction {
        ty_annotation: None,
        attrs: Vec::new(),
        name: name_ident,
        ty: None,
        sig,
        body: Box::new(body_expr),
        visibility,
    }))
}

fn lower_function_params(params: &[Param]) -> Vec<FunctionParam> {
    params
        .iter()
        .enumerate()
        .map(|(idx, param)| {
            let fallback = format!("arg{idx}");
            let ident = pat_to_ident(&param.pat).unwrap_or_else(|| Ident::new(fallback));
            FunctionParam::new(ident, Ty::any())
        })
        .collect()
}

fn lower_function_body(_body: &swc_ecma_ast::BlockStmt) -> Expr {
    Expr::block(ExprBlock::new())
}

fn lower_var_decl(var_decl: &VarDecl, visibility: Visibility) -> Vec<Item> {
    let mut items = Vec::new();
    for declarator in &var_decl.decls {
        let Some(name_ident) = pat_to_ident(&declarator.name) else {
            continue;
        };
        let init_expr = declarator
            .init
            .as_ref()
            .map(|expr| lower_expr(expr.as_ref()))
            .unwrap_or_else(Expr::unit);

        match var_decl.kind {
            VarDeclKind::Const => {
                items.push(Item::from(ItemDefConst {
                    ty_annotation: None,
                    visibility,
                    name: name_ident.clone(),
                    ty: None,
                    value: Box::new(init_expr),
                }));
            }
            VarDeclKind::Let | VarDeclKind::Var => {
                items.push(Item::from(ItemDeclConst {
                    ty_annotation: None,
                    name: name_ident.clone(),
                    ty: Ty::any(),
                }));
            }
        }
    }
    items
}

fn module_export_name_to_ident(name: &ModuleExportName) -> Ident {
    match name {
        ModuleExportName::Ident(ident) => Ident::new(sanitize_ident(&ident.sym.to_string())),
        ModuleExportName::Str(str_) => Ident::new(sanitize_ident(&str_.value)),
    }
}

fn pat_to_ident(pat: &Pat) -> Option<Ident> {
    match pat {
        Pat::Ident(binding) => Some(Ident::new(sanitize_ident(&binding.id.sym.to_string()))),
        Pat::Assign(assign) => pat_to_ident(&assign.left),
        Pat::Rest(rest) => pat_to_ident(&rest.arg),
        _ => None,
    }
}

fn lower_expr(expr: &TsExpr) -> Expr {
    match expr {
        TsExpr::Lit(lit) => lower_lit(lit),
        _ => Expr::unit(),
    }
}

fn lower_lit(lit: &Lit) -> Expr {
    match lit {
        Lit::Str(str_) => Expr::value(Value::string(str_.value.to_string())),
        Lit::Num(num) => Expr::value(Value::decimal(num.value)),
        Lit::Bool(boolean) => Expr::value(Value::bool(boolean.value)),
        Lit::Null(_) => Expr::value(Value::null()),
        Lit::BigInt(bigint) => Expr::value(Value::string(bigint.value.to_string())),
        _ => Expr::unit(),
    }
}

#[derive(Debug)]
enum ModuleSegment {
    SelfMod,
    SuperMod,
    Root,
    Ident(Ident),
}

fn module_spec_to_segments(spec: &str) -> Vec<ModuleSegment> {
    let unquoted = spec.trim_matches(|c| matches!(c, '"' | '\'' | '`'));
    let without_fragment = unquoted
        .split_once(|c| c == '?' || c == '#')
        .map(|(base, _)| base)
        .unwrap_or(unquoted);
    let without_ext = strip_extension(without_fragment);

    let mut segments = Vec::new();
    let mut leading_root = false;
    for (index, segment) in without_ext.split('/').enumerate() {
        if segment.is_empty() {
            if index == 0 {
                leading_root = true;
            }
            continue;
        }
        match segment {
            "." => segments.push(ModuleSegment::SelfMod),
            ".." => segments.push(ModuleSegment::SuperMod),
            _ => segments.push(ModuleSegment::Ident(Ident::new(sanitize_ident(segment)))),
        }
    }

    if leading_root {
        segments.insert(0, ModuleSegment::Root);
    }

    segments
}

fn strip_extension(path: &str) -> &str {
    for ext in [".d.ts", ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs"] {
        if let Some(stripped) = path.strip_suffix(ext) {
            return stripped;
        }
    }
    path
}

fn sanitize_ident(raw: &str) -> String {
    let mut ident = raw
        .chars()
        .map(|ch| match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => ch,
            '-' | '.' | '@' => '_',
            _ => '_',
        })
        .collect::<String>();
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn loose_mode_allows_recovery() {
        let frontend = TypeScriptFrontend::new(TsParseMode::Loose);
        let result = frontend.parse("const value = ;", None);
        assert!(
            result.is_ok(),
            "loose mode should not fail on syntax errors"
        );
    }

    #[test]
    fn strict_mode_fails_on_errors() {
        let frontend = TypeScriptFrontend::new(TsParseMode::Strict);
        let result = frontend.parse("const value = ;", None);
        assert!(result.is_err(), "strict mode should fail on syntax errors");
    }

    #[test]
    fn lowers_relative_import_with_group() {
        let frontend = TypeScriptFrontend::new(TsParseMode::Strict);
        let result = frontend
            .parse(
                r#"import foo, { bar as baz, type Baz } from "./lib/utils.ts";"#,
                None,
            )
            .expect("parsing import failed");

        let node = result.ast;
        let file = match node.kind() {
            NodeKind::File(file) => file,
            _ => panic!("expected file node"),
        };
        assert_eq!(file.items.len(), 1);

        let import = file.items[0].as_import().expect("expected import item");
        match &import.tree {
            ItemImportTree::Path(path) => {
                assert!(
                    matches!(path.segments.first(), Some(ItemImportTree::SelfMod)),
                    "expected first segment to be self"
                );
                assert_eq!(path.segments.len(), 4);
                assert!(
                    matches!(path.segments[1], ItemImportTree::Ident(_)),
                    "expected library segment"
                );
                assert!(
                    matches!(path.segments[2], ItemImportTree::Ident(_)),
                    "expected module segment"
                );
                match &path.segments[3] {
                    ItemImportTree::Group(group) => {
                        assert_eq!(group.items.len(), 2);
                        let mut entries = group
                            .items
                            .iter()
                            .filter_map(|item| match item {
                                ItemImportTree::Rename(rename) => Some((
                                    rename.from.as_str().to_string(),
                                    rename.to.as_str().to_string(),
                                )),
                                ItemImportTree::Ident(ident) => {
                                    Some((ident.as_str().to_string(), ident.as_str().to_string()))
                                }
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        entries.sort();
                        assert_eq!(
                            entries,
                            vec![
                                ("bar".to_string(), "baz".to_string()),
                                ("default".to_string(), "foo".to_string())
                            ]
                        );
                    }
                    other => panic!("expected group, found {other:?}"),
                }
            }
            other => panic!("expected import path, found {other:?}"),
        }
    }

    #[test]
    fn module_spec_handles_parent_segments() {
        let segments = module_spec_to_segments("../core/index.js");
        assert_eq!(segments.len(), 3);
        assert!(matches!(segments[0], ModuleSegment::SuperMod));
        match &segments[1] {
            ModuleSegment::Ident(ident) => assert_eq!(ident.as_str(), "core"),
            other => panic!("expected ident, found {other:?}"),
        }
        match &segments[2] {
            ModuleSegment::Ident(ident) => assert_eq!(ident.as_str(), "index"),
            other => panic!("expected ident, found {other:?}"),
        }
    }

    #[test]
    fn collects_import_references_for_runtime_edges() {
        let source = r#"
            import { foo } from "./foo";
            import type { Bar } from "./bar";
            import Baz from "./baz";
            export { qux } from "../qux/index.ts";
            export * from "./all";
        "#;

        let references = collect_import_references(source, None);
        assert_eq!(references.len(), 5);

        let runtime_refs: Vec<_> = references
            .iter()
            .filter(|reference| !reference.is_type_only)
            .collect();
        assert_eq!(runtime_refs.len(), 4);

        assert!(runtime_refs
            .iter()
            .any(|reference| reference.spec == "./foo"));
        assert!(runtime_refs
            .iter()
            .any(|reference| reference.spec == "./baz"));
        assert!(runtime_refs
            .iter()
            .any(|reference| reference.spec == "../qux/index.ts"));
        assert!(runtime_refs
            .iter()
            .any(|reference| reference.spec == "./all"));

        let type_only_refs: Vec<_> = references
            .into_iter()
            .filter(|reference| reference.is_type_only)
            .collect();
        assert_eq!(type_only_refs.len(), 1);
        assert_eq!(type_only_refs[0].spec, "./bar");
    }
}
