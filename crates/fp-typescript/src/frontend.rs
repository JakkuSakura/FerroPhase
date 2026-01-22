use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use fp_core::ast::{
    DecimalType, EnumTypeVariant, Expr, ExprBlock, ExprInvoke, ExprInvokeTarget, ExprKind, File,
    FunctionParam, FunctionSignature, Ident, Item, ItemDeclConst, ItemDefConst, ItemDefEnum,
    ItemDefFunction, ItemDefStruct, ItemDefType, ItemImport, ItemImportGroup, ItemImportPath,
    ItemImportRename, ItemImportTree, ItemKind, Locator, Module as AstModule, Node, NodeKind,
    StructuralField, Ty, TypeEnum, TypeInt, TypePrimitive, TypeStruct, TypeStructural, TypeTuple,
    TypeVec, Value, Visibility,
};
use fp_core::diagnostics::{Diagnostic, DiagnosticManager};
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::frontend::{FrontendResult, LanguageFrontend};
use swc_common::input::StringInput;
use swc_common::{sync::Lrc, FileName, SourceMap, DUMMY_SP};
use swc_ecma_ast::EsVersion;
use swc_ecma_ast::{
    ArrayPat, BlockStmt, ClassDecl, ClassExpr, ClassMember, Constructor, Decl, ExportAll,
    ExportDecl, ExportSpecifier, Expr as TsExpr, FnDecl, ImportDecl, ImportSpecifier, Lit,
    MemberProp, MethodKind, Module, ModuleDecl, ModuleExportName, ModuleItem, NamedExport,
    ObjectPat, ObjectPatProp, Param, ParamOrTsParamProp, Pat, PropName, TsEntityName, TsEnumDecl,
    TsEnumMemberId, TsExprWithTypeArgs, TsInterfaceDecl, TsIntersectionType, TsKeywordTypeKind,
    TsLit, TsLitType, TsParamPropParam, TsType, TsTypeAliasDecl, TsTypeAnn, TsTypeElement,
    TsTypeLit, TsTypeRef, TsUnionOrIntersectionType, TsUnionType, VarDecl, VarDeclKind,
};
use swc_ecma_parser::error::Error as SwcError;
use swc_ecma_parser::lexer::Lexer;
use swc_ecma_parser::{Parser, Syntax, TsSyntax};

use crate::resolution::{is_typescript_like_source, resolve_imports};
use crate::ts::serializer::TypeScriptSerializer;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TsParseMode {
    Strict,
    Loose,
}

const LANGUAGE_KEY: &str = "typescript";
const EXTENSIONS: &[&str] = &["ts", "tsx"];

#[derive(Debug, Default)]
pub struct DependencyParseOutcome {
    pub modules: Vec<(PathBuf, Node)>,
    pub warnings: Vec<String>,
}

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

    pub fn parse_dependencies(
        &self,
        path: &Path,
        source: &str,
        resolve: bool,
    ) -> CoreResult<DependencyParseOutcome> {
        if !resolve || !is_typescript_like_source(path) {
            return Ok(DependencyParseOutcome::default());
        }

        let resolved = resolve_imports(path, source);
        let mut outcome = DependencyParseOutcome {
            modules: Vec::new(),
            warnings: resolved.warnings,
        };

        let mode = self.parse_mode();
        for module in resolved.modules {
            match self.parse(&module.source, Some(module.path.as_path())) {
                Ok(result) => outcome.modules.push((module.path, result.ast)),
                Err(err) => {
                    if matches!(mode, TsParseMode::Loose) {
                        outcome.warnings.push(format!(
                            "Warning: failed to parse resolved import {} ({err})",
                            module.path.display()
                        ));
                    } else {
                        return Err(err);
                    }
                }
            }
        }

        Ok(outcome)
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
            macro_parser: None,
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

#[derive(Default)]
struct TypeLoweringContext {
    structs: HashMap<String, Vec<StructuralField>>,
    aliases: HashMap<String, Ty>,
}

impl TypeLoweringContext {
    fn record_item(&mut self, item: &Item) {
        match item.kind() {
            ItemKind::Module(module) => {
                for inner in &module.items {
                    self.record_item(inner);
                }
            }
            ItemKind::DefStruct(def) => {
                self.structs
                    .insert(def.name.as_str().to_string(), def.value.fields.clone());
            }
            ItemKind::DefType(def) => {
                self.aliases
                    .insert(def.name.as_str().to_string(), def.value.clone());
            }
            _ => {}
        }
    }

    fn resolve_struct_fields(&self, name: &str) -> Option<Vec<StructuralField>> {
        self.resolve_struct_fields_inner(name, &mut HashSet::new())
    }

    fn resolve_struct_fields_inner(
        &self,
        name: &str,
        visited: &mut HashSet<String>,
    ) -> Option<Vec<StructuralField>> {
        if !visited.insert(name.to_string()) {
            return None;
        }
        if let Some(fields) = self.structs.get(name) {
            return Some(fields.clone());
        }
        let ty = self.aliases.get(name)?;
        self.resolve_struct_fields_from_ty(ty, visited)
    }

    fn resolve_struct_fields_from_ty(
        &self,
        ty: &Ty,
        visited: &mut HashSet<String>,
    ) -> Option<Vec<StructuralField>> {
        match ty {
            Ty::Structural(structural) => Some(structural.fields.clone()),
            Ty::Expr(expr) => self.resolve_struct_fields_from_expr(expr, visited),
            _ => None,
        }
    }

    fn resolve_struct_fields_from_expr(
        &self,
        expr: &Expr,
        visited: &mut HashSet<String>,
    ) -> Option<Vec<StructuralField>> {
        match expr.kind() {
            ExprKind::Locator(locator) => {
                if let Some(ident) = locator.as_ident() {
                    self.resolve_struct_fields_inner(ident.as_str(), visited)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

fn lower_module(module: &Module) -> Vec<Item> {
    let mut ctx = TypeLoweringContext::default();
    let mut items = Vec::new();
    for item in &module.body {
        let mut lowered = match item {
            ModuleItem::ModuleDecl(decl) => lower_module_decl(decl, &ctx),
            ModuleItem::Stmt(stmt) => {
                if let swc_ecma_ast::Stmt::Decl(decl) = stmt {
                    lower_decl(decl, Visibility::Inherited, &ctx)
                } else {
                    Vec::new()
                }
            }
        };
        for lowered_item in &lowered {
            ctx.record_item(lowered_item);
        }
        items.append(&mut lowered);
    }
    items
}

fn lower_module_decl(decl: &ModuleDecl, ctx: &TypeLoweringContext) -> Vec<Item> {
    match decl {
        ModuleDecl::Import(import) => lower_import(import),
        ModuleDecl::ExportDecl(export) => lower_export_decl(export, ctx),
        ModuleDecl::ExportAll(export) => lower_export_all(export),
        ModuleDecl::ExportNamed(named) => lower_named_export(named),
        ModuleDecl::ExportDefaultDecl(default_decl) => lower_export_default_decl(default_decl, ctx),
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
        attrs: Vec::new(),
        visibility: Visibility::Inherited,
        tree: ItemImportTree::Path(path),
    })]
}

fn lower_export_decl(export: &ExportDecl, ctx: &TypeLoweringContext) -> Vec<Item> {
    lower_decl(&export.decl, Visibility::Public, ctx)
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
        attrs: Vec::new(),
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
                attrs: Vec::new(),
                visibility: Visibility::Public,
                tree: ItemImportTree::Path(path),
            }));
        }
    }

    items
}

fn lower_export_default_decl(
    default_decl: &swc_ecma_ast::ExportDefaultDecl,
    ctx: &TypeLoweringContext,
) -> Vec<Item> {
    match &default_decl.decl {
        swc_ecma_ast::DefaultDecl::Fn(fn_expr) => {
            if let Some(identifier) = &fn_expr.ident {
                let fn_decl = FnDecl {
                    declare: false,
                    ident: identifier.clone(),
                    function: fn_expr.function.clone(),
                };
                lower_fn_decl(&fn_decl, Visibility::Public, ctx)
                    .into_iter()
                    .collect()
            } else {
                Vec::new()
            }
        }
        swc_ecma_ast::DefaultDecl::Class(class_expr) => {
            lower_class_expr(class_expr, Visibility::Public)
        }
        swc_ecma_ast::DefaultDecl::TsInterfaceDecl(_) => Vec::new(),
    }
}

fn lower_decl(decl: &Decl, visibility: Visibility, ctx: &TypeLoweringContext) -> Vec<Item> {
    match decl {
        Decl::Fn(fn_decl) => lower_fn_decl(fn_decl, visibility, ctx)
            .into_iter()
            .collect(),
        Decl::Class(class_decl) => lower_class_decl(class_decl, visibility),
        Decl::Var(var_decl) => lower_var_decl(var_decl, visibility),
        Decl::TsTypeAlias(alias) => lower_type_alias(alias, visibility, ctx),
        Decl::TsInterface(interface) => lower_interface_decl(interface, visibility, ctx),
        Decl::TsEnum(enum_decl) => lower_enum_decl(enum_decl, visibility),
        Decl::Using(_) | Decl::TsModule(_) => Vec::new(),
    }
}

fn lower_type_alias(
    alias: &TsTypeAliasDecl,
    visibility: Visibility,
    _ctx: &TypeLoweringContext,
) -> Vec<Item> {
    if alias.type_params.is_some() {
        return Vec::new();
    }

    let name = sanitize_ident(&alias.id.sym.to_string());
    if name.is_empty() {
        return Vec::new();
    }
    let ident = Ident::new(name);

    let ty = lower_ts_type(alias.type_ann.as_ref());
    if let Ty::Structural(structural) = &ty {
        let def = ItemDefStruct {
            attrs: Vec::new(),
            visibility,
            name: ident.clone(),
            value: TypeStruct {
                name: ident.clone(),
                generics_params: Vec::new(),
                fields: structural.fields.clone(),
            },
        };
        return vec![Item::from(def)];
    }

    let def = ItemDefType {
        attrs: Vec::new(),
        visibility,
        name: ident,
        value: ty,
    };
    vec![Item::from(def)]
}

fn lower_interface_decl(
    interface: &TsInterfaceDecl,
    visibility: Visibility,
    ctx: &TypeLoweringContext,
) -> Vec<Item> {
    if interface.type_params.is_some() {
        return Vec::new();
    }

    let name = sanitize_ident(&interface.id.sym.to_string());
    if name.is_empty() {
        return Vec::new();
    }
    let ident = Ident::new(name);

    let mut merged_fields = Vec::new();
    let mut field_index: HashMap<String, usize> = HashMap::new();

    let inherited_names: Vec<Ident> = interface
        .extends
        .iter()
        .filter_map(heritage_expr_to_ident)
        .collect();

    for base in &inherited_names {
        if let Some(base_fields) = ctx.resolve_struct_fields(base.as_str()) {
            for field in base_fields {
                let key = field.name.as_str().to_string();
                if field_index.contains_key(&key) {
                    continue;
                }
                field_index.insert(key, merged_fields.len());
                merged_fields.push(field);
            }
        }
    }

    let mut own_fields = Vec::new();
    for member in &interface.body.body {
        if let Some(field) = lower_interface_member(member) {
            own_fields.push(field);
        }
    }

    if merged_fields.is_empty()
        && own_fields.is_empty()
        && inherited_names
            .iter()
            .filter_map(|ident| ctx.resolve_struct_fields(ident.as_str()))
            .count()
            == 0
    {
        if let Some(base_ident) = inherited_names.first() {
            let def = ItemDefType {
                attrs: Vec::new(),
                visibility,
                name: ident,
                value: Ty::ident(base_ident.clone()),
            };
            return vec![Item::from(def)];
        }
        return Vec::new();
    }

    for field in own_fields {
        let key = field.name.as_str().to_string();
        if let Some(idx) = field_index.get(&key).copied() {
            merged_fields[idx] = field;
        } else {
            field_index.insert(key, merged_fields.len());
            merged_fields.push(field);
        }
    }

    if merged_fields.is_empty() {
        if let Some(base_ident) = inherited_names.first() {
            let def = ItemDefType {
                attrs: Vec::new(),
                visibility,
                name: ident,
                value: Ty::ident(base_ident.clone()),
            };
            return vec![Item::from(def)];
        }
        return Vec::new();
    }

    let def = ItemDefStruct {
        attrs: Vec::new(),
        visibility,
        name: ident.clone(),
        value: TypeStruct {
            name: ident.clone(),
            generics_params: Vec::new(),
            fields: merged_fields,
        },
    };
    vec![Item::from(def)]
}

fn lower_interface_member(member: &TsTypeElement) -> Option<StructuralField> {
    match member {
        TsTypeElement::TsPropertySignature(prop) => {
            let name = property_signature_name(prop.key.as_ref(), prop.computed)?;
            let mut ty = prop
                .type_ann
                .as_ref()
                .map(|ann| lower_ts_type(ann.type_ann.as_ref()))
                .unwrap_or_else(Ty::any);
            if prop.optional && !is_option_ty(&ty) {
                ty = option_ty(ty);
            }
            Some(StructuralField::new(Ident::new(name), ty))
        }
        _ => None,
    }
}

fn option_ty(inner: Ty) -> Ty {
    let invoke = ExprInvoke {
        span: fp_core::span::Span::null(),
        target: ExprInvokeTarget::Function(Locator::ident("option")),
        args: vec![Expr::value(Value::Type(inner))],
        kwargs: Vec::new(),
    };
    Ty::expr(Expr::new(ExprKind::Invoke(invoke)))
}

fn is_option_ty(ty: &Ty) -> bool {
    match ty {
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Invoke(invoke) => match &invoke.target {
                ExprInvokeTarget::Function(locator) => locator
                    .as_ident()
                    .map(|ident| ident.as_str() == "option")
                    .unwrap_or(false),
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}

fn heritage_expr_to_ident(heritage: &TsExprWithTypeArgs) -> Option<Ident> {
    let name = heritage_expr_to_name(heritage.expr.as_ref())?;
    if name.is_empty() {
        None
    } else {
        Some(Ident::new(name))
    }
}

fn heritage_expr_to_name(expr: &TsExpr) -> Option<String> {
    match expr {
        TsExpr::Ident(ident) => Some(sanitize_ident(&ident.sym.to_string())),
        TsExpr::Member(member) => {
            let mut segments = Vec::new();
            collect_member_segments(member, &mut segments)?;
            Some(segments.join("_"))
        }
        _ => None,
    }
}

fn collect_member_segments(
    member: &swc_ecma_ast::MemberExpr,
    output: &mut Vec<String>,
) -> Option<()> {
    match member.obj.as_ref() {
        TsExpr::Ident(ident) => output.push(sanitize_ident(&ident.sym.to_string())),
        TsExpr::Member(inner) => collect_member_segments(inner, output)?,
        _ => return None,
    }

    let segment = match &member.prop {
        MemberProp::Ident(ident) => sanitize_ident(&ident.sym.to_string()),
        MemberProp::Computed(comp) => {
            if let TsExpr::Lit(Lit::Str(str_)) = comp.expr.as_ref() {
                sanitize_ident(&str_.value)
            } else {
                return None;
            }
        }
        MemberProp::PrivateName(_) => return None,
    };

    output.push(segment);
    Some(())
}

fn lower_enum_decl(enum_decl: &TsEnumDecl, visibility: Visibility) -> Vec<Item> {
    let name = sanitize_ident(&enum_decl.id.sym.to_string());
    if name.is_empty() {
        return Vec::new();
    }
    let ident = Ident::new(name);

    let mut variants = Vec::new();
    for member in &enum_decl.members {
        let Some(variant_name) = enum_member_name(&member.id) else {
            continue;
        };
        let value = member
            .init
            .as_ref()
            .and_then(|expr| infer_expr_type(expr.as_ref()))
            .unwrap_or_else(Ty::unit);
        variants.push(EnumTypeVariant {
            name: Ident::new(variant_name),
            value,
            discriminant: None,
        });
    }

    if variants.is_empty() {
        return Vec::new();
    }

    let def = ItemDefEnum {
        attrs: Vec::new(),
        visibility,
        name: ident.clone(),
        value: TypeEnum {
            name: ident.clone(),
            generics_params: Vec::new(),
            variants,
        },
    };
    vec![Item::from(def)]
}

fn enum_member_name(id: &TsEnumMemberId) -> Option<String> {
    match id {
        TsEnumMemberId::Ident(ident) => Some(sanitize_ident(&ident.sym.to_string())),
        TsEnumMemberId::Str(str_) => Some(sanitize_ident(&str_.value)),
    }
}

fn lower_class_decl(class_decl: &ClassDecl, visibility: Visibility) -> Vec<Item> {
    let name = sanitize_ident(&class_decl.ident.sym.to_string());
    lower_class_like(Some(name), &class_decl.class, visibility)
}

fn lower_class_expr(class_expr: &ClassExpr, visibility: Visibility) -> Vec<Item> {
    let name = class_expr
        .ident
        .as_ref()
        .map(|ident| sanitize_ident(&ident.sym.to_string()));
    lower_class_like(name, &class_expr.class, visibility)
}

fn lower_class_like(
    name: Option<String>,
    class: &swc_ecma_ast::Class,
    visibility: Visibility,
) -> Vec<Item> {
    let Some(class_name) = name.filter(|n| !n.is_empty()) else {
        return Vec::new();
    };

    let mut items = Vec::new();
    for member in &class.body {
        if let Some(item) = lower_class_member(member) {
            items.push(item);
        }
    }

    if items.is_empty() {
        return Vec::new();
    }

    vec![Item::from(AstModule {
        attrs: Vec::new(),
        name: Ident::new(class_name),
        items,
        visibility,
        is_external: false,
    })]
}

fn lower_class_member(member: &ClassMember) -> Option<Item> {
    match member {
        ClassMember::Constructor(constructor) => Some(lower_constructor(constructor)),
        ClassMember::Method(method) => lower_class_method(method),
        ClassMember::ClassProp(_)
        | ClassMember::PrivateMethod(_)
        | ClassMember::PrivateProp(_)
        | ClassMember::TsIndexSignature(_)
        | ClassMember::Empty(_)
        | ClassMember::AutoAccessor(_)
        | ClassMember::StaticBlock(_) => None,
    }
}

fn lower_constructor(constructor: &Constructor) -> Item {
    let params: Vec<Param> = constructor
        .params
        .iter()
        .map(|param| match param {
            ParamOrTsParamProp::Param(param) => param.clone(),
            ParamOrTsParamProp::TsParamProp(prop) => match &prop.param {
                TsParamPropParam::Ident(ident) => Param::from(Pat::Ident(ident.clone())),
                TsParamPropParam::Assign(assign) => Param::from(Pat::Assign(assign.clone())),
            },
        })
        .collect();

    let mut function = build_function_item(
        "new",
        &params,
        None,
        constructor.body.as_ref(),
        Visibility::Public,
    );
    function
        .sig
        .ret_ty
        .get_or_insert_with(|| Ty::ident(Ident::new("Self")));
    Item::from(function)
}

fn lower_class_method(method: &swc_ecma_ast::ClassMethod) -> Option<Item> {
    let base_name = prop_name_to_string(&method.key)?;
    let method_name = match method.kind {
        MethodKind::Method => base_name,
        MethodKind::Getter => format!("get_{}", base_name),
        MethodKind::Setter => format!("set_{}", base_name),
    };

    let mut function = build_function_item(
        &method_name,
        &method.function.params,
        method.function.return_type.as_deref(),
        method.function.body.as_ref(),
        Visibility::Public,
    );

    if matches!(method.kind, MethodKind::Getter) && function.sig.ret_ty.is_none() {
        function.sig.ret_ty = Some(Ty::any());
    }
    if matches!(method.kind, MethodKind::Setter) {
        function.sig.ret_ty = Some(Ty::unit());
    }

    Some(Item::from(function))
}

fn lower_fn_decl(
    fn_decl: &FnDecl,
    visibility: Visibility,
    _ctx: &TypeLoweringContext,
) -> Option<Item> {
    let name = sanitize_ident(&fn_decl.ident.sym.to_string());
    if name.is_empty() {
        return None;
    }
    let function = build_function_item(
        &name,
        &fn_decl.function.params,
        fn_decl.function.return_type.as_deref(),
        fn_decl.function.body.as_ref(),
        visibility,
    );
    Some(Item::from(function))
}

fn lower_function_params(params: &[Param]) -> Vec<FunctionParam> {
    params
        .iter()
        .enumerate()
        .map(|(idx, param)| {
            let info = param_info_from_pat(&param.pat, idx);
            let mut function_param = FunctionParam::new(info.ident, info.ty);
            if info.as_tuple {
                function_param.as_tuple = true;
            }
            if info.as_dict {
                function_param.as_dict = true;
            }
            function_param
        })
        .collect()
}

struct ParamInfo {
    ident: Ident,
    ty: Ty,
    as_tuple: bool,
    as_dict: bool,
}

fn param_info_from_pat(pat: &Pat, idx: usize) -> ParamInfo {
    let fallback = format!("arg{idx}");
    param_info_from_pat_inner(pat, &fallback)
}

fn param_info_from_pat_inner(pat: &Pat, fallback: &str) -> ParamInfo {
    match pat {
        Pat::Ident(binding) => {
            let name = sanitize_ident(&binding.id.sym.to_string());
            let ty = binding
                .type_ann
                .as_ref()
                .map(|ann| lower_ts_type(&ann.type_ann))
                .unwrap_or_else(Ty::any);
            ParamInfo {
                ident: Ident::new(name),
                ty,
                as_tuple: false,
                as_dict: false,
            }
        }
        Pat::Assign(assign) => {
            let mut info = param_info_from_pat_inner(&assign.left, fallback);
            if is_any_type(&info.ty) {
                if let Some(expr_ty) = infer_expr_type(assign.right.as_ref()) {
                    info.ty = expr_ty;
                }
            }
            info
        }
        Pat::Object(object_pat) => {
            let name = Ident::new(sanitize_ident(fallback));
            let ty = lower_object_pattern_type(object_pat);
            ParamInfo {
                ident: name,
                ty,
                as_tuple: false,
                as_dict: true,
            }
        }
        Pat::Array(array_pat) => {
            let name = Ident::new(sanitize_ident(fallback));
            let ty = lower_array_pattern_type(array_pat);
            ParamInfo {
                ident: name,
                ty,
                as_tuple: true,
                as_dict: false,
            }
        }
        Pat::Rest(rest) => {
            let mut info = param_info_from_pat_inner(&rest.arg, fallback);
            let elem_ty = rest
                .type_ann
                .as_ref()
                .map(|ann| lower_ts_type(&ann.type_ann))
                .unwrap_or_else(|| info.ty.clone());
            info.ty = Ty::Vec(TypeVec {
                ty: Box::new(elem_ty),
            });
            info.as_tuple = true;
            info
        }
        _ => ParamInfo {
            ident: Ident::new(sanitize_ident(fallback)),
            ty: Ty::any(),
            as_tuple: false,
            as_dict: false,
        },
    }
}

fn lower_function_body(_body: &swc_ecma_ast::BlockStmt) -> Expr {
    Expr::block(ExprBlock::new())
}

fn build_function_item(
    name: &str,
    params: &[Param],
    return_type: Option<&TsTypeAnn>,
    body: Option<&BlockStmt>,
    visibility: Visibility,
) -> ItemDefFunction {
    let name_ident = Ident::new(sanitize_ident(name));
    let mut sig = FunctionSignature::unit();
    sig.name = Some(name_ident.clone());
    sig.params = lower_function_params(params);
    if let Some(ret_ann) = return_type {
        sig.ret_ty = Some(lower_ts_type(ret_ann.type_ann.as_ref()));
    }

    let body_expr = body.map(lower_function_body).unwrap_or_else(Expr::unit);

    ItemDefFunction {
        ty_annotation: None,
        attrs: Vec::new(),
        name: name_ident,
        ty: None,
        sig,
        body: Box::new(body_expr),
        visibility,
    }
}

fn lower_object_pattern_type(pattern: &ObjectPat) -> Ty {
    if let Some(type_ann) = pattern.type_ann.as_ref() {
        return lower_ts_type(type_ann.type_ann.as_ref());
    }

    let mut fields = Vec::new();
    for prop in &pattern.props {
        match prop {
            ObjectPatProp::KeyValue(key_value) => {
                if let Some(name) = prop_name_to_string(&key_value.key) {
                    let field_ty = infer_pat_type(key_value.value.as_ref());
                    fields.push(StructuralField::new(Ident::new(name), field_ty));
                }
            }
            ObjectPatProp::Assign(assign) => {
                let name = sanitize_ident(&assign.key.id.sym.to_string());
                let ty = assign
                    .key
                    .type_ann
                    .as_ref()
                    .map(|ann| lower_ts_type(ann.type_ann.as_ref()))
                    .unwrap_or_else(Ty::any);
                fields.push(StructuralField::new(Ident::new(name), ty));
            }
            ObjectPatProp::Rest(rest) => {
                let ty = rest
                    .type_ann
                    .as_ref()
                    .map(|ann| lower_ts_type(ann.type_ann.as_ref()))
                    .unwrap_or_else(Ty::any);
                fields.push(StructuralField::new(Ident::new("rest"), ty));
            }
        }
    }

    structural_from_fields(fields)
}

fn lower_array_pattern_type(pattern: &ArrayPat) -> Ty {
    if let Some(type_ann) = pattern.type_ann.as_ref() {
        return lower_ts_type(type_ann.type_ann.as_ref());
    }

    let mut elements = Vec::new();
    for element in &pattern.elems {
        if let Some(pat) = element {
            elements.push(infer_pat_type(pat));
        }
    }

    if elements.is_empty() {
        Ty::Vec(TypeVec {
            ty: Box::new(Ty::any()),
        })
    } else {
        Ty::Tuple(TypeTuple { types: elements })
    }
}

fn infer_pat_type(pat: &Pat) -> Ty {
    match pat {
        Pat::Ident(binding) => binding
            .type_ann
            .as_ref()
            .map(|ann| lower_ts_type(ann.type_ann.as_ref()))
            .unwrap_or_else(Ty::any),
        Pat::Assign(assign) => {
            let ty = infer_pat_type(&assign.left);
            if is_any_type(&ty) {
                infer_expr_type(assign.right.as_ref()).unwrap_or(ty)
            } else {
                ty
            }
        }
        Pat::Object(obj) => lower_object_pattern_type(obj),
        Pat::Array(array) => lower_array_pattern_type(array),
        Pat::Rest(rest) => {
            let elem_ty = rest
                .type_ann
                .as_ref()
                .map(|ann| lower_ts_type(ann.type_ann.as_ref()))
                .unwrap_or_else(|| infer_pat_type(&rest.arg));
            Ty::Vec(TypeVec {
                ty: Box::new(elem_ty),
            })
        }
        _ => Ty::any(),
    }
}

fn infer_expr_type(expr: &TsExpr) -> Option<Ty> {
    match expr {
        TsExpr::Lit(Lit::Str(_)) => Some(Ty::Primitive(TypePrimitive::String)),
        TsExpr::Lit(Lit::Bool(_)) => Some(Ty::Primitive(TypePrimitive::Bool)),
        TsExpr::Lit(Lit::Num(_)) => Some(Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64))),
        TsExpr::Array(array) => {
            let mut element_types = Vec::new();
            for element in &array.elems {
                if let Some(expr_or_spread) = element {
                    if expr_or_spread.spread.is_some() {
                        return Some(Ty::Vec(TypeVec {
                            ty: Box::new(Ty::any()),
                        }));
                    }
                    let inner = expr_or_spread.expr.as_ref();
                    if let Some(inner_ty) = infer_expr_type(inner) {
                        element_types.push(inner_ty);
                    }
                }
            }
            let element_ty = unify_types(&element_types).unwrap_or_else(Ty::any);
            Some(Ty::Vec(TypeVec {
                ty: Box::new(element_ty),
            }))
        }
        TsExpr::Object(_) => Some(Ty::any()),
        _ => None,
    }
}

fn unify_types(types: &[Ty]) -> Option<Ty> {
    let mut concrete = types.iter().filter(|ty| !is_any_type(ty));
    let first = concrete.next()?.clone();
    if concrete.all(|ty| *ty == first) {
        Some(first)
    } else {
        Some(Ty::any())
    }
}

fn structural_from_fields(fields: Vec<StructuralField>) -> Ty {
    Ty::Structural(TypeStructural { fields })
}
fn is_any_type(ty: &Ty) -> bool {
    matches!(ty, Ty::Any(_) | Ty::Unknown(_))
}

fn explicit_type_from_pat(pat: &Pat) -> Option<Ty> {
    match pat {
        Pat::Ident(binding) => binding
            .type_ann
            .as_ref()
            .map(|ann| lower_ts_type(ann.type_ann.as_ref())),
        Pat::Assign(assign) => explicit_type_from_pat(&assign.left),
        Pat::Object(obj) => obj
            .type_ann
            .as_ref()
            .map(|ann| lower_ts_type(ann.type_ann.as_ref())),
        Pat::Array(array) => array
            .type_ann
            .as_ref()
            .map(|ann| lower_ts_type(ann.type_ann.as_ref())),
        Pat::Rest(rest) => rest
            .type_ann
            .as_ref()
            .map(|ann| lower_ts_type(ann.type_ann.as_ref())),
        _ => None,
    }
}

fn lower_ts_type(ty: &TsType) -> Ty {
    match ty {
        TsType::TsKeywordType(keyword) => match keyword.kind {
            TsKeywordTypeKind::TsStringKeyword => Ty::Primitive(TypePrimitive::String),
            TsKeywordTypeKind::TsNumberKeyword => {
                Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64))
            }
            TsKeywordTypeKind::TsBooleanKeyword => Ty::Primitive(TypePrimitive::Bool),
            TsKeywordTypeKind::TsAnyKeyword => Ty::any(),
            TsKeywordTypeKind::TsUndefinedKeyword
            | TsKeywordTypeKind::TsVoidKeyword
            | TsKeywordTypeKind::TsNullKeyword => Ty::unit(),
            TsKeywordTypeKind::TsNeverKeyword => Ty::any(),
            TsKeywordTypeKind::TsUnknownKeyword => Ty::any(),
            TsKeywordTypeKind::TsBigIntKeyword => {
                Ty::Primitive(TypePrimitive::Int(TypeInt::BigInt))
            }
            TsKeywordTypeKind::TsSymbolKeyword => Ty::Primitive(TypePrimitive::String),
            TsKeywordTypeKind::TsIntrinsicKeyword => Ty::any(),
            TsKeywordTypeKind::TsObjectKeyword => Ty::any(),
        },
        TsType::TsThisType(_) => Ty::ident(Ident::new("Self")),
        TsType::TsFnOrConstructorType(_) => Ty::any(),
        TsType::TsTypeRef(type_ref) => lower_ts_type_ref(type_ref),
        TsType::TsTypeQuery(_) => Ty::any(),
        TsType::TsTypeLit(lit) => lower_type_literal(lit),
        TsType::TsArrayType(array) => Ty::Vec(TypeVec {
            ty: Box::new(lower_ts_type(array.elem_type.as_ref())),
        }),
        TsType::TsTupleType(tuple) => Ty::Tuple(TypeTuple {
            types: tuple
                .elem_types
                .iter()
                .map(|elem| lower_ts_type(elem.ty.as_ref()))
                .collect(),
        }),
        TsType::TsOptionalType(optional) => lower_ts_type(optional.type_ann.as_ref()),
        TsType::TsRestType(rest) => Ty::Vec(TypeVec {
            ty: Box::new(lower_ts_type(rest.type_ann.as_ref())),
        }),
        TsType::TsUnionOrIntersectionType(union_or_intersection) => match union_or_intersection {
            TsUnionOrIntersectionType::TsUnionType(union) => lower_union_type(union),
            TsUnionOrIntersectionType::TsIntersectionType(intersection) => {
                lower_intersection_type(intersection)
            }
        },
        TsType::TsConditionalType(_) => Ty::any(),
        TsType::TsInferType(_) => Ty::any(),
        TsType::TsParenthesizedType(parenthesized) => {
            lower_ts_type(parenthesized.type_ann.as_ref())
        }
        TsType::TsTypeOperator(operator) => lower_ts_type(operator.type_ann.as_ref()),
        TsType::TsIndexedAccessType(_) => Ty::any(),
        TsType::TsMappedType(_) => Ty::any(),
        TsType::TsLitType(lit) => lower_literal_type(lit),
        TsType::TsTypePredicate(_) => Ty::Primitive(TypePrimitive::Bool),
        TsType::TsImportType(_) => Ty::any(),
    }
}

fn lower_union_type(union: &TsUnionType) -> Ty {
    let lowered: Vec<Ty> = union
        .types
        .iter()
        .map(|ty| lower_ts_type(ty.as_ref()))
        .collect();

    let mut has_unit = false;
    let mut non_unit: Vec<Ty> = Vec::new();
    for ty in &lowered {
        if matches!(ty, Ty::Unit(_)) {
            has_unit = true;
        } else {
            non_unit.push(ty.clone());
        }
    }

    if has_unit {
        let mut distinct: Vec<Ty> = Vec::new();
        for ty in &non_unit {
            if distinct.iter().any(|existing| existing == ty) {
                continue;
            }
            distinct.push(ty.clone());
        }
        if distinct.len() == 1 {
            return option_ty(distinct.into_iter().next().unwrap());
        }
    }

    if lowered.iter().any(|ty| is_any_type(ty)) {
        return Ty::any();
    }

    let filtered: Vec<Ty> = lowered
        .into_iter()
        .filter(|ty| !matches!(ty, Ty::Unknown(_)))
        .collect();

    if filtered.is_empty() {
        return Ty::any();
    }

    let first = filtered[0].clone();
    if filtered.iter().all(|ty| *ty == first) {
        first
    } else {
        Ty::any()
    }
}

fn lower_intersection_type(intersection: &TsIntersectionType) -> Ty {
    let lowered: Vec<Ty> = intersection
        .types
        .iter()
        .map(|ty| lower_ts_type(ty.as_ref()))
        .collect();
    lowered.into_iter().next().unwrap_or_else(Ty::any)
}

fn lower_literal_type(lit: &TsLitType) -> Ty {
    match &lit.lit {
        TsLit::Str(_) | TsLit::Tpl(_) => Ty::Primitive(TypePrimitive::String),
        TsLit::Bool(_) => Ty::Primitive(TypePrimitive::Bool),
        TsLit::Number(_) => Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
        TsLit::BigInt(_) => Ty::Primitive(TypePrimitive::Int(TypeInt::BigInt)),
    }
}

fn lower_ts_type_ref(type_ref: &TsTypeRef) -> Ty {
    let raw_name = entity_name_to_string(&type_ref.type_name);
    let simple = raw_name.replace('.', "_");
    let lower = simple.to_ascii_lowercase();

    let first_param = type_ref
        .type_params
        .as_ref()
        .and_then(|params| params.params.first())
        .map(|param| lower_ts_type(param.as_ref()));

    match lower.as_str() {
        "array" | "readonlyarray" | "set" => Ty::Vec(TypeVec {
            ty: Box::new(first_param.unwrap_or_else(Ty::any)),
        }),
        "promise" => first_param.unwrap_or_else(Ty::any),
        "partial" | "required" | "readonly" => first_param.unwrap_or_else(Ty::any),
        _ => Ty::ident(Ident::new(sanitize_ident(&simple))),
    }
}

fn lower_type_literal(lit: &TsTypeLit) -> Ty {
    let mut fields = Vec::new();
    for member in &lit.members {
        if let TsTypeElement::TsPropertySignature(prop) = member {
            if let Some(name) = property_signature_name(prop.key.as_ref(), prop.computed) {
                let ty = prop
                    .type_ann
                    .as_ref()
                    .map(|ann| lower_ts_type(ann.type_ann.as_ref()))
                    .unwrap_or_else(Ty::any);
                fields.push(StructuralField::new(Ident::new(name), ty));
            }
        }
    }
    structural_from_fields(fields)
}

fn property_signature_name(expr: &TsExpr, computed: bool) -> Option<String> {
    if computed {
        return None;
    }
    match expr {
        TsExpr::Ident(ident) => Some(sanitize_ident(&ident.sym.to_string())),
        TsExpr::Lit(Lit::Str(str_)) => Some(sanitize_ident(&str_.value)),
        TsExpr::Lit(Lit::Num(num)) => Some(sanitize_ident(&num.value.to_string())),
        _ => None,
    }
}

fn entity_name_to_string(name: &TsEntityName) -> String {
    match name {
        TsEntityName::Ident(ident) => ident.sym.to_string(),
        TsEntityName::TsQualifiedName(q) => {
            format!("{}.{}", entity_name_to_string(&q.left), q.right.sym)
        }
    }
}

fn lower_var_decl(var_decl: &VarDecl, visibility: Visibility) -> Vec<Item> {
    let mut items = Vec::new();
    for declarator in &var_decl.decls {
        let Some(name_ident) = pat_to_ident(&declarator.name) else {
            continue;
        };
        let explicit_ty = explicit_type_from_pat(&declarator.name);
        let mut inferred_ty = infer_pat_type(&declarator.name);
        if is_any_type(&inferred_ty) {
            if let Some(init) = declarator
                .init
                .as_ref()
                .and_then(|expr| infer_expr_type(expr.as_ref()))
            {
                inferred_ty = init;
            }
        }

        let init_expr = declarator
            .init
            .as_ref()
            .map(|expr| lower_expr(expr.as_ref()))
            .unwrap_or_else(Expr::unit);

        match var_decl.kind {
            VarDeclKind::Const => {
                let mut item = ItemDefConst {
                    attrs: Vec::new(),
                    mutable: None,
                    ty_annotation: None,
                    visibility: visibility.clone(),
                    name: name_ident.clone(),
                    ty: Some(inferred_ty.clone()),
                    value: Box::new(init_expr),
                };
                if let Some(explicit) = explicit_ty.clone() {
                    item.set_ty_annotation(explicit);
                }
                items.push(Item::from(item));
            }
            VarDeclKind::Let | VarDeclKind::Var => {
                let mut decl = ItemDeclConst {
                    ty_annotation: None,
                    name: name_ident.clone(),
                    ty: inferred_ty.clone(),
                };
                if let Some(explicit) = explicit_ty.clone() {
                    decl.ty_annotation = Some(explicit);
                }
                items.push(Item::from(decl));
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

fn prop_name_to_string(name: &PropName) -> Option<String> {
    match name {
        PropName::Ident(ident) => Some(sanitize_ident(&ident.sym.to_string())),
        PropName::Str(str_) => Some(sanitize_ident(&str_.value)),
        PropName::Num(num) => Some(sanitize_ident(&num.value.to_string())),
        PropName::BigInt(bigint) => Some(sanitize_ident(&bigint.value.to_string())),
        PropName::Computed(_) => None,
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

    #[test]
    fn lowers_class_exports_to_module_functions() {
        let frontend = TypeScriptFrontend::new(TsParseMode::Strict);
        let source = r#"
            export default class Binance {
                fetchMarkets(market: string) { return market; }
                get status() { return "ok"; }
                set status(value: string) {}
            }
        "#;

        let result = frontend.parse(source, None).expect("class lowering failed");
        let node = result.ast;
        let file = match node.kind() {
            NodeKind::File(file) => file,
            _ => panic!("expected file node"),
        };
        let module = file
            .items
            .iter()
            .find_map(|item| item.as_module())
            .expect("expected module for class");
        assert_eq!(module.name.as_str(), "Binance");
        let mut method_names: Vec<_> = module
            .items
            .iter()
            .filter_map(|item| item.get_ident().map(|ident| ident.as_str().to_string()))
            .collect();
        method_names.sort();
        assert!(method_names.contains(&"fetchMarkets".to_string()));
        assert!(method_names.contains(&"get_status".to_string()));
        assert!(method_names.contains(&"set_status".to_string()));
    }
}
