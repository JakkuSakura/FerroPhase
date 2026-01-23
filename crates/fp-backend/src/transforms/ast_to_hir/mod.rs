use fp_core::ast::Locator;
use fp_core::ast::Pattern;
use fp_core::error::Result;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::{FileId, Span};
use fp_core::{ast, ast::ItemKind, hir};
use std::collections::{HashMap, HashSet};
use std::path::Path;

mod exprs; // expression lowering
mod helpers;
mod items; // item/impl helpers
mod patterns; // pattern lowering // shared path/locator helpers

#[cfg(test)]
mod tests;

use fp_core::diagnostics::{Diagnostic, diagnostic_manager};

const DIAGNOSTIC_CONTEXT: &str = "ast_to_hir";

/// Generator for transforming AST to HIR (High-level IR)
///
/// NOTE: This is transitioning from stateful to share-nothing architecture.
/// The generator now supports lossy mode and will gradually become more pure.
pub struct HirGenerator {
    next_hir_id: hir::HirId,
    next_def_id: hir::DefId,
    current_file: FileId,
    current_position: u32,
    type_scopes: Vec<HashMap<String, hir::Res>>,
    value_scopes: Vec<HashMap<String, hir::Res>>,
    module_path: Vec<String>,
    module_visibility: Vec<bool>,
    global_value_defs: HashMap<String, SymbolEntry>,
    global_type_defs: HashMap<String, SymbolEntry>,
    preassigned_def_ids: HashMap<usize, hir::DefId>,
    enum_variant_def_ids: HashMap<String, hir::DefId>,
    type_aliases: HashMap<String, ast::Ty>,
    struct_field_defs: HashMap<hir::DefId, Vec<ast::StructuralField>>,
    trait_defs: HashMap<String, ast::ItemDefTrait>,
    structural_value_defs: HashMap<String, StructuralValueDef>,
    const_list_length_scopes: Vec<HashMap<String, usize>>,
    synthetic_items: Vec<hir::Item>,
    module_defs: HashSet<Vec<String>>,
    program_def_map: HashMap<hir::DefId, hir::Item>,
}

enum MaterializedTypeAlias {
    Struct(ast::TypeStruct),
    Structural(ast::TypeStructural),
    Enum(ast::TypeEnum),
}

#[derive(Debug, Clone)]
struct SymbolEntry {
    res: hir::Res,
    export: SymbolExport,
}

#[derive(Debug, Clone)]
struct StructuralValueDef {
    name: String,
    def_id: hir::DefId,
    fields: Vec<StructuralFieldSpec>,
}

#[derive(Debug, Clone, PartialEq)]
struct StructuralFieldSpec {
    name: String,
    ty: LiteralTypeKind,
}

#[derive(Debug, Clone)]
enum SymbolExport {
    Public,
    Scoped(Vec<String>),
}

impl SymbolExport {
    fn can_access(&self, current_module: &[String]) -> bool {
        match self {
            SymbolExport::Public => true,
            SymbolExport::Scoped(scope) => current_module.starts_with(scope.as_slice()),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum PathResolutionScope {
    Value,
    Type,
}

#[derive(Debug, Clone, PartialEq)]
enum LiteralTypeKind {
    Primitive(ast::TypePrimitive),
    Unit,
    Null,
}

#[derive(Debug, Clone)]
struct ImportBinding {
    target: Vec<String>,
    alias: Option<String>,
}

impl HirGenerator {
    fn add_error(&mut self, diag: Diagnostic) {
        diagnostic_manager().add_diagnostic(diag);
    }

    fn add_warning(&mut self, diag: Diagnostic) {
        diagnostic_manager().add_diagnostic(diag);
    }

    fn normalize_span(&self, span: Span) -> Span {
        span
    }

    fn handle_import(&mut self, _import: &ast::ItemImport) -> Result<()> {
        let mut bindings = Vec::new();
        self.collect_imports(Vec::new(), &_import.tree, &mut bindings)?;
        for binding in bindings {
            self.register_import_binding(binding, &_import.visibility);
        }
        Ok(())
    }

    fn collect_imports(
        &self,
        base: Vec<String>,
        tree: &ast::ItemImportTree,
        out: &mut Vec<ImportBinding>,
    ) -> Result<()> {
        match tree {
            ast::ItemImportTree::Path(path) => self.collect_imports_from_path(base, path, out),
            ast::ItemImportTree::Ident(ident) => {
                let mut target = base;
                target.push(ident.name.clone());
                out.push(ImportBinding {
                    target,
                    alias: None,
                });
                Ok(())
            }
            ast::ItemImportTree::Rename(rename) => {
                let mut target = base;
                target.push(rename.from.name.clone());
                out.push(ImportBinding {
                    target,
                    alias: Some(rename.to.name.clone()),
                });
                Ok(())
            }
            ast::ItemImportTree::Group(group) => {
                for item in &group.items {
                    self.collect_imports(base.clone(), item, out)?;
                }
                Ok(())
            }
            ast::ItemImportTree::Root
            | ast::ItemImportTree::SelfMod
            | ast::ItemImportTree::SuperMod
            | ast::ItemImportTree::Crate
            | ast::ItemImportTree::Glob => Ok(()),
        }
    }

    fn collect_imports_from_path(
        &self,
        base: Vec<String>,
        path: &ast::ItemImportPath,
        out: &mut Vec<ImportBinding>,
    ) -> Result<()> {
        let mut prefix = base;
        for seg in &path.segments {
            match seg {
                ast::ItemImportTree::Root | ast::ItemImportTree::Crate => {
                    prefix.clear();
                }
                ast::ItemImportTree::SelfMod => {
                    prefix = self.module_path.clone();
                }
                ast::ItemImportTree::SuperMod => {
                    prefix = self.module_path.clone();
                    prefix.pop();
                }
                ast::ItemImportTree::Ident(ident) => {
                    prefix.push(ident.name.clone());
                }
                ast::ItemImportTree::Rename(rename) => {
                    let mut target = prefix.clone();
                    target.push(rename.from.name.clone());
                    out.push(ImportBinding {
                        target,
                        alias: Some(rename.to.name.clone()),
                    });
                    return Ok(());
                }
                ast::ItemImportTree::Group(group) => {
                    for item in &group.items {
                        self.collect_imports(prefix.clone(), item, out)?;
                    }
                    return Ok(());
                }
                ast::ItemImportTree::Path(nested) => {
                    self.collect_imports_from_path(prefix.clone(), nested, out)?;
                    return Ok(());
                }
                ast::ItemImportTree::Glob => {
                    return Ok(());
                }
            }
        }

        if !prefix.is_empty() {
            out.push(ImportBinding {
                target: prefix,
                alias: None,
            });
        }
        Ok(())
    }

    fn register_import_binding(&mut self, binding: ImportBinding, visibility: &ast::Visibility) {
        let alias = binding
            .alias
            .clone()
            .unwrap_or_else(|| binding.target.last().cloned().unwrap_or_default());
        if alias.is_empty() {
            return;
        }
        if self.module_defs.contains(&binding.target) {
            self.current_value_scope()
                .insert(alias.clone(), hir::Res::Module(binding.target.clone()));
            return;
        }
        let key = binding.target.join("::");
        if let Some(res) = self.lookup_symbol(&key, &self.global_value_defs) {
            self.current_value_scope()
                .insert(alias.clone(), res.clone());
            self.record_value_symbol(&alias, res, visibility);
        }
        if let Some(res) = self.lookup_symbol(&key, &self.global_type_defs) {
            self.current_type_scope().insert(alias.clone(), res.clone());
            self.record_type_symbol(&alias, res, visibility);
        }
    }

    pub fn with_file<P: AsRef<Path>>(path: P) -> Self {
        let mut generator = Self::new();
        generator.reset_file_context(path);
        generator
    }

    /// Create a new HIR generator
    pub fn new() -> Self {
        Self {
            next_hir_id: 0,
            next_def_id: 0,
            current_file: 0, // Default file ID
            current_position: 0,
            type_scopes: vec![HashMap::new()],
            value_scopes: vec![HashMap::new()],
            module_path: Vec::new(),
            module_visibility: vec![true],
            global_value_defs: HashMap::new(),
            global_type_defs: HashMap::new(),
            preassigned_def_ids: HashMap::new(),
            enum_variant_def_ids: HashMap::new(),
            type_aliases: HashMap::new(),
            struct_field_defs: HashMap::new(),
            trait_defs: HashMap::new(),
            structural_value_defs: HashMap::new(),
            const_list_length_scopes: vec![HashMap::new()],
            synthetic_items: Vec::new(),
            module_defs: HashSet::new(),
            program_def_map: HashMap::new(),
        }
    }

    fn reset_file_context<P: AsRef<Path>>(&mut self, file_path: P) {
        self.current_file = fp_core::source_map::source_map()
            .file_id(file_path.as_ref())
            .unwrap_or(0);
        self.current_position = 0;
        self.type_scopes.clear();
        self.type_scopes.push(HashMap::new());
        self.value_scopes.clear();
        self.value_scopes.push(HashMap::new());
        self.module_path.clear();
        self.module_visibility.clear();
        self.module_visibility.push(true);
        self.global_value_defs.clear();
        self.global_type_defs.clear();
        self.preassigned_def_ids.clear();
        self.enum_variant_def_ids.clear();
        self.struct_field_defs.clear();
        self.module_defs.clear();
    }

    fn current_type_scope(&mut self) -> &mut HashMap<String, hir::Res> {
        self.type_scopes
            .last_mut()
            .expect("at least one type scope must exist")
    }

    fn current_value_scope(&mut self) -> &mut HashMap<String, hir::Res> {
        self.value_scopes
            .last_mut()
            .expect("at least one value scope must exist")
    }

    fn register_type_generic(&mut self, name: &str, hir_id: hir::HirId) {
        self.current_type_scope()
            .insert(name.to_string(), hir::Res::Local(hir_id));
    }

    fn register_value_def(&mut self, name: &str, def_id: hir::DefId, visibility: &ast::Visibility) {
        let res = hir::Res::Def(def_id);
        self.current_value_scope()
            .insert(name.to_string(), res.clone());
        self.record_value_symbol(name, res, visibility);
    }

    fn register_value_local(&mut self, name: &str, hir_id: hir::HirId) {
        self.current_value_scope()
            .insert(name.to_string(), hir::Res::Local(hir_id));
    }

    fn record_module_def(&mut self, name: &str) {
        let mut path = self.module_path.clone();
        path.push(name.to_string());
        self.module_defs.insert(path);
    }

    fn register_type_def(&mut self, name: &str, def_id: hir::DefId, visibility: &ast::Visibility) {
        let res = hir::Res::Def(def_id);
        self.current_type_scope()
            .insert(name.to_string(), res.clone());
        self.record_type_symbol(name, res, visibility);
    }

    fn record_value_symbol(&mut self, name: &str, res: hir::Res, visibility: &ast::Visibility) {
        let qualified = self.qualify_name(name);
        let export = self.symbol_export_marker(visibility);
        self.global_value_defs
            .insert(qualified, SymbolEntry { res, export });
    }

    fn record_type_symbol(&mut self, name: &str, res: hir::Res, visibility: &ast::Visibility) {
        let qualified = self.qualify_name(name);
        let export = self.symbol_export_marker(visibility);
        self.global_type_defs
            .insert(qualified, SymbolEntry { res, export });
    }

    fn symbol_export_marker(&self, visibility: &ast::Visibility) -> SymbolExport {
        if self.should_export(visibility) {
            SymbolExport::Public
        } else {
            SymbolExport::Scoped(self.module_path.clone())
        }
    }

    fn should_export(&self, visibility: &ast::Visibility) -> bool {
        matches!(visibility, ast::Visibility::Public) && self.current_module_visibility_flag()
    }

    fn map_visibility(&self, visibility: &ast::Visibility) -> hir::Visibility {
        match visibility {
            ast::Visibility::Public => hir::Visibility::Public,
            ast::Visibility::Crate => hir::Visibility::Private,
            ast::Visibility::Restricted(_) => hir::Visibility::Private,
            ast::Visibility::Inherited => hir::Visibility::Private,
            ast::Visibility::Private => hir::Visibility::Private,
        }
    }

    fn item_key(item: &ast::Item) -> usize {
        item as *const _ as usize
    }

    fn allocate_def_id_for_item(&mut self, item: &ast::Item) -> hir::DefId {
        let key = Self::item_key(item);
        if let Some(existing) = self.preassigned_def_ids.get(&key) {
            *existing
        } else {
            let def_id = self.next_def_id();
            self.preassigned_def_ids.insert(key, def_id);
            def_id
        }
    }

    fn def_id_for_item(&mut self, item: &ast::Item) -> hir::DefId {
        let key = Self::item_key(item);
        if let Some(id) = self.preassigned_def_ids.get(&key) {
            *id
        } else {
            self.allocate_def_id_for_item(item)
        }
    }

    fn prepare_lowering_state(&mut self) {
        self.type_scopes.clear();
        self.type_scopes.push(HashMap::new());
        self.value_scopes.clear();
        self.value_scopes.push(HashMap::new());
        self.module_path.clear();
        self.module_visibility.clear();
        self.module_visibility.push(true);
        self.next_hir_id = 0;
        self.current_position = 0;
        self.type_aliases.clear();
        self.trait_defs.clear();
        self.structural_value_defs.clear();
        self.const_list_length_scopes.clear();
        self.const_list_length_scopes.push(HashMap::new());
        self.synthetic_items.clear();
        self.module_defs.clear();
        // Keep predeclared struct fields available for struct update lowering.
    }

    fn predeclare_items(&mut self, items: &[ast::Item]) -> Result<()> {
        for item in items {
            if should_drop_quote_item(item) {
                continue;
            }
            match item.kind() {
                ItemKind::Module(module) => {
                    self.allocate_def_id_for_item(item);
                    self.record_module_def(module.name.as_str());
                    self.push_module_scope(&module.name.name, &module.visibility);
                    self.predeclare_items(&module.items)?;
                    self.pop_module_scope();
                }
                ItemKind::DefConst(def_const) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_value_def(&def_const.name.name, def_id, &def_const.visibility);
                }
                ItemKind::DefStruct(def_struct) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(&def_struct.name.name, def_id, &def_struct.visibility);
                    self.struct_field_defs
                        .insert(def_id, def_struct.value.fields.clone());
                }
                ItemKind::DefStructural(def_structural) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(
                        &def_structural.name.name,
                        def_id,
                        &def_structural.visibility,
                    );
                    self.struct_field_defs
                        .insert(def_id, def_structural.value.fields.clone());
                }
                ItemKind::DefEnum(def_enum) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(&def_enum.name.name, def_id, &def_enum.visibility);

                    for variant in &def_enum.value.variants {
                        let variant_def_id = self.next_def_id();
                        self.register_value_def(
                            &variant.name.name,
                            variant_def_id,
                            &def_enum.visibility,
                        );

                        let qualified_variant =
                            format!("{}::{}", def_enum.name.name, variant.name.name);
                        let fully_qualified = self.qualify_name(&qualified_variant);
                        self.record_value_symbol(
                            &qualified_variant,
                            hir::Res::Def(variant_def_id),
                            &def_enum.visibility,
                        );
                        self.enum_variant_def_ids
                            .insert(fully_qualified, variant_def_id);
                    }
                }
                ItemKind::DefFunction(def_fn) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_value_def(&def_fn.name.name, def_id, &def_fn.visibility);
                }
                ItemKind::DefTrait(def_trait) => {
                    let def_id = self.allocate_def_id_for_item(item);
                    self.register_type_def(&def_trait.name.name, def_id, &def_trait.visibility);
                    self.trait_defs
                        .insert(def_trait.name.name.clone(), def_trait.clone());
                }
                ItemKind::DefType(def_type) => {
                    self.register_type_alias(&def_type.name.name, &def_type.value);
                    if let Some(materialized) = self.materialized_type_alias(def_type) {
                        let def_id = self.allocate_def_id_for_item(item);
                        self.register_type_def(&def_type.name.name, def_id, &def_type.visibility);
                        match materialized {
                            MaterializedTypeAlias::Struct(struct_ty) => {
                                self.struct_field_defs
                                    .insert(def_id, struct_ty.fields.clone());
                            }
                            MaterializedTypeAlias::Structural(structural) => {
                                self.struct_field_defs
                                    .insert(def_id, structural.fields.clone());
                            }
                            MaterializedTypeAlias::Enum(enum_ty) => {
                                for variant in &enum_ty.variants {
                                    let variant_def_id = self.next_def_id();
                                    self.register_value_def(
                                        &variant.name.name,
                                        variant_def_id,
                                        &def_type.visibility,
                                    );

                                    let qualified_variant =
                                        format!("{}::{}", def_type.name.name, variant.name.name);
                                    let fully_qualified = self.qualify_name(&qualified_variant);
                                    self.record_value_symbol(
                                        &qualified_variant,
                                        hir::Res::Def(variant_def_id),
                                        &def_type.visibility,
                                    );
                                    self.enum_variant_def_ids
                                        .insert(fully_qualified, variant_def_id);
                                }
                            }
                        }
                    }
                }
                ItemKind::Impl(_) => {
                    self.allocate_def_id_for_item(item);
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn current_module_visibility_flag(&self) -> bool {
        *self.module_visibility.last().unwrap_or(&true)
    }

    fn compute_child_visibility(&self, visibility: &ast::Visibility) -> bool {
        match visibility {
            ast::Visibility::Public => self.current_module_visibility_flag(),
            _ => false,
        }
    }

    fn push_module_scope(&mut self, name: &str, visibility: &ast::Visibility) {
        self.module_path.push(name.to_string());
        let child_visibility = self.compute_child_visibility(visibility);
        self.module_visibility.push(child_visibility);
        self.push_type_scope();
        self.push_value_scope();
    }

    fn pop_module_scope(&mut self) {
        self.pop_value_scope();
        self.pop_type_scope();
        self.module_path.pop();
        self.module_visibility.pop();
        if self.module_visibility.is_empty() {
            self.module_visibility.push(true);
        }
    }

    fn qualify_name(&self, name: &str) -> String {
        if self.module_path.is_empty() {
            name.to_string()
        } else {
            let mut qualified = self.module_path.join("::");
            qualified.push_str("::");
            qualified.push_str(name);
            qualified
        }
    }

    fn lookup_symbol(&self, key: &str, map: &HashMap<String, SymbolEntry>) -> Option<hir::Res> {
        map.get(key).and_then(|entry| {
            if entry.export.can_access(&self.module_path) {
                Some(entry.res.clone())
            } else {
                None
            }
        })
    }

    fn resolve_type_symbol(&self, name: &str) -> Option<hir::Res> {
        self.type_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
            .or_else(|| self.lookup_symbol(name, &self.global_type_defs))
    }

    fn resolve_value_symbol(&self, name: &str) -> Option<hir::Res> {
        self.value_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
            .or_else(|| self.lookup_symbol(name, &self.global_value_defs))
    }

    fn push_value_scope(&mut self) {
        self.value_scopes.push(HashMap::new());
        self.const_list_length_scopes.push(HashMap::new());
    }

    fn pop_value_scope(&mut self) {
        self.value_scopes.pop();
        if self.value_scopes.is_empty() {
            self.value_scopes.push(HashMap::new());
        }
        self.const_list_length_scopes.pop();
        if self.const_list_length_scopes.is_empty() {
            self.const_list_length_scopes.push(HashMap::new());
        }
    }

    fn push_type_scope(&mut self) {
        self.type_scopes.push(HashMap::new());
    }

    fn pop_type_scope(&mut self) {
        self.type_scopes.pop();
        if self.type_scopes.is_empty() {
            self.type_scopes.push(HashMap::new());
        }
    }

    /// Create a span for the current position
    fn create_span(&mut self, length: u32) -> Span {
        let span = Span::new(
            self.current_file,
            self.current_position,
            self.current_position + length,
        );
        self.current_position += length;
        span
    }

    /// Transform an AST expression tree to HIR
    pub fn transform_expr(&mut self, ast_expr: &ast::Expr) -> Result<hir::Program> {
        let mut hir_program = hir::Program::new();

        // Transform the root expression into a main function
        let main_body = self.transform_expr_to_hir(ast_expr)?;
        let main_fn = self.create_main_function(main_body)?;

        // Add main function to program
        let main_item = hir::Item {
            hir_id: self.next_id(),
            def_id: self.next_def_id(),
            visibility: hir::Visibility::Public,
            kind: hir::ItemKind::Function(main_fn),
            span: self.create_span(4), // Span for "main" function
        };

        hir_program.items.push(main_item);

        Ok(hir_program)
    }

    /// Transform a parsed AST file into HIR
    pub fn transform_file(&mut self, file: &ast::File) -> Result<hir::Program> {
        let mut lowered = file.clone();
        let closure_diagnostics = lower_closures_in_file(&mut lowered)?;
        diagnostic_manager().add_diagnostics(closure_diagnostics);
        self.transform_file_inner(&lowered)
    }

    fn transform_file_inner(&mut self, file: &ast::File) -> Result<hir::Program> {
        self.reset_file_context(&file.path);
        self.prepare_lowering_state();
        self.predeclare_items(&file.items)?;
        let mut program = hir::Program::new();
        self.program_def_map = HashMap::new();
        for item in &self.synthetic_items {
            self.program_def_map.insert(item.def_id, item.clone());
        }

        for item in &file.items {
            self.append_item(&mut program, item)?;
        }

        if !self.synthetic_items.is_empty() {
            let mut synthetic = std::mem::take(&mut self.synthetic_items);
            for item in &synthetic {
                program.def_map.insert(item.def_id, item.clone());
                self.program_def_map.insert(item.def_id, item.clone());
            }
            program.items.splice(0..0, synthetic.drain(..));
        }

        self.program_def_map.extend(program.def_map.clone());
        Ok(program)
    }

    fn append_item(&mut self, program: &mut hir::Program, item: &ast::Item) -> Result<()> {
        if should_drop_quote_item(item) {
            return Ok(());
        }
        match item.kind() {
            ItemKind::Module(module) => {
                self.push_module_scope(&module.name.name, &module.visibility);
                for child in &module.items {
                    self.append_item(program, child)?;
                }
                self.pop_module_scope();
                Ok(())
            }
            ItemKind::Import(import) => {
                self.handle_import(import)?;
                Ok(())
            }
            ItemKind::DefType(def_type) => {
                self.register_type_alias(&def_type.name.name, &def_type.value);
                if let Some(hir_item) = self.materialize_def_type_item(item, def_type)? {
                    program.def_map.insert(hir_item.def_id, hir_item.clone());
                    self.program_def_map
                        .insert(hir_item.def_id, hir_item.clone());
                    program.items.push(hir_item);
                }
                Ok(())
            }
            ItemKind::Expr(expr) => {
                if let ast::ExprKind::Value(value) = expr.kind() {
                    if matches!(value.as_ref(), ast::Value::Unit(_)) {
                        return Ok(());
                    }
                }
                self.add_warning(
                    Diagnostic::warning(
                        "dropping unsupported module-level expression item".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(item.span()),
                );
                Ok(())
            }
            ItemKind::DeclFunction(_) => Ok(()),
            ItemKind::Macro(_) => {
                self.add_warning(
                    Diagnostic::warning(
                        "dropping macro item during AST→HIR in lossy mode".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(item.span()),
                );
                Ok(())
            }
            _ => {
                let hir_item = self.transform_item_to_hir(item)?;
                program.def_map.insert(hir_item.def_id, hir_item.clone());
                self.program_def_map
                    .insert(hir_item.def_id, hir_item.clone());
                program.items.push(hir_item);
                Ok(())
            }
        }
    }

    // Expression lowering helpers live in expressions.rs
    /// Transform an AST item into a HIR statement
    fn transform_item_to_hir_stmt(&mut self, item: &ast::BItem) -> Result<hir::StmtKind> {
        if should_drop_quote_item(item.as_ref()) {
            let unit_block = hir::Block {
                hir_id: self.next_id(),
                stmts: Vec::new(),
                expr: None,
            };
            let unit_expr = hir::Expr {
                hir_id: self.next_id(),
                kind: hir::ExprKind::Block(unit_block),
                span: self.create_span(1),
            };
            return Ok(hir::StmtKind::Expr(unit_expr));
        }

        match item.as_ref().kind() {
            ItemKind::Import(import) => {
                self.handle_import(import)?;
                let unit_block = hir::Block {
                    hir_id: self.next_id(),
                    stmts: Vec::new(),
                    expr: None,
                };
                let unit_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Block(unit_block),
                    span: self.create_span(1),
                };
                Ok(hir::StmtKind::Expr(unit_expr))
            }
            ItemKind::DefType(def_type) => {
                self.register_type_alias(&def_type.name.name, &def_type.value);
                if let Some(hir_item) = self.materialize_def_type_item(item.as_ref(), def_type)? {
                    Ok(hir::StmtKind::Item(hir_item))
                } else {
                    let unit_block = hir::Block {
                        hir_id: self.next_id(),
                        stmts: Vec::new(),
                        expr: None,
                    };
                    let unit_expr = hir::Expr {
                        hir_id: self.next_id(),
                        kind: hir::ExprKind::Block(unit_block),
                        span: self.create_span(1),
                    };
                    Ok(hir::StmtKind::Expr(unit_expr))
                }
            }
            ItemKind::Macro(_) => {
                self.add_warning(
                    Diagnostic::warning(
                        "dropping macro item in statement position during AST→HIR".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(item.span()),
                );
                let unit_block = hir::Block {
                    hir_id: self.next_id(),
                    stmts: Vec::new(),
                    expr: None,
                };
                let unit_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Block(unit_block),
                    span: self.create_span(1),
                };
                Ok(hir::StmtKind::Expr(unit_expr))
            }
            _ => {
                let hir_item = self.transform_item_to_hir(item.as_ref())?;
                Ok(hir::StmtKind::Item(hir_item))
            }
        }
    }

    /// Transform an AST item into a HIR item
    fn transform_item_to_hir(&mut self, item: &ast::Item) -> Result<hir::Item> {
        let hir_id = self.next_id();
        let def_id = self.def_id_for_item(item);
        let span = self.create_span(1);

        let (kind, visibility) = match item.kind() {
            ItemKind::DefConst(const_def) => {
                self.register_value_def(&const_def.name.name, def_id, &const_def.visibility);
                let hir_const = self.transform_const_def(const_def)?;
                (
                    hir::ItemKind::Const(hir_const),
                    self.map_visibility(&const_def.visibility),
                )
            }
            ItemKind::DefStruct(struct_def) => {
                self.register_type_def(&struct_def.name.name, def_id, &struct_def.visibility);
                self.push_type_scope();
                let generics = self.transform_generics(&struct_def.value.generics_params);
                let name = hir::Symbol::new(self.qualify_name(&struct_def.name.name));
                let fields = struct_def
                    .value
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: self.transform_type_to_hir(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                self.pop_type_scope();

                (
                    hir::ItemKind::Struct(hir::Struct {
                        name,
                        fields,
                        generics,
                    }),
                    self.map_visibility(&struct_def.visibility),
                )
            }
            ItemKind::DefStructural(struct_def) => {
                self.register_type_def(&struct_def.name.name, def_id, &struct_def.visibility);
                let name = hir::Symbol::new(self.qualify_name(&struct_def.name.name));
                let fields = struct_def
                    .value
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: self.transform_type_to_hir(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                (
                    hir::ItemKind::Struct(hir::Struct {
                        name,
                        fields,
                        generics: hir::Generics::default(),
                    }),
                    self.map_visibility(&struct_def.visibility),
                )
            }
            ItemKind::DefEnum(enum_def) => {
                self.register_type_def(&enum_def.name.name, def_id, &enum_def.visibility);
                self.push_type_scope();
                let generics = self.transform_generics(&enum_def.value.generics_params);
                let qualified_enum_name = hir::Symbol::new(self.qualify_name(&enum_def.name.name));

                let variants = enum_def
                    .value
                    .variants
                    .iter()
                    .map(|variant| {
                        let qualified_variant =
                            format!("{}::{}", enum_def.name.name, variant.name.name);
                        let fully_qualified = self.qualify_name(&qualified_variant);

                        let variant_def_id = if let Some(def_id) =
                            self.enum_variant_def_ids.get(&fully_qualified).copied()
                        {
                            def_id
                        } else {
                            let new_id = self.next_def_id();
                            self.enum_variant_def_ids
                                .insert(fully_qualified.clone(), new_id);
                            new_id
                        };

                        self.register_value_def(
                            &variant.name.name,
                            variant_def_id,
                            &enum_def.visibility,
                        );
                        self.record_value_symbol(
                            &qualified_variant,
                            hir::Res::Def(variant_def_id),
                            &enum_def.visibility,
                        );

                        let discriminant = variant
                            .discriminant
                            .as_ref()
                            .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                            .transpose()?;
                        let payload = match &variant.value {
                            ast::Ty::Unit(_) => {
                                if let Some(alias) =
                                    self.lookup_type_alias(&[variant.name.name.clone()])
                                {
                                    let alias = alias.clone();
                                    Some(self.transform_type_to_hir(&alias)?)
                                } else {
                                    None
                                }
                            }
                            other => Some(self.transform_type_to_hir(other)?),
                        };

                        Ok(hir::EnumVariant {
                            hir_id: self.next_id(),
                            def_id: variant_def_id,
                            name: hir::Symbol::new(variant.name.name.clone()),
                            discriminant,
                            payload,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                self.pop_type_scope();

                (
                    hir::ItemKind::Enum(hir::Enum {
                        name: qualified_enum_name,
                        variants,
                        generics,
                    }),
                    self.map_visibility(&enum_def.visibility),
                )
            }
            ItemKind::DefFunction(func_def) => {
                self.register_value_def(&func_def.name.name, def_id, &func_def.visibility);
                let function = self.transform_function(func_def, None)?;
                (
                    hir::ItemKind::Function(function),
                    self.map_visibility(&func_def.visibility),
                )
            }
            ItemKind::Impl(impl_block) => {
                let hir_impl = self.transform_impl(impl_block)?;
                (hir::ItemKind::Impl(hir_impl), hir::Visibility::Private)
            }
            ItemKind::DefType(def_type) => {
                self.register_type_alias(&def_type.name.name, &def_type.value);
                let unit_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Literal(hir::Lit::Bool(false)),
                    span: self.create_span(1),
                };
                let body = hir::Body {
                    hir_id: self.next_id(),
                    params: Vec::new(),
                    value: unit_expr,
                };
                let konst = hir::Const {
                    name: hir::Symbol::new(self.qualify_name(&def_type.name.name)),
                    ty: self.create_simple_type("bool"),
                    body,
                };
                (hir::ItemKind::Const(konst), hir::Visibility::Private)
            }
            ItemKind::DefTrait(def_trait) => {
                let unit_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Literal(hir::Lit::Bool(false)),
                    span: self.create_span(1),
                };
                let body = hir::Body {
                    hir_id: self.next_id(),
                    params: Vec::new(),
                    value: unit_expr,
                };
                let konst = hir::Const {
                    name: hir::Symbol::new(self.qualify_name(&def_trait.name.name)),
                    ty: self.create_simple_type("bool"),
                    body,
                };
                (
                    hir::ItemKind::Const(konst),
                    self.map_visibility(&def_trait.visibility),
                )
            }
            _ => {
                self.add_error(
                    Diagnostic::error(format!(
                        "Unimplemented AST item type for HIR transformation: {:?}",
                        item
                    ))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(item.span()),
                );
                let unit_expr = hir::Expr {
                    hir_id: self.next_id(),
                    kind: hir::ExprKind::Literal(hir::Lit::Bool(false)),
                    span: self.create_span(1),
                };
                let body = hir::Body {
                    hir_id: self.next_id(),
                    params: Vec::new(),
                    value: unit_expr,
                };
                let konst = hir::Const {
                    name: hir::Symbol::new(format!("__fp_error_{def_id}")),
                    ty: self.create_simple_type("bool"),
                    body,
                };
                return Ok(hir::Item {
                    hir_id,
                    def_id,
                    visibility: hir::Visibility::Private,
                    kind: hir::ItemKind::Const(konst),
                    span,
                });
            }
        };

        Ok(hir::Item {
            hir_id,
            def_id,
            visibility,
            kind,
            span,
        })
    }

    fn transform_const_def(&mut self, const_def: &ast::ItemDefConst) -> Result<hir::Const> {
        let list_len = self.const_list_length_from_expr(&const_def.value);
        if let Some(len) = list_len {
            self.record_const_list_length(&const_def.name.name, len);
        }
        let ty = if let Some(ty) = &const_def.ty {
            if let (ast::Ty::Vec(vec_ty), Some(len)) = (ty, list_len) {
                let len_expr =
                    ast::Expr::new(ast::ExprKind::Value(Box::new(ast::Value::int(len as i64))));
                let array_ty = ast::Ty::Array(ast::TypeArray {
                    elem: vec_ty.ty.clone(),
                    len: Box::new(len_expr),
                });
                self.transform_type_to_hir(&array_ty)?
            } else {
                self.transform_type_to_hir(ty)?
            }
        } else {
            self.create_unit_type()
        };

        let value = self.transform_expr_to_hir(&const_def.value)?;
        let body = hir::Body {
            hir_id: self.next_id(),
            params: Vec::new(),
            value,
        };

        Ok(hir::Const {
            name: hir::Symbol::new(self.qualify_name(&const_def.name.name)),
            ty,
            body,
        })
    }

    fn const_list_length_from_expr(&self, expr: &ast::Expr) -> Option<usize> {
        match expr.kind() {
            ast::ExprKind::Array(array) => Some(array.values.len()),
            ast::ExprKind::Value(value) => match value.as_ref() {
                ast::Value::List(list) => Some(list.values.len()),
                _ => None,
            },
            _ => None,
        }
    }

    fn record_const_list_length(&mut self, name: &str, len: usize) {
        if let Some(scope) = self.const_list_length_scopes.last_mut() {
            scope.insert(name.to_string(), len);
        }
    }

    fn lookup_const_list_length(&self, segments: &[ast::Ident]) -> Option<usize> {
        if segments.len() != 1 {
            return None;
        }
        let name = segments[0].name.as_str();
        self.const_list_length_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    /// Transform an AST type into a HIR type
    fn transform_type_to_hir(&mut self, ty: &ast::Ty) -> Result<hir::TypeExpr> {
        match ty {
            ast::Ty::Primitive(prim) => Ok(self.primitive_type_to_hir(*prim)),
            ast::Ty::Struct(struct_ty) => {
                if let Some(alias) = self.lookup_type_alias(&[struct_ty.name.name.to_string()]) {
                    let alias = alias.clone();
                    return self.transform_type_to_hir(&alias);
                }
                let path = self.locator_to_hir_path_with_scope(
                    &Locator::Ident(struct_ty.name.clone()),
                    PathResolutionScope::Type,
                )?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Path(path),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Reference(reference) => {
                let inner = self.transform_type_to_hir(&reference.ty)?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Ref(Box::new(inner)),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Unit(_) => Ok(self.create_unit_type()),
            ast::Ty::Nothing(_) => Ok(self.create_null_type()),
            ast::Ty::Any(_) => Ok(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Infer,
                Span::new(self.current_file, 0, 0),
            )),
            ast::Ty::Unknown(_) => Ok(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Infer,
                Span::new(self.current_file, 0, 0),
            )),
            ast::Ty::Tuple(tuple) => {
                let elements = tuple
                    .types
                    .iter()
                    .map(|ty| Ok(Box::new(self.transform_type_to_hir(ty)?)))
                    .collect::<Result<Vec<_>>>()?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Tuple(elements),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Structural(structural) => {
                let fields = structural
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::TypeStructuralField {
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: Box::new(self.transform_type_to_hir(&field.value)?),
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Structural(hir::TypeStructural { fields }),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Vec(vec_ty) => {
                let elem = Box::new(self.transform_type_to_hir(&vec_ty.ty)?);
                let args = hir::GenericArgs {
                    args: vec![hir::GenericArg::Type(elem)],
                };
                let path = hir::Path {
                    segments: vec![self.make_path_segment("Vec", Some(args))],
                    res: None,
                };
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Path(path),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Array(array_ty) => {
                let elem = Box::new(self.transform_type_to_hir(&array_ty.elem)?);
                let len_expr = Box::new(self.transform_expr_to_hir(array_ty.len.as_ref())?);
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Array(elem, Some(len_expr)),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Slice(slice_ty) => {
                let elem = Box::new(self.transform_type_to_hir(&slice_ty.elem)?);
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Slice(elem),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::TypeBinaryOp(type_op) => {
                if let Some(kind) = self.literal_type_kind(ty) {
                    let expr = match kind {
                        LiteralTypeKind::Primitive(prim) => self.primitive_type_to_hir(prim),
                        LiteralTypeKind::Unit => self.create_unit_type(),
                        LiteralTypeKind::Null => self.create_null_type(),
                    };
                    return Ok(expr);
                }
                let lhs = self.transform_type_to_hir(&type_op.lhs)?;
                let rhs = self.transform_type_to_hir(&type_op.rhs)?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::TypeBinaryOp(hir::TypeBinaryOp {
                        kind: type_op.kind,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Value(type_value) => {
                let expr = match type_value.value.as_ref() {
                    ast::Value::Int(_) => {
                        self.primitive_type_to_hir(ast::TypePrimitive::Int(ast::TypeInt::I64))
                    }
                    ast::Value::Bool(_) => self.primitive_type_to_hir(ast::TypePrimitive::Bool),
                    ast::Value::Decimal(_) => self
                        .primitive_type_to_hir(ast::TypePrimitive::Decimal(ast::DecimalType::F64)),
                    ast::Value::String(_) => self.primitive_type_to_hir(ast::TypePrimitive::String),
                    ast::Value::Char(_) => self.primitive_type_to_hir(ast::TypePrimitive::Char),
                    ast::Value::Unit(_) => self.create_unit_type(),
                    ast::Value::Null(_) | ast::Value::None(_) => self.create_null_type(),
                    ast::Value::Type(ty) => {
                        return self.transform_type_to_hir(ty);
                    }
                    other => {
                        self.add_error(
                            Diagnostic::error(format!(
                                "unsupported literal type in AST→HIR lowering: {:?}",
                                other
                            ))
                            .with_source_context(DIAGNOSTIC_CONTEXT)
                            .with_span(ty.span()),
                        );
                        return Ok(hir::TypeExpr::new(
                            self.next_id(),
                            hir::TypeExprKind::Error,
                            Span::new(self.current_file, 0, 0),
                        ));
                    }
                };
                Ok(expr)
            }
            ast::Ty::Quote(_) => {
                self.add_error(
                    Diagnostic::error(
                        "quote token types should be removed by const-eval".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(self.normalize_span(ty.span())),
                );
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Error,
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::Expr(expr) => {
                if let ast::ExprKind::Value(value) = expr.kind() {
                    match value.as_ref() {
                        ast::Value::Type(ty) => {
                            return self.transform_type_to_hir(ty);
                        }
                        ast::Value::Expr(inner) => {
                            if let Ok(path) =
                                self.ast_expr_to_hir_path(inner, PathResolutionScope::Type)
                            {
                                return Ok(hir::TypeExpr::new(
                                    self.next_id(),
                                    hir::TypeExprKind::Path(path),
                                    Span::new(self.current_file, 0, 0),
                                ));
                            }
                        }
                        _ => {}
                    }
                }
                if let Ok(path) = self.ast_expr_to_hir_path(expr, PathResolutionScope::Type) {
                    let segments = path
                        .segments
                        .iter()
                        .map(|seg| seg.name.as_str().to_string())
                        .collect::<Vec<_>>();
                    if let Some(alias) = self.lookup_type_alias(&segments) {
                        let alias = alias.clone();
                        return self.transform_type_to_hir(&alias);
                    }
                    return Ok(hir::TypeExpr::new(
                        self.next_id(),
                        hir::TypeExprKind::Path(path),
                        Span::new(self.current_file, 0, 0),
                    ));
                }
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Error,
                    Span::new(self.current_file, 0, 0),
                ))
            }
            ast::Ty::ImplTraits(_) => Ok(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Infer,
                Span::new(self.current_file, 0, 0),
            )),
            ast::Ty::Function(fn_ty) => {
                let inputs = fn_ty
                    .params
                    .iter()
                    .map(|ty| self.transform_type_to_hir(ty).map(Box::new))
                    .collect::<Result<Vec<_>>>()?;

                let output = if let Some(ret_ty) = &fn_ty.ret_ty {
                    Box::new(self.transform_type_to_hir(ret_ty)?)
                } else {
                    Box::new(self.create_unit_type())
                };

                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::FnPtr(hir::FnPtrType { inputs, output }),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            unsupported => {
                self.add_warning(
                    Diagnostic::warning(format!(
                        "unsupported type in AST→HIR lowering: {:?}",
                        unsupported
                    ))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(self.normalize_span(unsupported.span())),
                );
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Error,
                    Span::new(self.current_file, 0, 0),
                ))
            }
        }
    }

    /// Create a simple HIR literal expression
    pub fn create_simple_literal(&mut self, value: i64) -> hir::Expr {
        hir::Expr::new(
            self.next_id(),
            hir::ExprKind::Literal(hir::Lit::Integer(value)),
            Span::new(0, 0, 0),
        )
    }

    /// Create a simple HIR type
    pub fn create_simple_type(&mut self, type_name: &str) -> hir::TypeExpr {
        hir::TypeExpr::new(
            self.next_id(),
            hir::TypeExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: hir::Symbol::new(type_name),
                    args: None,
                }],
                res: None,
            }),
            Span::new(0, 0, 0),
        )
    }

    fn create_unit_type(&mut self) -> hir::TypeExpr {
        hir::TypeExpr::new(
            self.next_id(),
            hir::TypeExprKind::Tuple(Vec::new()),
            Span::new(self.current_file, 0, 0),
        )
    }

    fn create_null_type(&mut self) -> hir::TypeExpr {
        hir::TypeExpr::new(
            self.next_id(),
            hir::TypeExprKind::Path(hir::Path {
                segments: vec![hir::PathSegment {
                    name: hir::Symbol::new("null"),
                    args: None,
                }],
                res: None,
            }),
            Span::new(self.current_file, 0, 0),
        )
    }

    fn literal_type_kind(&self, ty: &ast::Ty) -> Option<LiteralTypeKind> {
        match ty {
            ast::Ty::Value(type_value) => match type_value.value.as_ref() {
                ast::Value::Int(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Int(
                    ast::TypeInt::I64,
                ))),
                ast::Value::Bool(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Bool)),
                ast::Value::Decimal(_) => Some(LiteralTypeKind::Primitive(
                    ast::TypePrimitive::Decimal(ast::DecimalType::F64),
                )),
                ast::Value::String(_) => {
                    Some(LiteralTypeKind::Primitive(ast::TypePrimitive::String))
                }
                ast::Value::Char(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Char)),
                ast::Value::Unit(_) => Some(LiteralTypeKind::Unit),
                ast::Value::Null(_) | ast::Value::None(_) => Some(LiteralTypeKind::Null),
                _ => None,
            },
            ast::Ty::TypeBinaryOp(op) if matches!(op.kind, ast::TypeBinaryOpKind::Union) => {
                let lhs = self.literal_type_kind(&op.lhs)?;
                let rhs = self.literal_type_kind(&op.rhs)?;
                if lhs == rhs {
                    Some(lhs)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn literal_type_kind_from_value(&self, value: &ast::Value) -> Option<LiteralTypeKind> {
        match value {
            ast::Value::Int(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Int(
                ast::TypeInt::I64,
            ))),
            ast::Value::Bool(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Bool)),
            ast::Value::Decimal(_) => Some(LiteralTypeKind::Primitive(
                ast::TypePrimitive::Decimal(ast::DecimalType::F64),
            )),
            ast::Value::String(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::String)),
            ast::Value::Char(_) => Some(LiteralTypeKind::Primitive(ast::TypePrimitive::Char)),
            ast::Value::Unit(_) => Some(LiteralTypeKind::Unit),
            ast::Value::Null(_) | ast::Value::None(_) => Some(LiteralTypeKind::Null),
            ast::Value::Type(ty) => self.literal_type_kind(ty),
            _ => None,
        }
    }

    fn structural_field_value_to_ty(&mut self, value: &ast::Value) -> ast::Ty {
        if let ast::Value::Type(ty) = value {
            return ty.clone();
        }
        if let Some(ty) = self.literal_type_kind_from_value(value).and_then(|kind| match kind {
            LiteralTypeKind::Primitive(prim) => Some(ast::Ty::Primitive(prim)),
            LiteralTypeKind::Unit => Some(ast::Ty::Unit(ast::TypeUnit)),
            LiteralTypeKind::Null => Some(ast::Ty::Nothing(ast::TypeNothing)),
        }) {
            return ty;
        }
        self.add_error(
            Diagnostic::error(format!(
                "unsupported structural field value type: {:?}",
                value
            ))
            .with_source_context(DIAGNOSTIC_CONTEXT)
            .with_span(value.span()),
        );
        ast::Ty::Unknown(ast::TypeUnknown)
    }

    fn structural_specs_compatible(
        &self,
        existing: &[StructuralFieldSpec],
        incoming: &[StructuralFieldSpec],
    ) -> bool {
        if existing.len() != incoming.len() {
            return false;
        }

        existing.iter().zip(incoming.iter()).all(|(lhs, rhs)| {
            if lhs.name != rhs.name {
                return false;
            }
            lhs.ty == rhs.ty
        })
    }

    fn structural_value_key(&self, fields: &[StructuralFieldSpec]) -> String {
        let mut parts = Vec::with_capacity(fields.len());
        for field in fields {
            let ty_key = match field.ty {
                LiteralTypeKind::Primitive(prim) => format!("{:?}", prim),
                LiteralTypeKind::Unit => "unit".to_string(),
                LiteralTypeKind::Null => "null".to_string(),
            };
            parts.push(format!("{}:{}", field.name, ty_key));
        }
        parts.join("|")
    }

    fn structural_value_name(&self, key: &str) -> String {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        key.hash(&mut hasher);
        format!("__structural_value_{:x}", hasher.finish())
    }

    fn find_compatible_structural_value_def(
        &self,
        fields: &[StructuralFieldSpec],
    ) -> Option<StructuralValueDef> {
        self.structural_value_defs
            .values()
            .find(|candidate| self.structural_specs_compatible(&candidate.fields, fields))
            .cloned()
    }

    fn register_structural_value_def(
        &mut self,
        name: String,
        fields: Vec<StructuralFieldSpec>,
        hir_fields: Vec<hir::StructField>,
        ast_fields: Vec<ast::StructuralField>,
    ) -> StructuralValueDef {
        let def_id = self.next_def_id();
        let name_symbol = hir::Symbol::new(self.qualify_name(&name));
        let hir_id = self.next_id();
        let span = self.create_span(1);

        let struct_item = hir::Item {
            hir_id,
            def_id,
            visibility: hir::Visibility::Private,
            kind: hir::ItemKind::Struct(hir::Struct {
                name: name_symbol,
                fields: hir_fields,
                generics: hir::Generics::default(),
            }),
            span,
        };

        self.register_type_def(&name, def_id, &ast::Visibility::Private);
        self.struct_field_defs.insert(def_id, ast_fields);
        self.synthetic_items.push(struct_item);

        StructuralValueDef {
            name,
            def_id,
            fields,
        }
    }

    fn should_update_structural_def(&self, def_id: hir::DefId) -> bool {
        let Some(fields) = self.struct_field_defs.get(&def_id) else {
            return false;
        };
        fields
            .iter()
            .all(|field| matches!(field.value, ast::Ty::Any(_) | ast::Ty::Unknown(_)))
    }

    fn update_structural_def_fields(
        &mut self,
        def_id: hir::DefId,
        hir_fields: Vec<hir::StructField>,
        ast_fields: Vec<ast::StructuralField>,
    ) {
        if let Some(item) = self
            .synthetic_items
            .iter_mut()
            .find(|item| item.def_id == def_id)
        {
            if let hir::ItemKind::Struct(strukt) = &mut item.kind {
                strukt.fields = hir_fields;
            }
        }
        self.struct_field_defs.insert(def_id, ast_fields);
    }

    fn structural_fields_from_value(
        &mut self,
        structural: &ast::ValueStructural,
    ) -> Vec<StructuralFieldSpec> {
        structural
            .fields
            .iter()
            .map(|field| {
                let ty = match self.literal_type_kind_from_value(&field.value) {
                    Some(ty) => ty,
                    None => {
                        self.add_error(
                            Diagnostic::error(format!(
                                "unsupported structural field value for HIR materialization: {:?}",
                                field.value
                            ))
                            .with_source_context(DIAGNOSTIC_CONTEXT)
                            .with_span(field.value.span()),
                        );
                        LiteralTypeKind::Null
                    }
                };
                StructuralFieldSpec {
                    name: field.name.name.clone(),
                    ty,
                }
            })
            .collect()
    }

    fn path_for_structural_def(&mut self, def: &StructuralValueDef) -> hir::Path {
        hir::Path {
            segments: vec![hir::PathSegment {
                name: hir::Symbol::new(def.name.clone()),
                args: None,
            }],
            res: Some(hir::Res::Def(def.def_id)),
        }
    }

    fn hir_type_for_value(&mut self, value: &ast::Value) -> Result<hir::TypeExpr> {
        let span = Span::new(self.current_file, 0, 0);
        let expr = match value {
            ast::Value::Int(_) => {
                self.primitive_type_to_hir(ast::TypePrimitive::Int(ast::TypeInt::I64))
            }
            ast::Value::Bool(_) => self.primitive_type_to_hir(ast::TypePrimitive::Bool),
            ast::Value::Decimal(_) => {
                self.primitive_type_to_hir(ast::TypePrimitive::Decimal(ast::DecimalType::F64))
            }
            ast::Value::String(_) => self.primitive_type_to_hir(ast::TypePrimitive::String),
            ast::Value::Char(_) => self.primitive_type_to_hir(ast::TypePrimitive::Char),
            ast::Value::Unit(_) => self.create_unit_type(),
            ast::Value::Null(_) | ast::Value::None(_) => {
                hir::TypeExpr::new(self.next_id(), hir::TypeExprKind::Infer, span)
            }
            ast::Value::Struct(struct_val) => {
                let path = self.locator_to_hir_path_with_scope(
                    &Locator::Ident(struct_val.ty.name.clone()),
                    PathResolutionScope::Type,
                )?;
                hir::TypeExpr::new(self.next_id(), hir::TypeExprKind::Path(path), span)
            }
            ast::Value::Structural(structural) => {
                let def = self.materialize_structural_value_def(structural)?;
                let path = self.path_for_structural_def(&def);
                hir::TypeExpr::new(self.next_id(), hir::TypeExprKind::Path(path), span)
            }
            ast::Value::Type(ty) => return self.transform_type_to_hir(ty),
            other => {
                self.add_error(
                    Diagnostic::error(format!(
                        "unsupported structural field value type: {:?}",
                        other
                    ))
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(value.span()),
                );
                return Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Error,
                    span,
                ));
            }
        };
        Ok(expr)
    }

    fn materialize_structural_value_def(
        &mut self,
        structural: &ast::ValueStructural,
    ) -> Result<StructuralValueDef> {
        let specs = self.structural_fields_from_value(structural);
        if let Some(def) = self.find_compatible_structural_value_def(&specs) {
            if self.should_update_structural_def(def.def_id) {
                let hir_fields = structural
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: self.hir_type_for_value(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                let ast_fields = structural
                    .fields
                    .iter()
                    .map(|field| {
                        let ty = self.structural_field_value_to_ty(&field.value);
                        Ok(ast::StructuralField::new(field.name.clone(), ty))
                    })
                    .collect::<Result<Vec<_>>>()?;

                self.update_structural_def_fields(def.def_id, hir_fields, ast_fields);
            }
            return Ok(def);
        }

        let key = self.structural_value_key(&specs);
        if let Some(def) = self.structural_value_defs.get(&key).cloned() {
            return Ok(def);
        }

        let hir_fields = structural
            .fields
            .iter()
            .map(|field| {
                Ok(hir::StructField {
                    hir_id: self.next_id(),
                    name: hir::Symbol::new(field.name.name.clone()),
                    ty: self.hir_type_for_value(&field.value)?,
                    vis: hir::Visibility::Public,
                })
            })
            .collect::<Result<Vec<_>>>()?;

        let ast_fields = structural
            .fields
            .iter()
            .map(|field| {
                let ty = self.structural_field_value_to_ty(&field.value);
                Ok(ast::StructuralField::new(field.name.clone(), ty))
            })
            .collect::<Result<Vec<_>>>()?;

        let name = self.structural_value_name(&key);
        let def = self.register_structural_value_def(name, specs, hir_fields, ast_fields);
        self.structural_value_defs.insert(key, def.clone());
        Ok(def)
    }

    fn register_type_alias(&mut self, name: &str, ty: &ast::Ty) {
        let qualified = self.qualify_name(name);
        self.type_aliases.insert(qualified, ty.clone());
    }

    fn lookup_type_alias(&self, segments: &[String]) -> Option<&ast::Ty> {
        let qualified = if segments.len() == 1 {
            self.qualify_name(&segments[0])
        } else {
            segments.join("::")
        };
        self.type_aliases
            .get(&qualified)
            .or_else(|| segments.get(0).and_then(|name| self.type_aliases.get(name)))
    }

    fn materialized_type_alias(
        &self,
        def_type: &ast::ItemDefType,
    ) -> Option<MaterializedTypeAlias> {
        match &def_type.value {
            ast::Ty::Struct(struct_ty) => Some(MaterializedTypeAlias::Struct(struct_ty.clone())),
            ast::Ty::Structural(structural) => {
                Some(MaterializedTypeAlias::Structural(structural.clone()))
            }
            ast::Ty::Enum(enum_ty) => Some(MaterializedTypeAlias::Enum(enum_ty.clone())),
            _ => None,
        }
    }

    fn materialize_def_type_item(
        &mut self,
        item: &ast::Item,
        def_type: &ast::ItemDefType,
    ) -> Result<Option<hir::Item>> {
        let def_id = self.def_id_for_item(item);
        let hir_id = self.next_id();
        let span = self.create_span(1);

        let (kind, visibility) = match self.materialized_type_alias(def_type) {
            Some(MaterializedTypeAlias::Struct(struct_ty)) => {
                self.register_type_def(&def_type.name.name, def_id, &def_type.visibility);
                self.push_type_scope();
                let generics = self.transform_generics(&struct_ty.generics_params);
                let name = hir::Symbol::new(self.qualify_name(&def_type.name.name));
                let fields = struct_ty
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: self.transform_type_to_hir(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                self.pop_type_scope();

                (
                    hir::ItemKind::Struct(hir::Struct {
                        name,
                        fields,
                        generics,
                    }),
                    self.map_visibility(&def_type.visibility),
                )
            }
            Some(MaterializedTypeAlias::Structural(structural)) => {
                self.register_type_def(&def_type.name.name, def_id, &def_type.visibility);
                let name = hir::Symbol::new(self.qualify_name(&def_type.name.name));
                let fields = structural
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: hir::Symbol::new(field.name.name.clone()),
                            ty: self.transform_type_to_hir(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                (
                    hir::ItemKind::Struct(hir::Struct {
                        name,
                        fields,
                        generics: hir::Generics::default(),
                    }),
                    self.map_visibility(&def_type.visibility),
                )
            }
            Some(MaterializedTypeAlias::Enum(enum_ty)) => {
                self.register_type_def(&def_type.name.name, def_id, &def_type.visibility);
                self.push_type_scope();
                let generics = self.transform_generics(&enum_ty.generics_params);
                let qualified_enum_name = hir::Symbol::new(self.qualify_name(&def_type.name.name));

                let variants = enum_ty
                    .variants
                    .iter()
                    .map(|variant| {
                        let qualified_variant =
                            format!("{}::{}", def_type.name.name, variant.name.name);
                        let fully_qualified = self.qualify_name(&qualified_variant);

                        let variant_def_id = if let Some(def_id) =
                            self.enum_variant_def_ids.get(&fully_qualified).copied()
                        {
                            def_id
                        } else {
                            let new_id = self.next_def_id();
                            self.enum_variant_def_ids
                                .insert(fully_qualified.clone(), new_id);
                            new_id
                        };

                        self.register_value_def(
                            &variant.name.name,
                            variant_def_id,
                            &def_type.visibility,
                        );
                        self.record_value_symbol(
                            &qualified_variant,
                            hir::Res::Def(variant_def_id),
                            &def_type.visibility,
                        );

                        let discriminant = variant
                            .discriminant
                            .as_ref()
                            .map(|expr| self.transform_expr_to_hir(expr.as_ref()))
                            .transpose()?;
                        let payload = match &variant.value {
                            ast::Ty::Unit(_) => None,
                            other => Some(self.transform_type_to_hir(other)?),
                        };

                        Ok(hir::EnumVariant {
                            hir_id: self.next_id(),
                            def_id: variant_def_id,
                            name: hir::Symbol::new(variant.name.name.clone()),
                            discriminant,
                            payload,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;
                self.pop_type_scope();

                (
                    hir::ItemKind::Enum(hir::Enum {
                        name: qualified_enum_name,
                        variants,
                        generics,
                    }),
                    self.map_visibility(&def_type.visibility),
                )
            }
            None => return Ok(None),
        };

        Ok(Some(hir::Item {
            hir_id,
            def_id,
            kind,
            visibility,
            span,
        }))
    }
}

fn should_drop_quote_item(item: &ast::Item) -> bool {
    match item.kind() {
        ItemKind::DefFunction(func) => {
            signature_contains_quote(&func.sig)
        }
        ItemKind::DeclFunction(func) => {
            signature_contains_quote(&func.sig)
        }
        ItemKind::DefConst(def) => {
            def.ty_annotation()
                .or_else(|| def.ty.as_ref())
                .is_some_and(ty_contains_quote)
                || expr_contains_quote_value(def.value.as_ref())
        }
        _ => false,
    }
}

fn signature_contains_quote(sig: &ast::FunctionSignature) -> bool {
    sig.params.iter().any(|param| ty_contains_quote(&param.ty))
        || sig.ret_ty.as_ref().is_some_and(ty_contains_quote)
}

fn ty_contains_quote(ty: &ast::Ty) -> bool {
    match ty {
        ast::Ty::Quote(_) => true,
        ast::Ty::Tuple(tuple) => tuple.types.iter().any(ty_contains_quote),
        ast::Ty::Array(array) => ty_contains_quote(&array.elem),
        ast::Ty::Vec(vec) => ty_contains_quote(&vec.ty),
        ast::Ty::Reference(reference) => ty_contains_quote(&reference.ty),
        ast::Ty::Slice(slice) => ty_contains_quote(&slice.elem),
        ast::Ty::Struct(def) => def.fields.iter().any(|field| ty_contains_quote(&field.value)),
        ast::Ty::Structural(def) => def.fields.iter().any(|field| ty_contains_quote(&field.value)),
        ast::Ty::Enum(def) => def
            .variants
            .iter()
            .any(|variant| ty_contains_quote(&variant.value)),
        ast::Ty::Function(func) => func.params.iter().any(ty_contains_quote)
            || func
                .ret_ty
                .as_ref()
                .is_some_and(|ty| ty_contains_quote(ty.as_ref())),
        ast::Ty::TypeBinaryOp(op) => {
            ty_contains_quote(&op.lhs) || ty_contains_quote(&op.rhs)
        }
        ast::Ty::TypeBounds(bounds) => bounds
            .bounds
            .iter()
            .any(|expr| expr_contains_quote_value(expr)),
        ast::Ty::Value(value) => value_contains_quote(value.value.as_ref()),
        ast::Ty::Expr(expr) => expr_contains_quote_value(expr.as_ref()),
        ast::Ty::Primitive(_)
        | ast::Ty::TokenStream(_)
        | ast::Ty::ImplTraits(_)
        | ast::Ty::Any(_)
        | ast::Ty::Unit(_)
        | ast::Ty::Unknown(_)
        | ast::Ty::Nothing(_)
        | ast::Ty::Type(_)
        | ast::Ty::AnyBox(_) => false,
    }
}

fn expr_contains_quote_value(expr: &ast::Expr) -> bool {
    if let ast::ExprKind::Value(value) = expr.kind() {
        return value_contains_quote(value.as_ref());
    }
    false
}

fn value_contains_quote(value: &ast::Value) -> bool {
    match value {
        ast::Value::QuoteToken(_) => true,
        ast::Value::List(list) => !list.values.is_empty()
            && list
                .values
                .iter()
                .all(|value| value_contains_quote(value)),
        _ => false,
    }
}

impl Default for HirGenerator {
    fn default() -> Self {
        Self::new()
    }
}

fn lower_closures_in_file(file: &mut ast::File) -> Result<Vec<Diagnostic>> {
    let mut pass = ClosureLowering::new();
    pass.find_and_transform_functions(&mut file.items)?;
    pass.rewrite_usage(&mut file.items)?;

    if !pass.generated_items.is_empty() {
        let mut new_items = pass.generated_items;
        new_items.append(&mut file.items);
        file.items = new_items;
    }
    Ok(pass.diagnostics)
}

const DUMMY_CAPTURE_NAME: &str = "__fp_no_capture";

fn expand_intrinsic_collection(expr: &mut ast::Expr) -> bool {
    if let ast::ExprKind::IntrinsicContainer(collection) = expr.kind() {
        let new_expr = collection.clone().into_const_expr();
        *expr = new_expr;
        true
    } else {
        false
    }
}

#[derive(Clone)]
struct ClosureInfo {
    env_struct_ident: ast::Ident,
    env_struct_ty: ast::Ty,
    call_fn_ident: ast::Ident,
    call_ret_ty: ast::Ty,
}

#[derive(Clone)]
struct Capture {
    name: ast::Ident,
    ty: ast::Ty,
}

struct ClosureLowering {
    counter: usize,
    function_infos: HashMap<String, ClosureInfo>,
    struct_infos: HashMap<String, ClosureInfo>,
    variable_infos: HashMap<String, ClosureInfo>,
    generated_items: Vec<ast::Item>,
    diagnostics: Vec<Diagnostic>,
}

impl ClosureLowering {
    fn new() -> Self {
        Self {
            counter: 0,
            function_infos: HashMap::new(),
            struct_infos: HashMap::new(),
            variable_infos: HashMap::new(),
            generated_items: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    fn add_error(&mut self, diag: Diagnostic) {
        self.diagnostics.push(diag);
    }

    fn find_and_transform_functions(&mut self, items: &mut [ast::Item]) -> Result<()> {
        for item in items {
            match item.kind_mut() {
                ast::ItemKind::Module(module) => {
                    self.find_and_transform_functions(&mut module.items)?;
                }
                ast::ItemKind::DefFunction(func) => {
                    if let Some(info) = self.transform_function(func)? {
                        self.function_infos
                            .insert(func.name.as_str().to_string(), info.clone());
                        self.struct_infos
                            .insert(info.env_struct_ident.as_str().to_string(), info);
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn transform_function(
        &mut self,
        func: &mut ast::ItemDefFunction,
    ) -> Result<Option<ClosureInfo>> {
        if let Some(info) = self.transform_closure_expr(func.body.as_mut())? {
            let env_ret_ty = info.env_struct_ty.clone();

            if let Some(ty_fn) = func.ty.as_mut() {
                ty_fn.ret_ty = Some(Box::new(env_ret_ty.clone()));
            }

            if func.ty.is_none() {
                func.ty = Some(ast::TypeFunction {
                    params: func
                        .sig
                        .params
                        .iter()
                        .map(|param| param.ty.clone())
                        .collect(),
                    generics_params: func.sig.generics_params.clone(),
                    ret_ty: Some(Box::new(env_ret_ty.clone())),
                });
            }

            if func.ty_annotation.is_some() || func.ty.is_some() {
                func.ty_annotation = func
                    .ty
                    .as_ref()
                    .map(|ty_fn| ast::Ty::Function(ty_fn.clone()));
            }

            if let Some(ret_slot) = func.sig.ret_ty.as_mut() {
                *ret_slot = env_ret_ty.clone();
            } else {
                func.sig.ret_ty = Some(env_ret_ty.clone());
            }

            return Ok(Some(info));
        }

        if let ast::ExprKind::Block(block) = func.body.kind_mut() {
            if let Some(last_expr) = block.last_expr_mut() {
                if let Some(info) = self.transform_closure_expr(last_expr)? {
                    return Ok(Some(info));
                }
            }
        }

        Ok(None)
    }

    fn transform_closure_expr(&mut self, expr: &mut ast::Expr) -> Result<Option<ClosureInfo>> {
        let Some(expr_ty) = expr.ty().cloned() else {
            return Ok(None);
        };
        let ast::Ty::Function(fn_ty) = expr_ty.clone() else {
            return Ok(None);
        };

        let ast::ExprKind::Closure(closure) = expr.kind_mut() else {
            return Ok(None);
        };

        let mut param_names = Vec::new();
        let mut param_set = HashSet::new();
        for param in &closure.params {
            if let ast::PatternKind::Ident(ident) = param.kind() {
                let name = ident.ident.name.as_str().to_string();
                param_set.insert(name.clone());
                param_names.push(name);
            } else {
                self.add_error(
                    Diagnostic::error(
                        "only simple identifier parameters are supported in closures".to_string(),
                    )
                    .with_source_context(DIAGNOSTIC_CONTEXT)
                    .with_span(param.span()),
                );
                return Ok(None);
            }
        }

        let captures = self.collect_captures(closure.body.as_ref(), &param_set)?;

        let struct_ident = ast::Ident::new(format!("__Closure{}", self.counter));
        let call_ident = ast::Ident::new(format!("__closure{}_call", self.counter));
        self.counter += 1;

        let mut struct_fields: Vec<ast::StructuralField> = captures
            .iter()
            .map(|capture| ast::StructuralField::new(capture.name.clone(), capture.ty.clone()))
            .collect();
        if struct_fields.is_empty() {
            struct_fields.push(ast::StructuralField::new(
                ast::Ident::new(DUMMY_CAPTURE_NAME),
                ast::Ty::Primitive(ast::TypePrimitive::Int(ast::TypeInt::I8)),
            ));
        }
        let struct_decl = ast::TypeStruct {
            name: struct_ident.clone(),
            generics_params: Vec::new(),
            fields: struct_fields,
        };
        let env_struct_ty = ast::Ty::Struct(struct_decl.clone());

        let mut struct_item = ast::Item::new(ast::ItemKind::DefStruct(ast::ItemDefStruct {
            attrs: Vec::new(),
            visibility: ast::Visibility::Private,
            name: struct_ident.clone(),
            value: struct_decl.clone(),
        }));
        struct_item.set_ty(ast::Ty::Struct(struct_decl.clone()));
        let env_param_ident = ast::Ident::new("__env");
        let mut fn_params = Vec::new();
        let mut fn_param_tys = Vec::new();
        fn_params.push(ast::FunctionParam::new(
            env_param_ident.clone(),
            env_struct_ty.clone(),
        ));
        fn_param_tys.push(env_struct_ty.clone());
        for (idx, name) in param_names.iter().enumerate() {
            let ty = fn_ty
                .params
                .get(idx)
                .cloned()
                .unwrap_or_else(|| ast::Ty::Any(ast::TypeAny));
            fn_params.push(ast::FunctionParam::new(
                ast::Ident::new(name.clone()),
                ty.clone(),
            ));
            fn_param_tys.push(ty);
        }

        let mut rewritten_body = (*closure.body).clone();
        let inferred_ret_ty = fn_ty
            .ret_ty
            .as_ref()
            .and_then(|ty| {
                if matches!(ty.as_ref(), ast::Ty::Unknown(_)) {
                    None
                } else {
                    Some(ty.as_ref().clone())
                }
            })
            .or_else(|| {
                closure
                    .body
                    .ty()
                    .cloned()
                    .or_else(|| rewritten_body.ty().cloned())
                    .and_then(|ty| {
                        if matches!(ty, ast::Ty::Unknown(_)) {
                            None
                        } else {
                            Some(ty)
                        }
                    })
            });
        let fallback_ret_ty = fn_ty.ret_ty.as_ref().and_then(|ty| {
            if matches!(ty.as_ref(), ast::Ty::Unknown(_)) {
                None
            } else {
                Some(ty.as_ref().clone())
            }
        });
        let call_ret_ty = inferred_ret_ty
            .clone()
            .or(fallback_ret_ty)
            .unwrap_or_else(|| ast::Ty::Unknown(ast::TypeUnknown));

        self.rewrite_captured_usage(&mut rewritten_body, &captures, &env_param_ident);

        let mut fn_item_ast =
            ast::ItemDefFunction::new_simple(call_ident.clone(), rewritten_body.into());
        fn_item_ast.visibility = ast::Visibility::Private;
        fn_item_ast.sig.params = fn_params;
        fn_item_ast.sig.ret_ty = Some(call_ret_ty.clone());
        fn_item_ast.ty = Some(ast::TypeFunction {
            params: fn_param_tys.clone(),
            generics_params: Vec::new(),
            ret_ty: Some(Box::new(call_ret_ty.clone())),
        });
        fn_item_ast.ty_annotation = fn_item_ast.ty.clone().map(|ty_fn| ast::Ty::Function(ty_fn));

        let fn_item = ast::Item::new(ast::ItemKind::DefFunction(fn_item_ast));

        self.generated_items.push(struct_item);
        self.generated_items.push(fn_item);

        let mut fields = Vec::new();
        for capture in &captures {
            let mut value_expr = ast::Expr::ident(capture.name.clone());
            value_expr.set_ty(capture.ty.clone());
            fields.push(ast::ExprField::new(capture.name.clone(), value_expr));
        }
        if fields.is_empty() {
            let mut value_expr = ast::Expr::value(ast::Value::int(0));
            value_expr.set_ty(ast::Ty::Primitive(ast::TypePrimitive::Int(
                ast::TypeInt::I8,
            )));
            fields.push(ast::ExprField::new(
                ast::Ident::new(DUMMY_CAPTURE_NAME),
                value_expr,
            ));
        }

        let struct_name_expr = ast::Expr::ident(struct_ident.clone());

        let mut struct_expr = ast::Expr::new(ast::ExprKind::Struct(ast::ExprStruct {
            span: fp_core::span::Span::null(),
            name: struct_name_expr.into(),
            fields,
            update: None,
        }));
        struct_expr.set_ty(env_struct_ty.clone());

        *expr = struct_expr;

        let info = ClosureInfo {
            env_struct_ident: struct_ident,
            env_struct_ty,
            call_fn_ident: call_ident,
            call_ret_ty: call_ret_ty.clone(),
        };

        Ok(Some(info))
    }

    fn rewrite_usage(&mut self, items: &mut [ast::Item]) -> Result<()> {
        for item in items {
            match item.kind_mut() {
                ast::ItemKind::Module(module) => self.rewrite_usage(&mut module.items)?,
                ast::ItemKind::DefFunction(func) => {
                    self.rewrite_in_expr(func.body.as_mut())?;
                }
                ast::ItemKind::DefConst(def) => self.rewrite_in_expr(def.value.as_mut())?,
                ast::ItemKind::DefStatic(def) => self.rewrite_in_expr(def.value.as_mut())?,
                ast::ItemKind::Expr(expr) => self.rewrite_in_expr(expr)?,
                _ => {}
            }
        }
        Ok(())
    }

    fn rewrite_in_expr(&mut self, expr: &mut ast::Expr) -> Result<()> {
        if expand_intrinsic_collection(expr) {
            return self.rewrite_in_expr(expr);
        }

        if let Some(info) = self.transform_closure_expr(expr)? {
            self.struct_infos
                .insert(info.env_struct_ident.as_str().to_string(), info);
            return self.rewrite_in_expr(expr);
        }

        match expr.kind_mut() {
            ast::ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    self.rewrite_in_stmt(stmt)?;
                }
                if let Some(last) = block.last_expr_mut() {
                    self.rewrite_in_expr(last)?;
                }
            }
            ast::ExprKind::If(expr_if) => {
                self.rewrite_in_expr(expr_if.cond.as_mut())?;
                self.rewrite_in_expr(expr_if.then.as_mut())?;
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.rewrite_in_expr(elze)?;
                }
            }
            ast::ExprKind::Loop(expr_loop) => self.rewrite_in_expr(expr_loop.body.as_mut())?,
            ast::ExprKind::While(expr_while) => {
                self.rewrite_in_expr(expr_while.cond.as_mut())?;
                self.rewrite_in_expr(expr_while.body.as_mut())?;
            }
            ast::ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_mut() {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_mut() {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::Continue(_) => {}
            ast::ExprKind::ConstBlock(const_block) => {
                self.rewrite_in_expr(const_block.expr.as_mut())?;
            }
            ast::ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.rewrite_in_expr(case.cond.as_mut())?;
                    self.rewrite_in_expr(case.body.as_mut())?;
                }
            }
            ast::ExprKind::For(expr_for) => {
                self.rewrite_in_expr(expr_for.iter.as_mut())?;
                self.rewrite_in_expr(expr_for.body.as_mut())?;
            }
            ast::ExprKind::Let(expr_let) => self.rewrite_in_expr(expr_let.expr.as_mut())?,
            ast::ExprKind::Macro(_) => {}
            ast::ExprKind::Quote(q) => {
                for stmt in &mut q.block.stmts {
                    self.rewrite_in_stmt(stmt)?;
                }
                if let Some(last) = q.block.clone().last_expr_mut() {
                    let mut last_clone = last.clone();
                    self.rewrite_in_expr(&mut last_clone)?;
                }
            }
            ast::ExprKind::Splice(s) => {
                self.rewrite_in_expr(s.token.as_mut())?;
            }
            ast::ExprKind::Invoke(invoke) => {
                for arg in &mut invoke.args {
                    self.rewrite_in_expr(arg)?;
                }
                match &mut invoke.target {
                    ast::ExprInvokeTarget::Expr(target) => {
                        self.rewrite_in_expr(target.as_mut())?;
                        if let Some(info) = self.closure_info_from_expr(target.as_ref()) {
                            let call_locator = ast::Locator::ident(info.call_fn_ident.clone());
                            let mut new_args = Vec::with_capacity(invoke.args.len() + 1);
                            new_args.push(*target.clone());
                            new_args.extend(invoke.args.iter().cloned());
                            invoke.target = ast::ExprInvokeTarget::Function(call_locator);
                            invoke.args = new_args;
                            expr.set_ty(info.call_ret_ty.clone());
                        }
                    }
                    ast::ExprInvokeTarget::Function(locator) => {
                        if let Some(ident) = locator.as_ident() {
                            let info = self
                                .variable_infos
                                .get(ident.as_str())
                                .cloned()
                                .or_else(|| self.struct_infos.get(ident.as_str()).cloned());
                            if let Some(info) = info {
                                let mut env_expr =
                                    ast::Expr::new(ast::ExprKind::Locator(locator.clone()));
                                env_expr.set_ty(info.env_struct_ty.clone());
                                let call_locator = ast::Locator::ident(info.call_fn_ident.clone());
                                let mut new_args = Vec::with_capacity(invoke.args.len() + 1);
                                new_args.push(env_expr);
                                new_args.extend(invoke.args.iter().cloned());
                                invoke.target = ast::ExprInvokeTarget::Function(call_locator);
                                invoke.args = new_args;
                                expr.set_ty(info.call_ret_ty.clone());
                            }
                        }
                    }
                    _ => {}
                }
            }
            ast::ExprKind::Await(await_expr) => {
                self.rewrite_in_expr(await_expr.base.as_mut())?;
            }
            ast::ExprKind::Async(async_expr) => {
                self.rewrite_in_expr(async_expr.expr.as_mut())?;
            }
            ast::ExprKind::Assign(assign) => {
                self.rewrite_in_expr(assign.target.as_mut())?;
                self.rewrite_in_expr(assign.value.as_mut())?;
            }
            ast::ExprKind::Select(select) => self.rewrite_in_expr(select.obj.as_mut())?,
            ast::ExprKind::Struct(struct_expr) => {
                self.rewrite_in_expr(struct_expr.name.as_mut())?;
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_in_expr(value)?;
                    }
                }
            }
            ast::ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.rewrite_in_expr(value)?;
                    }
                }
            }
            ast::ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::ArrayRepeat(array_repeat) => {
                self.rewrite_in_expr(array_repeat.elem.as_mut())?;
                self.rewrite_in_expr(array_repeat.len.as_mut())?;
            }
            ast::ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.rewrite_in_expr(value)?;
                }
            }
            ast::ExprKind::Reference(reference) => {
                self.rewrite_in_expr(reference.referee.as_mut())?;
            }
            ast::ExprKind::Dereference(deref) => {
                self.rewrite_in_expr(deref.referee.as_mut())?;
            }
            ast::ExprKind::Cast(cast) => self.rewrite_in_expr(cast.expr.as_mut())?,
            ast::ExprKind::Index(index) => {
                self.rewrite_in_expr(index.obj.as_mut())?;
                self.rewrite_in_expr(index.index.as_mut())?;
            }
            ast::ExprKind::BinOp(binop) => {
                self.rewrite_in_expr(binop.lhs.as_mut())?;
                self.rewrite_in_expr(binop.rhs.as_mut())?;
            }
            ast::ExprKind::UnOp(unop) => self.rewrite_in_expr(unop.val.as_mut())?,
            ast::ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    self.rewrite_in_expr(start.as_mut())?;
                }
                if let Some(end) = range.end.as_mut() {
                    self.rewrite_in_expr(end.as_mut())?;
                }
                if let Some(step) = range.step.as_mut() {
                    self.rewrite_in_expr(step.as_mut())?;
                }
            }
            ast::ExprKind::FormatString(format) => {
                let _ = format;
            }
            ast::ExprKind::Try(expr_try) => self.rewrite_in_expr(expr_try.expr.as_mut())?,
            ast::ExprKind::Value(value) => match value.as_mut() {
                ast::Value::Expr(expr) => self.rewrite_in_expr(expr.as_mut())?,
                ast::Value::Function(func) => self.rewrite_in_expr(func.body.as_mut())?,
                _ => {}
            },
            ast::ExprKind::Splat(splat) => self.rewrite_in_expr(splat.iter.as_mut())?,
            ast::ExprKind::SplatDict(dict) => self.rewrite_in_expr(dict.dict.as_mut())?,
            ast::ExprKind::Item(item) => self.rewrite_in_item(item.as_mut())?,
            ast::ExprKind::IntrinsicCall(call) => {
                for arg in &mut call.args {
                    self.rewrite_in_expr(arg)?;
                }
                for kwarg in &mut call.kwargs {
                    self.rewrite_in_expr(&mut kwarg.value)?;
                }
            }
            ast::ExprKind::Paren(paren) => self.rewrite_in_expr(paren.expr.as_mut())?,
            ast::ExprKind::IntrinsicContainer(_) => {
                unreachable!("intrinsic collections should have been expanded")
            }
            ast::ExprKind::Locator(_) | ast::ExprKind::Closured(_) => {}
            ast::ExprKind::Closure(_) | ast::ExprKind::Any(_) | ast::ExprKind::Id(_) => {}
        }
        Ok(())
    }

    fn rewrite_in_stmt(&mut self, stmt: &mut ast::BlockStmt) -> Result<()> {
        match stmt {
            ast::BlockStmt::Expr(expr_stmt) => self.rewrite_in_expr(expr_stmt.expr.as_mut())?,
            ast::BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    self.rewrite_in_expr(init)?;
                    if let Some(info) = self.closure_info_from_expr(init) {
                        let mut names = Vec::new();
                        collect_pattern_idents(&stmt_let.pat, &mut names);
                        for name in names {
                            self.variable_infos.insert(name, info.clone());
                        }
                        stmt_let.pat.set_ty(info.env_struct_ty.clone());
                        init.set_ty(info.env_struct_ty.clone());
                    }
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    self.rewrite_in_expr(diverge)?;
                }
            }
            ast::BlockStmt::Item(item) => self.rewrite_in_item(item.as_mut())?,
            ast::BlockStmt::Noop | ast::BlockStmt::Any(_) => {}
        }
        Ok(())
    }

    fn rewrite_in_item(&mut self, item: &mut ast::Item) -> Result<()> {
        match item.kind_mut() {
            ast::ItemKind::Expr(expr) => self.rewrite_in_expr(expr)?,
            ast::ItemKind::DefConst(def) => {
                self.rewrite_in_expr(def.value.as_mut())?;
                if let Some(info) = self.closure_info_from_expr(def.value.as_ref()) {
                    self.variable_infos
                        .insert(def.name.as_str().to_string(), info.clone());
                    def.ty = Some(info.env_struct_ty.clone());
                    def.ty_annotation = Some(info.env_struct_ty.clone());
                    def.value.set_ty(info.env_struct_ty.clone());
                }
            }
            ast::ItemKind::DefStatic(def) => {
                self.rewrite_in_expr(def.value.as_mut())?;
                if let Some(info) = self.closure_info_from_expr(def.value.as_ref()) {
                    self.variable_infos
                        .insert(def.name.as_str().to_string(), info.clone());
                    def.ty = info.env_struct_ty.clone();
                    def.ty_annotation = Some(info.env_struct_ty.clone());
                    def.value.set_ty(info.env_struct_ty.clone());
                }
            }
            ast::ItemKind::DefFunction(func) => self.rewrite_in_expr(func.body.as_mut())?,
            ast::ItemKind::Module(module) => self.rewrite_usage(&mut module.items)?,
            _ => {}
        }
        Ok(())
    }

    fn closure_info_from_expr(&self, expr: &ast::Expr) -> Option<ClosureInfo> {
        match expr.kind() {
            ast::ExprKind::Struct(struct_expr) => extract_ident(struct_expr.name.as_ref())
                .and_then(|ident| self.struct_infos.get(ident.as_str()).cloned()),
            ast::ExprKind::Invoke(invoke) => {
                if let ast::ExprInvokeTarget::Function(locator) = &invoke.target {
                    locator
                        .as_ident()
                        .and_then(|ident| self.function_infos.get(ident.as_str()).cloned())
                } else {
                    None
                }
            }
            ast::ExprKind::Locator(locator) => locator
                .as_ident()
                .and_then(|ident| self.variable_infos.get(ident.as_str()).cloned()),
            ast::ExprKind::Paren(paren) => self.closure_info_from_expr(paren.expr.as_ref()),
            _ => None,
        }
    }

    fn collect_captures(&self, expr: &ast::Expr, params: &HashSet<String>) -> Result<Vec<Capture>> {
        let mut collector = CaptureCollector::new(params.clone());
        collector.visit(expr);
        Ok(collector.into_captures())
    }

    fn rewrite_captured_usage(
        &self,
        expr: &mut ast::Expr,
        captures: &[Capture],
        env_ident: &ast::Ident,
    ) {
        let mut replacer = CaptureReplacer::new(captures, env_ident.clone());
        replacer.visit(expr);
    }
}

struct CaptureCollector {
    scope: Vec<HashSet<String>>,
    captures: Vec<(String, ast::Ty)>,
    seen: HashSet<String>,
}

impl CaptureCollector {
    fn new(params: HashSet<String>) -> Self {
        Self {
            scope: vec![params],
            captures: Vec::new(),
            seen: HashSet::new(),
        }
    }

    fn visit(&mut self, expr: &ast::Expr) {
        match expr.kind() {
            ast::ExprKind::Quote(q) => {
                self.scope.push(HashSet::new());
                for stmt in &q.block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = q.block.last_expr() {
                    self.visit(last);
                }
                self.scope.pop();
            }
            ast::ExprKind::Splice(s) => {
                self.visit(s.token.as_ref());
            }
            ast::ExprKind::Closure(_) | ast::ExprKind::Closured(_) => {}
            ast::ExprKind::IntrinsicContainer(collection) => {
                let expanded = collection.clone().into_const_expr();
                self.visit(&expanded);
            }
            ast::ExprKind::Block(block) => {
                self.scope.push(HashSet::new());
                for stmt in &block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = block.last_expr() {
                    self.visit(last);
                }
                self.scope.pop();
            }
            ast::ExprKind::Let(expr_let) => {
                self.visit(expr_let.expr.as_ref());
                let mut names = Vec::new();
                collect_pattern_idents(&expr_let.pat, &mut names);
                if let Some(scope) = self.scope.last_mut() {
                    for name in names {
                        scope.insert(name);
                    }
                }
            }
            ast::ExprKind::Macro(_) => {}
            ast::ExprKind::Invoke(invoke) => {
                match &invoke.target {
                    ast::ExprInvokeTarget::Expr(target) => self.visit(target.as_ref()),
                    ast::ExprInvokeTarget::Method(select) => self.visit(select.obj.as_ref()),
                    _ => {}
                }
                for arg in &invoke.args {
                    self.visit(arg);
                }
            }
            ast::ExprKind::Assign(assign) => {
                self.visit(assign.target.as_ref());
                self.visit(assign.value.as_ref());
            }
            ast::ExprKind::Await(await_expr) => {
                self.visit(await_expr.base.as_ref());
            }
            ast::ExprKind::Async(async_expr) => {
                self.visit(async_expr.expr.as_ref());
            }
            ast::ExprKind::BinOp(binop) => {
                self.visit(binop.lhs.as_ref());
                self.visit(binop.rhs.as_ref());
            }
            ast::ExprKind::UnOp(unop) => self.visit(unop.val.as_ref()),
            ast::ExprKind::Select(select) => self.visit(select.obj.as_ref()),
            ast::ExprKind::Struct(struct_expr) => {
                self.visit(struct_expr.name.as_ref());
                for field in &struct_expr.fields {
                    if let Some(value) = field.value.as_ref() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Structural(struct_expr) => {
                for field in &struct_expr.fields {
                    if let Some(value) = field.value.as_ref() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Array(array) => {
                for value in &array.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::ArrayRepeat(array_repeat) => {
                self.visit(array_repeat.elem.as_ref());
                self.visit(array_repeat.len.as_ref());
            }
            ast::ExprKind::Tuple(tuple) => {
                for value in &tuple.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::Reference(reference) => self.visit(reference.referee.as_ref()),
            ast::ExprKind::Dereference(deref) => self.visit(deref.referee.as_ref()),
            ast::ExprKind::Cast(cast) => self.visit(cast.expr.as_ref()),
            ast::ExprKind::Index(index) => {
                self.visit(index.obj.as_ref());
                self.visit(index.index.as_ref());
            }
            ast::ExprKind::If(expr_if) => {
                self.visit(expr_if.cond.as_ref());
                self.visit(expr_if.then.as_ref());
                if let Some(elze) = expr_if.elze.as_ref() {
                    self.visit(elze);
                }
            }
            ast::ExprKind::Loop(expr_loop) => self.visit(expr_loop.body.as_ref()),
            ast::ExprKind::While(expr_while) => {
                self.visit(expr_while.cond.as_ref());
                self.visit(expr_while.body.as_ref());
            }
            ast::ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_ref() {
                    self.visit(value.as_ref());
                }
            }
            ast::ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_ref() {
                    self.visit(value.as_ref());
                }
            }
            ast::ExprKind::Continue(_) => {}
            ast::ExprKind::ConstBlock(const_block) => {
                self.visit(const_block.expr.as_ref());
            }
            ast::ExprKind::For(expr_for) => {
                self.visit(expr_for.iter.as_ref());
                self.visit(expr_for.body.as_ref());
            }
            ast::ExprKind::Match(expr_match) => {
                for case in &expr_match.cases {
                    self.visit(case.cond.as_ref());
                    self.visit(case.body.as_ref());
                }
            }
            ast::ExprKind::FormatString(format) => {
                let _ = format;
            }
            ast::ExprKind::Range(range) => {
                if let Some(start) = range.start.as_ref() {
                    self.visit(start.as_ref());
                }
                if let Some(end) = range.end.as_ref() {
                    self.visit(end.as_ref());
                }
                if let Some(step) = range.step.as_ref() {
                    self.visit(step.as_ref());
                }
            }
            ast::ExprKind::Try(expr_try) => self.visit(expr_try.expr.as_ref()),
            ast::ExprKind::Value(value) => match value.as_ref() {
                ast::Value::Expr(expr) => self.visit(expr.as_ref()),
                ast::Value::Function(func) => self.visit(func.body.as_ref()),
                _ => {}
            },
            ast::ExprKind::Paren(paren) => self.visit(paren.expr.as_ref()),
            ast::ExprKind::Locator(locator) => {
                if let Some(ident) = locator.as_ident() {
                    let name = ident.as_str();
                    if !self.is_in_scope(name) && !self.seen.contains(name) {
                        let ty = expr
                            .ty()
                            .cloned()
                            .unwrap_or_else(|| ast::Ty::Any(ast::TypeAny));
                        self.seen.insert(name.to_string());
                        self.captures.push((name.to_string(), ty));
                    }
                }
            }
            ast::ExprKind::Splat(splat) => self.visit(splat.iter.as_ref()),
            ast::ExprKind::SplatDict(dict) => self.visit(dict.dict.as_ref()),
            ast::ExprKind::Item(item) => self.visit_item(item.as_ref()),
            ast::ExprKind::IntrinsicCall(call) => {
                for arg in &call.args {
                    self.visit(arg);
                }
                for kwarg in &call.kwargs {
                    self.visit(&kwarg.value);
                }
            }
            ast::ExprKind::Any(_) | ast::ExprKind::Id(_) => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &ast::BlockStmt) {
        match stmt {
            ast::BlockStmt::Expr(expr_stmt) => self.visit(expr_stmt.expr.as_ref()),
            ast::BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_ref() {
                    self.visit(init);
                }
                if let Some(diverge) = stmt_let.diverge.as_ref() {
                    self.visit(diverge);
                }
                let mut names = Vec::new();
                collect_pattern_idents(&stmt_let.pat, &mut names);
                if let Some(scope) = self.scope.last_mut() {
                    for name in names {
                        scope.insert(name);
                    }
                }
            }
            ast::BlockStmt::Item(item) => self.visit_item(item.as_ref()),
            ast::BlockStmt::Noop | ast::BlockStmt::Any(_) => {}
        }
    }

    fn visit_item(&mut self, item: &ast::Item) {
        match item.kind() {
            ast::ItemKind::Expr(expr) => self.visit(expr),
            ast::ItemKind::DefConst(def) => self.visit(def.value.as_ref()),
            ast::ItemKind::DefStatic(def) => self.visit(def.value.as_ref()),
            ast::ItemKind::DefFunction(func) => self.visit(func.body.as_ref()),
            ast::ItemKind::Module(module) => {
                for item in &module.items {
                    self.visit_item(item);
                }
            }
            _ => {}
        }
    }

    fn is_in_scope(&self, name: &str) -> bool {
        self.scope.iter().rev().any(|scope| scope.contains(name))
    }

    fn into_captures(self) -> Vec<Capture> {
        self.captures
            .into_iter()
            .map(|(name, ty)| Capture {
                name: ast::Ident::new(name),
                ty,
            })
            .collect()
    }
}

fn collect_pattern_idents(pat: &ast::Pattern, out: &mut Vec<String>) {
    match pat.kind() {
        ast::PatternKind::Ident(ident) => out.push(ident.ident.name.as_str().to_string()),
        ast::PatternKind::Bind(bind) => {
            out.push(bind.ident.ident.name.as_str().to_string());
            collect_pattern_idents(&bind.pattern, out);
        }
        ast::PatternKind::Tuple(pat_tuple) => {
            for pat in &pat_tuple.patterns {
                collect_pattern_idents(pat, out);
            }
        }
        ast::PatternKind::Struct(pat_struct) => {
            for field in &pat_struct.fields {
                if let Some(rename) = field.rename.as_ref() {
                    collect_pattern_idents(rename.as_ref(), out);
                } else {
                    out.push(field.name.as_str().to_string());
                }
            }
        }
        ast::PatternKind::TupleStruct(pat_tuple) => {
            for pat in &pat_tuple.patterns {
                collect_pattern_idents(pat, out);
            }
        }
        _ => {}
    }
}

struct CaptureReplacer {
    captures: HashMap<String, ast::Ty>,
    env_ident: ast::Ident,
}

impl CaptureReplacer {
    fn new(captures: &[Capture], env_ident: ast::Ident) -> Self {
        let mut capture_map = HashMap::new();
        for capture in captures {
            capture_map.insert(capture.name.as_str().to_string(), capture.ty.clone());
        }
        Self {
            captures: capture_map,
            env_ident,
        }
    }

    fn visit(&mut self, expr: &mut ast::Expr) {
        match expr.kind_mut() {
            ast::ExprKind::Locator(locator) => {
                if let Some(ident) = locator.as_ident() {
                    if let Some(capture_ty) = self.captures.get(ident.as_str()) {
                        let mut expr_struct =
                            ast::Expr::new(ast::ExprKind::Select(ast::ExprSelect {
                                span: fp_core::span::Span::null(),
                                obj: ast::Expr::ident(self.env_ident.clone()).into(),
                                field: ident.clone(),
                                select: ast::ExprSelectType::Field,
                            }));
                        expr_struct.set_ty(capture_ty.clone());
                        *expr = expr_struct;
                    }
                }
            }
            ast::ExprKind::Block(block) => {
                for stmt in &mut block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = block.last_expr_mut() {
                    self.visit(last);
                }
            }
            ast::ExprKind::If(expr_if) => {
                self.visit(expr_if.cond.as_mut());
                self.visit(expr_if.then.as_mut());
                if let Some(elze) = expr_if.elze.as_mut() {
                    self.visit(elze);
                }
            }
            ast::ExprKind::Loop(expr_loop) => self.visit(expr_loop.body.as_mut()),
            ast::ExprKind::While(expr_while) => {
                self.visit(expr_while.cond.as_mut());
                self.visit(expr_while.body.as_mut());
            }
            ast::ExprKind::Return(expr_return) => {
                if let Some(value) = expr_return.value.as_mut() {
                    self.visit(value.as_mut());
                }
            }
            ast::ExprKind::Break(expr_break) => {
                if let Some(value) = expr_break.value.as_mut() {
                    self.visit(value.as_mut());
                }
            }
            ast::ExprKind::Continue(_) => {}
            ast::ExprKind::ConstBlock(const_block) => {
                self.visit(const_block.expr.as_mut());
            }
            ast::ExprKind::Match(expr_match) => {
                for case in &mut expr_match.cases {
                    self.visit(case.cond.as_mut());
                    self.visit(case.body.as_mut());
                }
            }
            ast::ExprKind::For(expr_for) => {
                self.visit(expr_for.iter.as_mut());
                self.visit(expr_for.body.as_mut());
            }
            ast::ExprKind::Let(expr_let) => self.visit(expr_let.expr.as_mut()),
            ast::ExprKind::Macro(_) => {}
            ast::ExprKind::Invoke(invoke) => {
                for arg in &mut invoke.args {
                    self.visit(arg);
                }
                match &mut invoke.target {
                    ast::ExprInvokeTarget::Expr(target) => {
                        self.visit(target.as_mut());
                    }
                    ast::ExprInvokeTarget::Function(locator) => {
                        if let Some(ident) = locator.as_ident() {
                            if let Some(capture_ty) = self.captures.get(ident.as_str()) {
                                let mut expr_struct =
                                    ast::Expr::new(ast::ExprKind::Select(ast::ExprSelect {
                                        span: fp_core::span::Span::null(),
                                        obj: ast::Expr::ident(self.env_ident.clone()).into(),
                                        field: ident.clone(),
                                        select: ast::ExprSelectType::Field,
                                    }));
                                expr_struct.set_ty(capture_ty.clone());
                                invoke.target = ast::ExprInvokeTarget::Expr(expr_struct.into());
                            }
                        }
                    }
                    ast::ExprInvokeTarget::Method(select) => {
                        self.visit(select.obj.as_mut());
                    }
                    _ => {}
                }
            }
            ast::ExprKind::Await(await_expr) => {
                self.visit(await_expr.base.as_mut());
            }
            ast::ExprKind::Async(async_expr) => {
                self.visit(async_expr.expr.as_mut());
            }
            ast::ExprKind::Assign(assign) => {
                self.visit(assign.target.as_mut());
                self.visit(assign.value.as_mut());
            }
            ast::ExprKind::Select(select) => self.visit(select.obj.as_mut()),
            ast::ExprKind::Struct(struct_expr) => {
                self.visit(struct_expr.name.as_mut());
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Structural(struct_expr) => {
                for field in &mut struct_expr.fields {
                    if let Some(value) = field.value.as_mut() {
                        self.visit(value);
                    }
                }
            }
            ast::ExprKind::Array(array) => {
                for value in &mut array.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::ArrayRepeat(array_repeat) => {
                self.visit(array_repeat.elem.as_mut());
                self.visit(array_repeat.len.as_mut());
            }
            ast::ExprKind::Tuple(tuple) => {
                for value in &mut tuple.values {
                    self.visit(value);
                }
            }
            ast::ExprKind::Reference(reference) => self.visit(reference.referee.as_mut()),
            ast::ExprKind::Dereference(deref) => self.visit(deref.referee.as_mut()),
            ast::ExprKind::Cast(cast) => self.visit(cast.expr.as_mut()),
            ast::ExprKind::Index(index) => {
                self.visit(index.obj.as_mut());
                self.visit(index.index.as_mut());
            }
            ast::ExprKind::BinOp(binop) => {
                self.visit(binop.lhs.as_mut());
                self.visit(binop.rhs.as_mut());
            }
            ast::ExprKind::UnOp(unop) => self.visit(unop.val.as_mut()),
            ast::ExprKind::Range(range) => {
                if let Some(start) = range.start.as_mut() {
                    self.visit(start.as_mut());
                }
                if let Some(end) = range.end.as_mut() {
                    self.visit(end.as_mut());
                }
                if let Some(step) = range.step.as_mut() {
                    self.visit(step.as_mut());
                }
            }
            ast::ExprKind::FormatString(format) => {
                let _ = format;
            }
            ast::ExprKind::Try(expr_try) => self.visit(expr_try.expr.as_mut()),
            ast::ExprKind::Value(value) => match value.as_mut() {
                ast::Value::Expr(expr) => self.visit(expr.as_mut()),
                ast::Value::Function(func) => self.visit(func.body.as_mut()),
                _ => {}
            },
            ast::ExprKind::Paren(paren) => self.visit(paren.expr.as_mut()),
            ast::ExprKind::Splat(splat) => self.visit(splat.iter.as_mut()),
            ast::ExprKind::SplatDict(dict) => self.visit(dict.dict.as_mut()),
            ast::ExprKind::Item(item) => self.visit_item(item.as_mut()),
            ast::ExprKind::IntrinsicCall(call) => {
                for arg in &mut call.args {
                    self.visit(arg);
                }
                for kwarg in &mut call.kwargs {
                    self.visit(&mut kwarg.value);
                }
            }
            ast::ExprKind::Quote(q) => {
                for stmt in &mut q.block.stmts {
                    self.visit_stmt(stmt);
                }
                if let Some(last) = q.block.last_expr_mut() {
                    self.visit(last);
                }
            }
            ast::ExprKind::Splice(s) => {
                self.visit(s.token.as_mut());
            }
            ast::ExprKind::IntrinsicContainer(container) => {
                let mut new_expr = container.clone().into_const_expr();
                self.visit(&mut new_expr);
                *expr = new_expr;
            }
            ast::ExprKind::Any(_)
            | ast::ExprKind::Id(_)
            | ast::ExprKind::Closure(_)
            | ast::ExprKind::Closured(_) => {}
        }
    }

    fn visit_stmt(&mut self, stmt: &mut ast::BlockStmt) {
        match stmt {
            ast::BlockStmt::Expr(expr_stmt) => self.visit(expr_stmt.expr.as_mut()),
            ast::BlockStmt::Let(stmt_let) => {
                if let Some(init) = stmt_let.init.as_mut() {
                    self.visit(init);
                }
                if let Some(diverge) = stmt_let.diverge.as_mut() {
                    self.visit(diverge);
                }
            }
            ast::BlockStmt::Item(item) => self.visit_item(item.as_mut()),
            ast::BlockStmt::Noop | ast::BlockStmt::Any(_) => {}
        }
    }

    fn visit_item(&mut self, item: &mut ast::Item) {
        match item.kind_mut() {
            ast::ItemKind::Expr(expr) => self.visit(expr),
            ast::ItemKind::DefConst(def) => self.visit(def.value.as_mut()),
            ast::ItemKind::DefStatic(def) => self.visit(def.value.as_mut()),
            ast::ItemKind::DefFunction(func) => self.visit(func.body.as_mut()),
            ast::ItemKind::Module(module) => {
                for item in &mut module.items {
                    self.visit_item(item);
                }
            }
            _ => {}
        }
    }
}

fn extract_ident(expr: &ast::Expr) -> Option<&ast::Ident> {
    if let ast::ExprKind::Locator(locator) = expr.kind() {
        locator.as_ident()
    } else {
        None
    }
}
