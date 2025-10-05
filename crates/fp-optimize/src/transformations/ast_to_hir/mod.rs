use fp_core::error::Result;
use fp_core::id::Locator;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::pat::Pattern;
use fp_core::span::{FileId, Span};
use fp_core::{ast, ast::ItemKind, hir};
use std::collections::HashMap;
use std::path::Path;

use super::IrTransform;

mod expressions;

#[cfg(test)]
mod tests;

use fp_core::diagnostics::Diagnostic;

const DIAGNOSTIC_CONTEXT: &str = "ast_to_hir";

/// Generator for transforming AST to HIR (High-level IR)
///
/// NOTE: This is transitioning from stateful to share-nothing architecture.
/// The generator now supports error tolerance and will gradually become more pure.
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

    // NEW: Error tolerance support
    /// Collected errors during transformation (non-fatal)
    pub errors: Vec<Diagnostic>,
    /// Collected warnings during transformation
    pub warnings: Vec<Diagnostic>,
    /// Whether error recovery should be attempted
    pub error_tolerance: bool,
    /// Maximum number of errors to collect before giving up
    pub max_errors: usize,
}

#[derive(Debug, Clone)]
struct SymbolEntry {
    res: hir::Res,
    export: SymbolExport,
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

impl HirGenerator {
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

            // Initialize error tolerance support
            errors: Vec::new(),
            warnings: Vec::new(),
            error_tolerance: false, // Disabled by default for backward compatibility
            max_errors: 10,
        }
    }

    /// Create a new HIR generator with error tolerance enabled
    pub fn with_error_tolerance(max_errors: usize) -> Self {
        let mut generator = Self::new();
        generator.error_tolerance = true;
        generator.max_errors = max_errors;
        generator
    }

    /// Enable error tolerance on an existing generator
    pub fn enable_error_tolerance(&mut self, max_errors: usize) -> &mut Self {
        self.error_tolerance = true;
        self.max_errors = max_errors;
        self
    }

    /// Add an error to the collection (for error tolerance mode)
    fn add_error(&mut self, error: Diagnostic) -> bool {
        self.errors.push(error);
        // Return false if we've hit the error limit (should stop transformation)
        self.errors.len() < self.max_errors
    }

    fn build_error(message: impl Into<String>) -> Diagnostic {
        Diagnostic::error(message.into()).with_source_context(DIAGNOSTIC_CONTEXT)
    }

    /// Add a warning to the collection
    #[allow(dead_code)]
    fn add_warning(&mut self, warning: Diagnostic) {
        self.warnings.push(warning);
    }

    /// Check if we should continue after an error (in tolerance mode)
    #[allow(dead_code)]
    fn should_continue_after_error(&self) -> bool {
        self.error_tolerance && self.errors.len() < self.max_errors
    }

    /// Create an error HIR expression for recovery
    #[allow(dead_code)]
    fn create_error_expr(&mut self) -> hir::Expr {
        // Create a literal placeholder expression for error recovery
        hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Literal(hir::Lit::Bool(false)), // Placeholder expression for recovery
            span: Span::new(
                self.current_file,
                self.current_position,
                self.current_position,
            ),
        }
    }

    /// Create an error HIR item for recovery  
    #[allow(dead_code)]
    fn create_error_item(&mut self) -> hir::Item {
        // For now, skip creating error recovery items since HIR structure is complex
        // In practice, error recovery at item level would handle this differently
        panic!("Item-level error recovery not implemented yet")
    }

    /// Get all collected errors and warnings
    pub fn take_diagnostics(&mut self) -> (Vec<Diagnostic>, Vec<Diagnostic>) {
        (
            std::mem::take(&mut self.errors),
            std::mem::take(&mut self.warnings),
        )
    }

    fn handle_import(&mut self, import: &ast::ItemImport) -> Result<()> {
        let entries = match self.expand_import_tree(&import.tree, Vec::new()) {
            Ok(entries) => entries,
            Err(e) if self.error_tolerance => {
                // In error tolerance mode, collect the error and continue
                self.add_error(
                    Self::build_error(format!("Failed to expand import tree: {}", e))
                        .with_suggestion("Check import syntax and module availability".to_string()),
                );
                return Ok(()); // Continue with empty imports
            }
            Err(e) => return Err(e), // Legacy behavior
        };

        for (path_segments, alias) in entries {
            let value_res = self.lookup_global_res(&path_segments, PathResolutionScope::Value);
            let type_res = self.lookup_global_res(&path_segments, PathResolutionScope::Type);

            if value_res.is_none() && type_res.is_none() {
                if let Some(first) = path_segments.first() {
                    if first == "std" || first == "core" || first == "alloc" {
                        // Standard library imports are not yet modeled; ignore them so
                        // user code can continue through the pipeline.
                        continue;
                    }
                }
                if self.error_tolerance {
                    // Collect error and continue instead of early return
                    let continue_processing = self.add_error(
                        Self::build_error(format!(
                            "Unresolved import: {}",
                            path_segments.join("::")
                        ))
                        .with_suggestions(vec![
                            "Check if the module exists".to_string(),
                            "Verify the import path".to_string(),
                            "Make sure the symbol is exported".to_string(),
                        ]),
                    );

                    if !continue_processing {
                        return Err(crate::error::optimization_error("Too many import errors"));
                    }
                    continue; // Skip this import but continue with others
                } else {
                    // Legacy behavior: early return
                    return Err(crate::error::optimization_error(format!(
                        "Unresolved import: {}",
                        path_segments.join("::")
                    )));
                }
            }

            if let Some(res) = value_res {
                self.current_value_scope()
                    .insert(alias.clone(), res.clone());
                self.record_value_symbol(&alias, res, &import.visibility);
            }

            if let Some(res) = type_res {
                self.current_type_scope().insert(alias.clone(), res.clone());
                self.record_type_symbol(&alias, res, &import.visibility);
            }
        }

        Ok(())
    }

    fn expand_import_tree(
        &self,
        tree: &ast::ItemImportTree,
        base: Vec<String>,
    ) -> Result<Vec<(Vec<String>, String)>> {
        match tree {
            ast::ItemImportTree::Path(path) => self.expand_import_segments(&path.segments, base),
            ast::ItemImportTree::Group(group) => {
                let mut results = Vec::new();
                for item in &group.items {
                    results.extend(self.expand_import_tree(item, base.clone())?);
                }
                Ok(results)
            }
            ast::ItemImportTree::Root => self.expand_import_segments(&[], Vec::new()),
            ast::ItemImportTree::SelfMod => {
                self.expand_import_segments(&[], self.module_path.clone())
            }
            ast::ItemImportTree::SuperMod => {
                self.expand_import_segments(&[], self.parent_module_path())
            }
            ast::ItemImportTree::Crate => self.expand_import_segments(&[], Vec::new()),
            ast::ItemImportTree::Glob => Err(crate::error::optimization_error(
                "Glob imports are not yet supported".to_string(),
            )),
            _ => self.expand_import_segments(std::slice::from_ref(tree), base),
        }
    }

    fn expand_import_segments(
        &self,
        segments: &[ast::ItemImportTree],
        base: Vec<String>,
    ) -> Result<Vec<(Vec<String>, String)>> {
        if segments.is_empty() {
            return Ok(Vec::new());
        }

        let first = &segments[0];
        let rest = &segments[1..];
        match first {
            ast::ItemImportTree::Ident(ident) => {
                let name = ident.name.as_str();
                let mut new_base = base;
                match name {
                    "self" => new_base = self.module_path.clone(),
                    "super" => new_base = self.parent_module_path(),
                    "crate" => new_base = Vec::new(),
                    _ => new_base.push(ident.name.clone()),
                }

                if rest.is_empty() && !matches!(name, "self" | "super" | "crate") {
                    Ok(vec![(new_base.clone(), ident.name.clone())])
                } else if rest.is_empty() {
                    Ok(Vec::new())
                } else {
                    self.expand_import_segments(rest, new_base)
                }
            }
            ast::ItemImportTree::Rename(rename) => {
                if !rest.is_empty() {
                    return Err(crate::error::optimization_error(
                        "Rename segments must be terminal".to_string(),
                    ));
                }
                let mut new_base = base;
                new_base.push(rename.from.name.clone());
                Ok(vec![(new_base, rename.to.name.clone())])
            }
            ast::ItemImportTree::Group(group) => {
                let mut results = Vec::new();
                for item in &group.items {
                    results.extend(self.expand_import_tree(item, base.clone())?);
                }
                if rest.is_empty() {
                    Ok(results)
                } else {
                    let mut final_results = Vec::new();
                    for (path_segments, alias) in results {
                        let mut more = self.expand_import_segments(rest, path_segments.clone())?;
                        if more.is_empty() {
                            final_results.push((path_segments, alias));
                        } else {
                            final_results.append(&mut more);
                        }
                    }
                    Ok(final_results)
                }
            }
            ast::ItemImportTree::Path(path) => {
                let nested = self.expand_import_segments(&path.segments, base.clone())?;
                if rest.is_empty() {
                    Ok(nested)
                } else {
                    let mut results = Vec::new();
                    for (segments_acc, alias) in nested {
                        let mut more = self.expand_import_segments(rest, segments_acc.clone())?;
                        if more.is_empty() {
                            results.push((segments_acc, alias));
                        } else {
                            results.append(&mut more);
                        }
                    }
                    Ok(results)
                }
            }
            ast::ItemImportTree::Root => self.expand_import_segments(rest, Vec::new()),
            ast::ItemImportTree::SelfMod => {
                self.expand_import_segments(rest, self.module_path.clone())
            }
            ast::ItemImportTree::SuperMod => {
                self.expand_import_segments(rest, self.parent_module_path())
            }
            ast::ItemImportTree::Crate => self.expand_import_segments(rest, Vec::new()),
            ast::ItemImportTree::Glob => Err(crate::error::optimization_error(
                "Glob imports are not yet supported".to_string(),
            )),
        }
    }

    /// Create a new HIR generator with file context
    pub fn with_file<P: AsRef<Path>>(file_path: P) -> Self {
        // Generate a simple hash-based file ID from the path
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        file_path.as_ref().hash(&mut hasher);
        let file_id = hasher.finish();

        Self {
            next_hir_id: 0,
            next_def_id: 0,
            current_file: file_id,
            current_position: 0,
            type_scopes: vec![HashMap::new()],
            value_scopes: vec![HashMap::new()],
            module_path: Vec::new(),
            module_visibility: vec![true],
            global_value_defs: HashMap::new(),
            global_type_defs: HashMap::new(),
            preassigned_def_ids: HashMap::new(),
            enum_variant_def_ids: HashMap::new(),

            // Initialize error tolerance support
            errors: Vec::new(),
            warnings: Vec::new(),
            error_tolerance: false,
            max_errors: 10,
        }
    }

    fn reset_file_context<P: AsRef<Path>>(&mut self, file_path: P) {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        file_path.as_ref().hash(&mut hasher);
        self.current_file = hasher.finish();
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
    }

    fn predeclare_items(&mut self, items: &[ast::Item]) -> Result<()> {
        for item in items {
            match item.kind() {
                ItemKind::Module(module) => {
                    self.allocate_def_id_for_item(item);
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

    fn parent_module_path(&self) -> Vec<String> {
        if self.module_path.is_empty() {
            Vec::new()
        } else {
            let mut parent = self.module_path.clone();
            parent.pop();
            parent
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
    }

    fn pop_value_scope(&mut self) {
        self.value_scopes.pop();
        if self.value_scopes.is_empty() {
            self.value_scopes.push(HashMap::new());
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
        self.reset_file_context(&file.path);
        self.predeclare_items(&file.items)?;
        self.prepare_lowering_state();
        let mut program = hir::Program::new();

        for item in &file.items {
            self.append_item(&mut program, item)?;
        }

        Ok(program)
    }

    fn append_item(&mut self, program: &mut hir::Program, item: &ast::Item) -> Result<()> {
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
            ItemKind::DeclFunction(_) => Ok(()),
            _ => {
                let hir_item = self.transform_item_to_hir(item)?;
                program.def_map.insert(hir_item.def_id, hir_item.clone());
                program.items.push(hir_item);
                Ok(())
            }
        }
    }

    // Expression lowering helpers live in expressions.rs
    /// Transform an AST item into a HIR statement
    fn transform_item_to_hir_stmt(&mut self, item: &ast::BItem) -> Result<hir::StmtKind> {
        let hir_item = self.transform_item_to_hir(item.as_ref())?;
        Ok(hir::StmtKind::Item(hir_item))
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
                let name = self.qualify_name(&struct_def.name.name);
                let fields = struct_def
                    .value
                    .fields
                    .iter()
                    .map(|field| {
                        Ok(hir::StructField {
                            hir_id: self.next_id(),
                            name: field.name.name.clone(),
                            ty: self.transform_type_to_hir(&field.value)?,
                            vis: hir::Visibility::Public,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                let generics = hir::Generics {
                    params: vec![],
                    where_clause: None,
                };

                (
                    hir::ItemKind::Struct(hir::Struct {
                        name,
                        fields,
                        generics,
                    }),
                    self.map_visibility(&struct_def.visibility),
                )
            }
            ItemKind::DefEnum(enum_def) => {
                self.register_type_def(&enum_def.name.name, def_id, &enum_def.visibility);
                let qualified_enum_name = self.qualify_name(&enum_def.name.name);
                let generics = hir::Generics {
                    params: Vec::new(),
                    where_clause: None,
                };

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

                        Ok(hir::EnumVariant {
                            hir_id: self.next_id(),
                            def_id: variant_def_id,
                            name: variant.name.name.clone(),
                            discriminant,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

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
            _ => {
                return Err(crate::error::optimization_error(format!(
                    "Unimplemented AST item type for HIR transformation: {:?}",
                    item
                )));
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
        let ty = if let Some(ty) = &const_def.ty {
            self.transform_type_to_hir(ty)?
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
            name: self.qualify_name(&const_def.name.name),
            ty,
            body,
        })
    }

    /// Transform an AST type into a HIR type
    fn transform_type_to_hir(&mut self, ty: &ast::Ty) -> Result<hir::TypeExpr> {
        match ty {
            ast::Ty::Primitive(prim) => Ok(self.primitive_type_to_hir(*prim)),
            ast::Ty::Struct(struct_ty) => Ok(hir::TypeExpr::new(
                self.next_id(),
                hir::TypeExprKind::Path(hir::Path {
                    segments: vec![self.make_path_segment(&struct_ty.name.name, None)],
                    res: None,
                }),
                Span::new(self.current_file, 0, 0),
            )),
            ast::Ty::Reference(reference) => {
                let inner = self.transform_type_to_hir(&reference.ty)?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Ref(Box::new(inner)),
                    Span::new(self.current_file, 0, 0),
                ))
            }
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
            ast::Ty::Vec(vec_ty) => {
                let args = self.convert_generic_args(&[*vec_ty.ty.clone()])?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Path(hir::Path {
                        segments: vec![self.make_path_segment("Vec", Some(args))],
                        res: None,
                    }),
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
            ast::Ty::Expr(expr) => {
                let path = self.ast_expr_to_hir_path(expr, PathResolutionScope::Type)?;
                Ok(hir::TypeExpr::new(
                    self.next_id(),
                    hir::TypeExprKind::Path(path),
                    Span::new(self.current_file, 0, 0),
                ))
            }
            _ => {
                // Fallback to unit type for unsupported types
                Ok(self.create_unit_type())
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
                    name: type_name.to_string(),
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
}

impl<'a> IrTransform<&'a ast::Expr, hir::Program> for HirGenerator {
    fn transform(&mut self, source: &'a ast::Expr) -> Result<hir::Program> {
        self.transform_expr(source)
    }
}

impl<'a> IrTransform<&'a ast::File, hir::Program> for HirGenerator {
    fn transform(&mut self, source: &'a ast::File) -> Result<hir::Program> {
        self.transform_file(source)
    }
}

impl Default for HirGenerator {
    fn default() -> Self {
        Self::new()
    }
}
