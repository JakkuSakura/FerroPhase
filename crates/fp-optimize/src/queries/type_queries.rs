// Type queries - stateless operations for type system information

use fp_core::ast::*;
use fp_core::context::SharedScopedContext;
use fp_core::ctx::ty::{FieldInfo, TypeId, TypeInfo, TypeRegistry};
use fp_core::diagnostics::report_error;
use fp_core::error::Result;
use fp_core::id::Ident;
use std::collections::HashSet;
use std::fmt::Write;
use std::sync::Arc;

/// Stateless type queries for introspection and validation
pub struct TypeQueries {
    type_registry: Arc<TypeRegistry>,
}

impl TypeQueries {
    pub fn new(type_registry: Arc<TypeRegistry>) -> Self {
        Self { type_registry }
    }

    /// Register basic types from AST
    pub fn register_basic_types(&self, ast: &Node) -> Result<()> {
        let (structs, aliases) = collect_type_items(ast);

        // Create stubs first so references across structs resolve by name
        for struct_def in &structs {
            self.register_struct_stub(struct_def);
        }
        for alias in &aliases {
            self.register_alias_stub(alias)?;
        }

        // Populate field information now that all names exist in the registry
        for struct_def in &structs {
            self.populate_struct_fields(struct_def)?;
        }

        Ok(())
    }

    /// Validate basic type references (non-const)
    pub fn validate_basic_references(&self, ast: &Node, _ctx: &SharedScopedContext) -> Result<()> {
        let (structs, aliases) = collect_type_items(ast);
        let mut missing = Vec::new();

        for alias in &aliases {
            if !self.type_exists(&alias.value) {
                missing.push(format!(
                    "type alias {} -> {}",
                    alias.name,
                    type_display(&alias.value)
                ));
            }
        }

        for struct_def in &structs {
            for field in &struct_def.value.fields {
                if !self.type_exists(&field.value) {
                    missing.push(format!(
                        "{}.{}: {}",
                        struct_def.name,
                        field.name,
                        type_display(&field.value)
                    ));
                }
            }
        }

        if missing.is_empty() {
            Ok(())
        } else {
            Err(report_error(format!(
                "Unresolved types: {}",
                missing.join(", ")
            )))
        }
    }

    /// Register generated types from metaprogramming
    pub fn register_generated_types(&self, ast: &Node) -> Result<()> {
        // Generated types appear in the AST after const-eval mutations are applied. The
        // basic registration path is idempotent, so we can reuse it safely.
        self.register_basic_types(ast)
    }

    /// Validate all type references including generated ones
    pub fn validate_all_references(&self, ast: &Node, ctx: &SharedScopedContext) -> Result<()> {
        self.validate_basic_references(ast, ctx)
    }

    /// Get size of a type in bytes
    pub fn sizeof(&self, type_name: &str) -> Result<usize> {
        if let Some(info) = self.type_registry.get_type_by_name(type_name) {
            return self.type_registry.get_size(info.id);
        }

        Ok(primitive_size(type_name).unwrap_or(8))
    }

    /// Check if a struct has a specific field
    pub fn hasfield(&self, struct_type: &str, field_name: &str) -> Result<bool> {
        if let Some(info) = self.type_registry.get_type_by_name(struct_type) {
            Ok(info.fields.iter().any(|field| field.name == field_name))
        } else {
            Ok(false)
        }
    }

    /// Get number of fields in a struct
    pub fn field_count(&self, struct_type: &str) -> Result<usize> {
        if let Some(info) = self.type_registry.get_type_by_name(struct_type) {
            Ok(info.fields.len())
        } else {
            Ok(0)
        }
    }

    /// Get field information for a struct
    pub fn reflect_fields(&self, struct_type: &str) -> Result<Vec<(String, String)>> {
        if let Some(info) = self.type_registry.get_type_by_name(struct_type) {
            Ok(info
                .fields
                .iter()
                .map(|field| (field.name.clone(), type_display(&field.ast_type)))
                .collect())
        } else {
            Ok(Vec::new())
        }
    }

    fn register_struct_stub(&self, struct_def: &ItemDefStruct) {
        let name = struct_def.name.name.clone();
        if self.type_registry.get_type_by_name(&name).is_some() {
            return;
        }

        let type_info = TypeInfo {
            id: TypeId::new(),
            name: name.clone(),
            ast_type: Ty::Struct(struct_def.value.clone()),
            size_bytes: None,
            fields: Vec::new(),
            methods: Vec::new(),
            traits_implemented: Vec::new(),
        };

        self.type_registry.register_type(type_info);
    }

    fn register_alias_stub(&self, alias: &ItemDefType) -> Result<()> {
        let name = alias.name.name.clone();
        if self.type_registry.get_type_by_name(&name).is_some() {
            return Ok(());
        }

        let referenced_id = self.resolve_or_register_type(&alias.value)?;
        let referenced_type = self
            .type_registry
            .get_type_info(referenced_id)
            .map(|info| info.ast_type)
            .unwrap_or_else(|| alias.value.clone());

        let type_info = TypeInfo {
            id: TypeId::new(),
            name: name.clone(),
            ast_type: referenced_type,
            size_bytes: None,
            fields: Vec::new(),
            methods: Vec::new(),
            traits_implemented: Vec::new(),
        };
        self.type_registry.register_type(type_info);

        Ok(())
    }

    fn populate_struct_fields(&self, struct_def: &ItemDefStruct) -> Result<()> {
        let name = struct_def.name.name.clone();
        let Some(info) = self.type_registry.get_type_by_name(&name) else {
            return Err(report_error(format!("Struct {} not registered", name)));
        };

        let mut known_fields: HashSet<String> =
            info.fields.iter().map(|field| field.name.clone()).collect();

        for field in &struct_def.value.fields {
            if known_fields.contains(field.name.as_str()) {
                continue;
            }

            let field_type_id = self.resolve_or_register_type(&field.value)?;
            let field_info = FieldInfo {
                name: field.name.name.clone(),
                type_id: field_type_id,
                ast_type: field.value.clone(),
                attributes: Vec::new(),
            };

            self.type_registry.add_field(info.id, field_info)?;
            known_fields.insert(field.name.name.clone());
        }

        Ok(())
    }

    fn resolve_or_register_type(&self, ty: &Ty) -> Result<TypeId> {
        match ty {
            Ty::Reference(reference) => self.resolve_or_register_type(&reference.ty),
            Ty::Vec(vec_ty) => {
                let element_id = self.resolve_or_register_type(&vec_ty.ty)?;
                let element_name = self
                    .type_registry
                    .get_type_info(element_id)
                    .map(|info| info.name)
                    .unwrap_or_else(|| type_display(&vec_ty.ty));
                let mut name = String::from("Vec<");
                let _ = write!(&mut name, "{}", element_name);
                name.push('>');
                Ok(self.ensure_named_type(&name, Ty::ident(Ident::new(name.clone()))))
            }
            Ty::Tuple(tuple) => {
                let mut name = String::from("(");
                for (idx, ty) in tuple.types.iter().enumerate() {
                    if idx > 0 {
                        name.push_str(", ");
                    }
                    let _ = write!(&mut name, "{}", type_display(ty));
                    let _ = self.resolve_or_register_type(ty)?;
                }
                name.push(')');
                Ok(self.ensure_named_type(&name, Ty::ident(Ident::new(name.clone()))))
            }
            Ty::Struct(struct_ty) => {
                let name = struct_ty.name.name.clone();
                if let Some(info) = self.type_registry.get_type_by_name(&name) {
                    Ok(info.id)
                } else {
                    // Inline struct literal – register it on the fly
                    let info = TypeInfo {
                        id: TypeId::new(),
                        name: name.clone(),
                        ast_type: Ty::Struct(struct_ty.clone()),
                        size_bytes: None,
                        fields: Vec::new(),
                        methods: Vec::new(),
                        traits_implemented: Vec::new(),
                    };
                    let type_id = info.id;
                    self.type_registry.register_type(info);
                    for field in &struct_ty.fields {
                        let field_type_id = self.resolve_or_register_type(&field.value)?;
                        let field_info = FieldInfo {
                            name: field.name.name.clone(),
                            type_id: field_type_id,
                            ast_type: field.value.clone(),
                            attributes: Vec::new(),
                        };
                        self.type_registry.add_field(type_id, field_info)?;
                    }
                    Ok(type_id)
                }
            }
            Ty::Expr(expr) => match expr.as_ref() {
                Expr::Locator(locator) => {
                    if let Some(ident) = locator.as_ident() {
                        Ok(self.ensure_named_type(ident.as_str(), Ty::ident(ident.clone())))
                    } else {
                        let display = type_display(ty);
                        Ok(
                            self.ensure_named_type(
                                &display,
                                Ty::ident(Ident::new(display.clone())),
                            ),
                        )
                    }
                }
                _ => {
                    let display = type_display(ty);
                    Ok(self.ensure_named_type(&display, Ty::ident(Ident::new(display.clone()))))
                }
            },
            _ => {
                let display = type_display(ty);
                Ok(self.ensure_named_type(&display, Ty::ident(Ident::new(display.clone()))))
            }
        }
    }

    fn ensure_named_type(&self, name: &str, ast_type: Ty) -> TypeId {
        if let Some(info) = self.type_registry.get_type_by_name(name) {
            info.id
        } else {
            let type_info = TypeInfo {
                id: TypeId::new(),
                name: name.to_string(),
                ast_type,
                size_bytes: primitive_size(name),
                fields: Vec::new(),
                methods: Vec::new(),
                traits_implemented: Vec::new(),
            };
            let type_id = type_info.id;
            self.type_registry.register_type(type_info);
            type_id
        }
    }

    fn type_exists(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Expr(expr) => match expr.as_ref() {
                Expr::Locator(locator) => locator
                    .as_ident()
                    .and_then(|ident| self.type_registry.get_type_by_name(ident.as_str()))
                    .is_some(),
                _ => {
                    let name = type_display(ty);
                    self.type_registry.get_type_by_name(&name).is_some()
                        || primitive_size(&name).is_some()
                }
            },
            _ => {
                let name = type_display(ty);
                self.type_registry.get_type_by_name(&name).is_some()
                    || primitive_size(&name).is_some()
            }
        }
    }
}

fn collect_type_items(ast: &Node) -> (Vec<ItemDefStruct>, Vec<ItemDefType>) {
    let mut structs = Vec::new();
    let mut aliases = Vec::new();

    walk_items(ast, &mut |item| match item {
        Item::DefStruct(def) => structs.push(def.clone()),
        Item::DefType(def) => aliases.push(def.clone()),
        _ => {}
    });

    (structs, aliases)
}

fn walk_items<F>(node: &Node, f: &mut F)
where
    F: FnMut(&Item),
{
    match node {
        Node::Item(item) => walk_item(item, f),
        Node::Expr(expr) => walk_expr(expr, f),
        Node::File(file) => {
            for item in &file.items {
                walk_item(item, f);
            }
        }
    }
}

fn walk_expr<F>(expr: &Expr, f: &mut F)
where
    F: FnMut(&Item),
{
    match expr {
        Expr::Block(block) => {
            for stmt in &block.stmts {
                match stmt {
                    BlockStmt::Item(item) => walk_item(item.as_ref(), f),
                    BlockStmt::Expr(inner) => walk_expr(inner.expr.as_ref(), f),
                    BlockStmt::Let(let_stmt) => {
                        if let Some(init) = &let_stmt.init {
                            walk_expr(init, f);
                        }
                        if let Some(diverge) = &let_stmt.diverge {
                            walk_expr(diverge, f);
                        }
                    }
                    _ => {}
                }
            }
        }
        Expr::If(if_expr) => {
            walk_expr(&if_expr.cond, f);
            walk_expr(&if_expr.then, f);
            if let Some(elze) = if_expr.elze.as_ref() {
                walk_expr(elze, f);
            }
        }
        Expr::Invoke(invoke) => {
            for arg in &invoke.args {
                walk_expr(arg, f);
            }
            if let ExprInvokeTarget::Expr(inner) = &invoke.target {
                walk_expr(inner.as_ref(), f);
            }
        }
        Expr::Let(let_expr) => {
            walk_expr(let_expr.expr.as_ref(), f);
        }
        Expr::Assign(assign) => {
            walk_expr(assign.target.as_ref(), f);
            walk_expr(assign.value.as_ref(), f);
        }
        Expr::Struct(struct_expr) => {
            for field in &struct_expr.fields {
                if let Some(value) = field.value.as_ref() {
                    walk_expr(value, f);
                }
            }
        }
        Expr::Item(item) => walk_item(item, f),
        _ => {}
    }
}

fn walk_item<F>(item: &Item, f: &mut F)
where
    F: FnMut(&Item),
{
    f(item);
    match item {
        Item::Module(module) => {
            for child in &module.items {
                walk_item(child, f);
            }
        }
        Item::Impl(item_impl) => {
            for child in &item_impl.items {
                walk_item(child, f);
            }
        }
        Item::Expr(expr) => walk_expr(expr, f),
        _ => {}
    }
}

fn type_display(ty: &Ty) -> String {
    ty.to_string()
}

fn primitive_size(name: &str) -> Option<usize> {
    match name {
        "i8" | "u8" => Some(1),
        "i16" | "u16" => Some(2),
        "i32" | "u32" | "f32" => Some(4),
        "i64" | "u64" | "f64" => Some(8),
        "bool" => Some(1),
        "char" => Some(4),
        _ => None,
    }
}
