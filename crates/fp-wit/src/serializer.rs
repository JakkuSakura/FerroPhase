use std::collections::{HashMap, HashSet};
use std::fmt::Write;

use fp_core::ast::{
    self, AstSerializer, AttrMeta, Attribute, Expr, ExprInvokeTarget, ExprKind, FunctionSignature,
    Ident, Item, ItemImpl, ItemKind, Locator, Node, NodeKind, Ty, TypeInt, TypePrimitive,
    TypeStructural, Value, Visibility,
};
use fp_core::error::{Error as CoreError, Result};

/// Serializes FerroPhase AST nodes into WIT source text.
#[derive(Debug, Clone)]
pub struct WitSerializer {
    options: WitOptions,
}

impl WitSerializer {
    pub fn new() -> Self {
        Self::with_options(WitOptions::default())
    }

    pub fn with_options(options: WitOptions) -> Self {
        Self { options }
    }

    pub fn with_world_mode(world_mode: WorldMode) -> Self {
        let mut options = WitOptions::default();
        options.world_mode = world_mode;
        Self::with_options(options)
    }
}

#[derive(Debug, Clone)]
pub struct WitOptions {
    pub package: String,
    pub root_interface: String,
    pub world_mode: WorldMode,
}

impl Default for WitOptions {
    fn default() -> Self {
        Self {
            package: "ferrophase:generated".to_string(),
            root_interface: "ferrophase".to_string(),
            world_mode: WorldMode::PerPackage,
        }
    }
}

#[derive(Debug, Clone)]
pub enum WorldMode {
    Single { world_name: String },
    PerPackage,
}

impl AstSerializer for WitSerializer {
    fn serialize_node(&self, node: &Node) -> Result<String> {
        let mut emitter = WitEmitter::new(self.options.clone());
        emitter.emit_node(node)?;
        Ok(emitter.finish())
    }
}

fn collect_doc_strings(attrs: &[Attribute]) -> Vec<String> {
    let mut docs = Vec::new();
    for attr in attrs {
        if let AttrMeta::NameValue(name_value) = &attr.meta {
            if name_value.name.last().as_str() == "doc" {
                if let Some(text) = expr_to_string(&name_value.value) {
                    for line in text.lines() {
                        let trimmed = line.trim();
                        if trimmed.is_empty() {
                            docs.push(String::new());
                        } else {
                            docs.push(trimmed.to_string());
                        }
                    }
                }
            }
        }
    }
    docs
}

fn expr_to_string(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Value(value) => match value.as_ref() {
            Value::String(s) => Some(s.value.clone()),
            _ => None,
        },
        _ => None,
    }
}

#[derive(Clone)]
struct ReceiverContext {
    namespace: Option<String>,
    type_name: String,
}

struct WitEmitter {
    options: WitOptions,
    root: InterfaceBuilder,
    interfaces: HashMap<String, InterfaceBuilder>,
    exports: HashSet<String>,
    interface_packages: HashMap<String, String>,
}

impl WitEmitter {
    fn new(options: WitOptions) -> Self {
        let root_name = options.root_interface.clone();
        Self {
            options,
            root: InterfaceBuilder::new(root_name.clone(), root_name),
            interfaces: HashMap::new(),
            exports: HashSet::new(),
            interface_packages: HashMap::new(),
        }
    }

    fn emit_node(&mut self, node: &Node) -> Result<()> {
        match node.kind() {
            NodeKind::File(file) => {
                for item in &file.items {
                    self.emit_item(item, None, None, None, None)?;
                }
                Ok(())
            }
            NodeKind::Item(item) => self.emit_item(item, None, None, None, None),
            NodeKind::Expr(_) => Err(CoreError::from(
                "WIT serialization expects a file or module-level item".to_string(),
            )),
            NodeKind::Query(_) => Err(CoreError::from(
                "WIT serialization does not support query documents".to_string(),
            )),
            NodeKind::Schema(_) => Err(CoreError::from(
                "WIT serialization does not support schema documents".to_string(),
            )),
            NodeKind::Workspace(_) => Err(CoreError::from(
                "WIT serialization does not support workspace documents".to_string(),
            )),
        }
    }

    fn emit_item(
        &mut self,
        item: &Item,
        scope: Option<String>,
        current_interface: Option<String>,
        current_package: Option<String>,
        receiver_ctx: Option<ReceiverContext>,
    ) -> Result<()> {
        let scope_ref = scope.as_deref();
        match item.kind() {
            ItemKind::Module(module) => {
                let interface_name = interface_name(scope_ref, &module.name);
                let package = if scope.is_none() {
                    interface_name.clone()
                } else {
                    current_package
                        .clone()
                        .or_else(|| {
                            scope_ref.and_then(|name| self.interface_packages.get(name).cloned())
                        })
                        .unwrap_or_else(|| interface_name.clone())
                };
                self.interface_packages
                    .entry(interface_name.clone())
                    .or_insert_with(|| package.clone());
                for inner in &module.items {
                    self.emit_item(
                        inner,
                        Some(interface_name.clone()),
                        Some(interface_name.clone()),
                        Some(package.clone()),
                        None,
                    )?;
                }
                if self
                    .interfaces
                    .get(&interface_name)
                    .map(|builder| builder.has_entries())
                    .unwrap_or(false)
                {
                    self.exports.insert(interface_name);
                } else {
                    self.interfaces.remove(&interface_name);
                }
                Ok(())
            }
            ItemKind::Macro(_) => {
                // Item macros are not material for WIT serialization; skip.
                Ok(())
            }
            ItemKind::Impl(impl_block) => {
                if let Some(interface_name) = impl_interface_name(scope_ref, impl_block) {
                    let package = current_package
                        .clone()
                        .or_else(|| {
                            scope_ref.and_then(|name| self.interface_packages.get(name).cloned())
                        })
                        .unwrap_or_else(|| interface_name.clone());
                    self.interface_packages
                        .entry(interface_name.clone())
                        .or_insert_with(|| package.clone());
                    let type_name = locator_to_ident_from_expr(&impl_block.self_ty)
                        .map(|ident| sanitize_type_identifier(ident.as_str()))
                        .unwrap_or_else(|| "self".to_string());
                    let receiver = ReceiverContext {
                        namespace: scope_ref.map(|s| s.to_string()),
                        type_name,
                    };
                    for inner in &impl_block.items {
                        self.emit_item(
                            inner,
                            Some(interface_name.clone()),
                            Some(interface_name.clone()),
                            Some(package.clone()),
                            Some(receiver.clone()),
                        )?;
                    }
                    if self
                        .interfaces
                        .get(&interface_name)
                        .map(|builder| builder.has_entries())
                        .unwrap_or(false)
                    {
                        self.exports.insert(interface_name);
                    } else {
                        self.interfaces.remove(&interface_name);
                    }
                }
                Ok(())
            }
            ItemKind::DefStruct(def) => {
                self.builder_for(current_interface.as_deref(), current_package.as_deref())
                    .add_struct(def);
                Ok(())
            }
            ItemKind::DefEnum(def) => {
                self.builder_for(current_interface.as_deref(), current_package.as_deref())
                    .add_enum(def);
                Ok(())
            }
            ItemKind::DefType(def) => {
                self.builder_for(current_interface.as_deref(), current_package.as_deref())
                    .add_alias(def);
                Ok(())
            }
            ItemKind::DefFunction(def) => {
                let docs = collect_doc_strings(&def.attrs);
                self.builder_for(current_interface.as_deref(), current_package.as_deref())
                    .add_function(
                        &def.sig,
                        &def.name,
                        def.visibility,
                        docs,
                        receiver_ctx.as_ref(),
                        current_package.as_deref(),
                        scope_ref,
                    );
                Ok(())
            }
            ItemKind::DeclFunction(decl) => {
                self.builder_for(current_interface.as_deref(), current_package.as_deref())
                    .add_function(
                        &decl.sig,
                        &decl.name,
                        Visibility::Public,
                        Vec::new(),
                        receiver_ctx.as_ref(),
                        current_package.as_deref(),
                        scope_ref,
                    );
                Ok(())
            }
            ItemKind::Expr(_)
            | ItemKind::Any(_)
            | ItemKind::Import(_)
            | ItemKind::DeclConst(_)
            | ItemKind::DeclStatic(_)
            | ItemKind::DeclType(_)
            | ItemKind::DefConst(_)
            | ItemKind::DefStatic(_)
            | ItemKind::DefStructural(_)
            | ItemKind::DefTrait(_) => Ok(()),
        }
    }

    fn builder_for(
        &mut self,
        interface: Option<&str>,
        package: Option<&str>,
    ) -> &mut InterfaceBuilder {
        match interface {
            Some(name) => {
                let package_name = package
                    .map(|s| s.to_string())
                    .or_else(|| self.interface_packages.get(name).cloned())
                    .unwrap_or_else(|| name.to_string());
                self.interface_packages
                    .entry(name.to_string())
                    .or_insert_with(|| package_name.clone());
                self.interfaces
                    .entry(name.to_string())
                    .or_insert_with(|| InterfaceBuilder::new(name.to_string(), package_name))
            }
            None => &mut self.root,
        }
    }

    fn finish(mut self) -> String {
        let mut rendered: Vec<RenderedInterface> = self
            .interfaces
            .drain()
            .map(|(_, builder)| builder.finish())
            .collect();

        if self.root.has_entries() {
            let root_name = self.root.name().to_string();
            self.exports.insert(root_name.clone());
            rendered.push(self.root.finish());
        }

        rendered.sort_by(|a, b| a.name.cmp(&b.name));
        rendered.dedup_by(|a, b| a.name == b.name);

        let mut output = String::new();
        let _ = writeln!(output, "package {};\n", self.options.package);

        for interface in &rendered {
            output.push_str(&interface.source);
            if !interface.source.ends_with('\n') {
                output.push('\n');
            }
        }

        let package_by_interface: HashMap<_, _> = rendered
            .iter()
            .map(|ri| (ri.name.clone(), ri.package.clone()))
            .collect();

        match &self.options.world_mode {
            WorldMode::Single { world_name } => {
                let mut exports: Vec<String> = self.exports.into_iter().collect();
                exports.sort();
                if !exports.is_empty() {
                    let _ = writeln!(output, "world {world_name} {{");
                    for export in exports {
                        let _ = writeln!(output, "    export {export};");
                    }
                    let _ = writeln!(output, "}}");
                }
            }
            WorldMode::PerPackage => {
                let mut grouped: HashMap<String, Vec<String>> = HashMap::new();
                for export in &self.exports {
                    if let Some(package) = package_by_interface.get(export) {
                        grouped
                            .entry(package.clone())
                            .or_default()
                            .push(export.clone());
                    }
                }
                let mut packages: Vec<_> = grouped.into_iter().collect();
                packages.sort_by(|a, b| a.0.cmp(&b.0));
                for (package, mut exports) in packages {
                    exports.sort();
                    let world_name = sanitize_identifier(&format!("{package}-world"));
                    let _ = writeln!(output, "world {world_name} {{");
                    for export in exports {
                        let _ = writeln!(output, "    export {export};");
                    }
                    let _ = writeln!(output, "}}\n");
                }
            }
        }

        output
    }
}

#[derive(Debug)]
struct InterfaceBuilder {
    name: String,
    package: String,
    entries: Vec<InterfaceEntry>,
    seen_functions: HashSet<String>,
    uses: HashMap<String, HashSet<String>>,
    defined_types: HashSet<String>,
    structural_types: HashMap<String, String>,
    structural_sequence: usize,
}

impl InterfaceBuilder {
    fn new(name: String, package: String) -> Self {
        Self {
            name,
            package,
            entries: Vec::new(),
            seen_functions: HashSet::new(),
            uses: HashMap::new(),
            defined_types: HashSet::new(),
            structural_types: HashMap::new(),
            structural_sequence: 0,
        }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn has_entries(&self) -> bool {
        !self.entries.is_empty()
    }

    fn add_struct(&mut self, def: &ast::ItemDefStruct) {
        let package_name = self.package.clone();
        let mut fields = Vec::new();
        for field in &def.value.fields {
            let field_hint = format!("{}-{}", def.name.as_str(), field.name.as_str());
            let wit_ty = self.wit_type_for(
                &field.value,
                Some(&field_hint),
                None,
                Some(package_name.as_str()),
                None,
            );
            self.register_type_use(&field.value, &wit_ty, Some(package_name.as_str()), None);
            fields.push((sanitize_identifier(field.name.as_str()), wit_ty));
        }
        let name = sanitize_type_identifier(def.name.as_str());
        self.defined_types.insert(name.clone());
        self.entries.push(InterfaceEntry::Record {
            name,
            fields,
            docs: Vec::new(),
        });
    }

    fn add_enum(&mut self, def: &ast::ItemDefEnum) {
        let name = sanitize_type_identifier(def.name.as_str());
        if ENUMS_AS_STRINGS.contains(&name.as_str()) {
            self.defined_types.insert(name.clone());
            self.entries.push(InterfaceEntry::Alias {
                name,
                target: "string".to_string(),
                docs: Vec::new(),
            });
            return;
        }
        let package_name = self.package.clone();
        let mut cases = Vec::new();
        for variant in &def.value.variants {
            let assoc = match &variant.value {
                ast::Ty::Unit(_) => None,
                other => {
                    let variant_hint = format!("{}-{}", def.name.as_str(), variant.name.as_str());
                    let wit_ty = self.wit_type_for(
                        other,
                        Some(&variant_hint),
                        None,
                        Some(package_name.as_str()),
                        None,
                    );
                    self.register_type_use(other, &wit_ty, Some(package_name.as_str()), None);
                    Some(wit_ty)
                }
            };
            cases.push((ident_to_wit(&variant.name), assoc));
        }
        self.defined_types.insert(name.clone());
        self.entries.push(InterfaceEntry::Variant {
            name,
            cases,
            docs: Vec::new(),
        });
    }

    fn add_alias(&mut self, def: &ast::ItemDefType) {
        let name = sanitize_type_identifier(def.name.as_str());
        let package_name = self.package.clone();
        let alias_hint = format!("{}-alias", def.name.as_str());
        let target = self.wit_type_for(
            &def.value,
            Some(&alias_hint),
            None,
            Some(package_name.as_str()),
            None,
        );
        self.register_type_use(&def.value, &target, Some(package_name.as_str()), None);
        if name == target {
            return;
        }
        self.defined_types.insert(name.clone());
        self.entries.push(InterfaceEntry::Alias {
            name,
            target,
            docs: Vec::new(),
        });
    }

    fn add_function(
        &mut self,
        sig: &FunctionSignature,
        name: &Ident,
        _visibility: Visibility,
        docs: Vec<String>,
        receiver_ctx: Option<&ReceiverContext>,
        current_package: Option<&str>,
        receiver_scope: Option<&str>,
    ) {
        let mut params = Vec::new();
        if sig.receiver.is_some() {
            let ty = receiver_ctx
                .map(|ctx| ctx.type_name.clone())
                .unwrap_or_else(|| {
                    receiver_scope
                        .map(sanitize_identifier)
                        .unwrap_or_else(|| "self".to_string())
                });
            if let Some(ctx) = receiver_ctx {
                if let Some(namespace) = &ctx.namespace {
                    if namespace != &self.name {
                        self.add_use(namespace, &ctx.type_name);
                    }
                }
            }
            params.push(("self".to_string(), ty));
        }

        for (idx, param) in sig.params.iter().enumerate() {
            let pname = if param.name.as_str().is_empty() {
                format!("arg{idx}")
            } else {
                ident_to_wit(&param.name)
            };
            let param_hint = format!("{}-{}", name.as_str(), pname);
            let wit_ty = self.wit_type_for(
                &param.ty,
                Some(&param_hint),
                receiver_scope,
                current_package,
                receiver_ctx,
            );
            self.register_type_use(&param.ty, &wit_ty, current_package, receiver_ctx);
            params.push((pname, wit_ty));
        }

        let result = sig.ret_ty.as_ref().map(|ty| {
            let result_hint = format!("{}-result", name.as_str());
            let wit_ty = self.wit_type_for(
                ty,
                Some(&result_hint),
                receiver_scope,
                current_package,
                receiver_ctx,
            );
            self.register_type_use(ty, &wit_ty, current_package, receiver_ctx);
            wit_ty
        });

        let function_name = ident_to_wit(name);
        if !self.seen_functions.insert(function_name.clone()) {
            return;
        }

        self.entries.push(InterfaceEntry::Function {
            name: function_name,
            params,
            result,
            docs,
        });
    }

    fn wit_type_for(
        &mut self,
        ty: &Ty,
        hint: Option<&str>,
        receiver_scope: Option<&str>,
        current_package: Option<&str>,
        receiver_ctx: Option<&ReceiverContext>,
    ) -> String {
        match ty {
            Ty::Primitive(p) => match p {
                TypePrimitive::Bool => "bool".to_string(),
                TypePrimitive::Char => "char".to_string(),
                TypePrimitive::String => "string".to_string(),
                TypePrimitive::Int(int_ty) => match int_ty {
                    TypeInt::I8 => "s8".to_string(),
                    TypeInt::I16 => "s16".to_string(),
                    TypeInt::I32 => "s32".to_string(),
                    TypeInt::I64 => "s64".to_string(),
                    TypeInt::U8 => "u8".to_string(),
                    TypeInt::U16 => "u16".to_string(),
                    TypeInt::U32 => "u32".to_string(),
                    TypeInt::U64 => "u64".to_string(),
                    TypeInt::BigInt => "s64".to_string(),
                },
                TypePrimitive::Decimal(dec) => match dec {
                    ast::DecimalType::F32 => "f32".to_string(),
                    ast::DecimalType::F64 => "f64".to_string(),
                    _ => "f64".to_string(),
                },
                TypePrimitive::List => "list<string>".to_string(),
            },
            Ty::Reference(reference) => self.wit_type_for(
                reference.ty.as_ref(),
                hint,
                receiver_scope,
                current_package,
                receiver_ctx,
            ),
            Ty::Tuple(tuple) => {
                let mut parts = Vec::new();
                for (idx, inner) in tuple.types.iter().enumerate() {
                    let nested_hint = hint.map(|base| format!("{base}-item{idx}"));
                    let wit = self.wit_type_for(
                        inner,
                        nested_hint.as_deref(),
                        receiver_scope,
                        current_package,
                        receiver_ctx,
                    );
                    parts.push(wit);
                }
                format!("tuple<{}>", parts.join(", "))
            }
            Ty::Vec(vec_ty) => {
                let nested_hint = hint.map(|base| format!("{base}-item"));
                let inner = self.wit_type_for(
                    vec_ty.ty.as_ref(),
                    nested_hint.as_deref(),
                    receiver_scope,
                    current_package,
                    receiver_ctx,
                );
                format!("list<{inner}>")
            }
            Ty::Array(array) => {
                let nested_hint = hint.map(|base| format!("{base}-item"));
                let inner = self.wit_type_for(
                    array.elem.as_ref(),
                    nested_hint.as_deref(),
                    receiver_scope,
                    current_package,
                    receiver_ctx,
                );
                format!("list<{inner}>")
            }
            Ty::Slice(slice) => {
                let nested_hint = hint.map(|base| format!("{base}-item"));
                let inner = self.wit_type_for(
                    slice.elem.as_ref(),
                    nested_hint.as_deref(),
                    receiver_scope,
                    current_package,
                    receiver_ctx,
                );
                format!("list<{inner}>")
            }
            Ty::Struct(s) => normalize_type_name(sanitize_type_identifier(s.name.as_str())),
            Ty::Enum(e) => normalize_type_name(sanitize_type_identifier(e.name.as_str())),
            Ty::Unit(_) => "tuple<>".to_string(),
            Ty::Structural(structural) => self.ensure_structural_type(
                structural,
                hint,
                receiver_scope,
                current_package,
                receiver_ctx,
            ),
            Ty::Any(_) | Ty::TypeBinaryOp(_) | Ty::AnyBox(_) | Ty::Unknown(_) | Ty::Nothing(_) => "json".to_string(),
            Ty::Type(_) | Ty::TypeBounds(_) | Ty::ImplTraits(_) => "json".to_string(),
            Ty::Value(_) => "json".to_string(),
            Ty::Function(_) => "func".to_string(),
            Ty::Expr(expr) => {
                expr_to_wit_type(expr, receiver_scope).unwrap_or_else(|| "json".to_string())
            }
            Ty::QuoteToken(_) => "json".to_string(),
        }
    }

    fn ensure_structural_type(
        &mut self,
        structural: &TypeStructural,
        hint: Option<&str>,
        receiver_scope: Option<&str>,
        current_package: Option<&str>,
        receiver_ctx: Option<&ReceiverContext>,
    ) -> String {
        let fingerprint = InterfaceBuilder::structural_fingerprint(structural);
        if let Some(existing) = self.structural_types.get(&fingerprint) {
            return existing.clone();
        }

        let name = self.next_structural_name(hint);
        self.structural_types.insert(fingerprint, name.clone());
        self.defined_types.insert(name.clone());

        let mut fields = Vec::new();
        for field in &structural.fields {
            let field_name = sanitize_identifier(field.name.as_str());
            let nested_hint = format!("{name}-{field_name}");
            let wit = self.wit_type_for(
                &field.value,
                Some(&nested_hint),
                receiver_scope,
                current_package,
                receiver_ctx,
            );
            self.register_type_use(&field.value, &wit, current_package, receiver_ctx);
            fields.push((field_name, wit));
        }

        self.entries.insert(
            0,
            InterfaceEntry::Record {
                name: name.clone(),
                fields,
                docs: Vec::new(),
            },
        );

        name
    }

    fn next_structural_name(&mut self, hint: Option<&str>) -> String {
        let mut base = hint
            .map(|h| sanitize_type_identifier(h))
            .filter(|name| !name.is_empty())
            .unwrap_or_else(|| format!("struct-{}", self.structural_sequence + 1));

        if base.is_empty() {
            base = format!("struct-{}", self.structural_sequence + 1);
        }

        let mut candidate = base.clone();
        let mut counter = 1;
        while self.defined_types.contains(&candidate)
            || self
                .structural_types
                .values()
                .any(|existing| existing == &candidate)
        {
            counter += 1;
            candidate = format!("{base}-{counter}");
        }

        self.structural_sequence += 1;
        candidate
    }

    fn structural_fingerprint(structural: &TypeStructural) -> String {
        if structural.fields.is_empty() {
            return "<empty>".to_string();
        }
        let mut parts = Vec::new();
        for field in &structural.fields {
            parts.push(format!(
                "{}:{}",
                field.name.as_str(),
                InterfaceBuilder::ty_fingerprint(&field.value)
            ));
        }
        parts.join("|")
    }

    fn ty_fingerprint(ty: &Ty) -> String {
        match ty {
            Ty::Structural(structural) => InterfaceBuilder::structural_fingerprint(structural),
            _ => format!("{ty:?}"),
        }
    }

    fn finish(mut self) -> RenderedInterface {
        self.ensure_aliases();

        let mut out = String::new();
        let _ = writeln!(out, "interface {} {{", self.name);
        if !self.uses.is_empty() {
            let mut namespaces: Vec<_> = self.uses.into_iter().collect();
            namespaces.sort_by(|a, b| a.0.cmp(&b.0));
            for (namespace, names) in namespaces {
                let mut items: Vec<_> = names.into_iter().collect();
                items.sort();
                let joined = items.join(", ");
                let _ = writeln!(out, "    use {namespace}.{{{joined}}};\n");
            }
        }
        for entry in self.entries {
            match entry {
                InterfaceEntry::Record { name, fields, docs } => {
                    write_docs(&mut out, 4, &docs);
                    if fields.is_empty() {
                        let _ = writeln!(out, "    record {name} {{}}\n");
                    } else {
                        let _ = writeln!(out, "    record {name} {{");
                        for (field, ty) in fields {
                            let _ = writeln!(out, "        {field}: {ty},");
                        }
                        let _ = writeln!(out, "    }}\n");
                    }
                }
                InterfaceEntry::Variant { name, cases, docs } => {
                    write_docs(&mut out, 4, &docs);
                    if cases.is_empty() {
                        let _ = writeln!(out, "    variant {name} {{}}\n");
                    } else {
                        let _ = writeln!(out, "    variant {name} {{");
                        for (case, assoc) in cases {
                            if let Some(ty) = assoc {
                                let _ = writeln!(out, "        {case}({ty}),");
                            } else {
                                let _ = writeln!(out, "        {case},");
                            }
                        }
                        let _ = writeln!(out, "    }}\n");
                    }
                }
                InterfaceEntry::Alias { name, target, docs } => {
                    write_docs(&mut out, 4, &docs);
                    let _ = writeln!(out, "    type {name} = {target};\n");
                }
                InterfaceEntry::Function {
                    name,
                    params,
                    result,
                    docs,
                } => {
                    write_docs(&mut out, 4, &docs);
                    let params_str = params
                        .iter()
                        .map(|(param, ty)| format!("{param}: {ty}"))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let result_str = result.map(|ty| format!(" -> {ty}")).unwrap_or_default();
                    let _ = writeln!(out, "    {name}: func({params_str}){result_str};\n");
                }
            }
        }
        let _ = writeln!(out, "}}\n");
        RenderedInterface {
            name: self.name,
            package: self.package,
            source: out,
        }
    }
}

impl Default for InterfaceBuilder {
    fn default() -> Self {
        Self::new(String::new(), String::new())
    }
}

impl InterfaceBuilder {
    fn add_use(&mut self, namespace: &str, name: &str) {
        let entry = self
            .uses
            .entry(namespace.to_string())
            .or_insert_with(HashSet::new);
        entry.insert(name.to_string());
    }

    fn register_type_use(
        &mut self,
        ty: &Ty,
        _wit_ty: &str,
        current_package: Option<&str>,
        receiver_ctx: Option<&ReceiverContext>,
    ) {
        let mut references = Vec::new();
        collect_type_references(ty, &mut references);

        for (namespace, name) in references {
            if Some(namespace.as_str()) == current_package {
                continue;
            }
            if receiver_ctx
                .and_then(|ctx| ctx.namespace.as_deref())
                .map(|ns| ns == namespace)
                .unwrap_or(false)
            {
                continue;
            }
            if namespace == self.name {
                continue;
            }
            if namespace.is_empty() || !namespace.starts_with("fp-") {
                continue;
            }
            self.add_use(&namespace, &name);
        }
    }

    fn rewrite_type_usages(&mut self, target: &str, replacement: &str) {
        for entry in &mut self.entries {
            match entry {
                InterfaceEntry::Record { fields, .. } => {
                    for (_, ty) in fields {
                        *ty = rewrite_type_token(target, replacement, ty);
                    }
                }
                InterfaceEntry::Variant { cases, .. } => {
                    for (_, assoc) in cases {
                        if let Some(ty) = assoc {
                            *ty = rewrite_type_token(target, replacement, ty);
                        }
                    }
                }
                InterfaceEntry::Alias {
                    target: alias_target,
                    ..
                } => {
                    *alias_target = rewrite_type_token(target, replacement, alias_target);
                }
                InterfaceEntry::Function { params, result, .. } => {
                    for (_, ty) in params {
                        *ty = rewrite_type_token(target, replacement, ty);
                    }
                    if let Some(res_ty) = result {
                        *res_ty = rewrite_type_token(target, replacement, res_ty);
                    }
                }
            }
        }
    }

    fn ensure_aliases(&mut self) {
        let mut referenced = HashSet::new();
        for entry in &self.entries {
            collect_entry_types(entry, &mut referenced);
        }

        if referenced.is_empty() {
            return;
        }

        let function_names: HashSet<String> = self
            .entries
            .iter()
            .filter_map(|entry| match entry {
                InterfaceEntry::Function { name, .. } => Some(name.clone()),
                _ => None,
            })
            .collect();

        let mut alias_types = Vec::new();
        for ty in referenced {
            if !self.needs_alias(&ty) {
                continue;
            }
            if function_names.contains(&ty) {
                self.rewrite_type_usages(&ty, "string");
                continue;
            }
            alias_types.push(ty);
        }

        if alias_types.is_empty() {
            return;
        }

        alias_types.sort();
        for ty in alias_types.into_iter().rev() {
            let target = InterfaceBuilder::alias_default_target(&ty).to_string();
            self.entries.insert(
                0,
                InterfaceEntry::Alias {
                    name: ty.clone(),
                    target,
                    docs: Vec::new(),
                },
            );
            self.defined_types.insert(ty);
        }
    }

    fn needs_alias(&self, ty: &str) -> bool {
        if ty.is_empty() {
            return false;
        }
        if is_builtin_wit_type(ty) {
            return false;
        }
        if self.defined_types.contains(ty) {
            return false;
        }
        if self.uses.values().any(|names| names.contains(ty)) {
            return false;
        }
        if ty.chars().all(|ch| ch.is_ascii_digit()) {
            return false;
        }
        true
    }

    fn alias_default_target(name: &str) -> &'static str {
        match name {
            "string" | "str" => "string",
            "strings" => "list<string>",
            "bool" | "boolean" => "bool",
            "int" | "num" => "f64",
            "ints" | "nums" => "list<f64>",
            "json" => "any",
            _ => "any",
        }
    }
}

#[derive(Debug)]
struct RenderedInterface {
    name: String,
    package: String,
    source: String,
}

fn write_docs(out: &mut String, indent: usize, docs: &[String]) {
    if docs.is_empty() {
        return;
    }
    let prefix = " ".repeat(indent);
    for doc in docs {
        if doc.is_empty() {
            let _ = writeln!(out, "{prefix}///");
        } else {
            let _ = writeln!(out, "{prefix}/// {}", doc);
        }
    }
}

fn collect_entry_types(entry: &InterfaceEntry, out: &mut HashSet<String>) {
    match entry {
        InterfaceEntry::Record { fields, .. } => {
            for (_, ty) in fields {
                collect_type_tokens(ty, out);
            }
        }
        InterfaceEntry::Variant { cases, .. } => {
            for (_, assoc) in cases {
                if let Some(ty) = assoc {
                    collect_type_tokens(ty, out);
                }
            }
        }
        InterfaceEntry::Alias { target, .. } => {
            collect_type_tokens(target, out);
        }
        InterfaceEntry::Function { params, result, .. } => {
            for (_, ty) in params {
                collect_type_tokens(ty, out);
            }
            if let Some(ty) = result {
                collect_type_tokens(ty, out);
            }
        }
    }
}

fn collect_type_tokens(ty: &str, out: &mut HashSet<String>) {
    let mut current = String::new();
    for ch in ty.chars() {
        if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
            current.push(ch);
        } else if !current.is_empty() {
            out.insert(current.clone());
            current.clear();
        }
    }
    if !current.is_empty() {
        out.insert(current);
    }
}

fn rewrite_type_token(target: &str, replacement: &str, ty: &str) -> String {
    if !ty.contains(target) {
        return ty.to_string();
    }

    let mut result = String::with_capacity(ty.len());
    let mut current = String::new();
    for ch in ty.chars() {
        if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
            current.push(ch);
        } else {
            if !current.is_empty() {
                if current == target {
                    result.push_str(replacement);
                } else {
                    result.push_str(&current);
                }
                current.clear();
            }
            result.push(ch);
        }
    }
    if !current.is_empty() {
        if current == target {
            result.push_str(replacement);
        } else {
            result.push_str(&current);
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::{
        AttrMeta, AttrMetaNameValue, AttrStyle, Attribute, Expr, File as AstFile, Ident, Item,
        ItemDefFunction, Node, NodeKind, Path, Value,
    };
    use std::path::PathBuf;

    fn doc_attr(text: &str) -> Attribute {
        Attribute {
            style: AttrStyle::Outer,
            meta: AttrMeta::NameValue(AttrMetaNameValue {
                name: Path::from_ident(Ident::new("doc")),
                value: Expr::value(Value::string(text.to_string())).into(),
            }),
        }
    }

    #[test]
    fn function_docs_are_serialized() {
        let mut func = ItemDefFunction::new_simple(Ident::new("greet"), Expr::unit().into());
        func.attrs.push(doc_attr("Greets the caller"));

        let item = Item::from(func);
        let file = AstFile {
            path: PathBuf::new(),
            items: vec![item],
        };
        let node = Node::from(NodeKind::File(file));

        let serializer = WitSerializer::new();
        let wit = serializer.serialize_node(&node).expect("serialize to wit");

        assert!(wit.contains("/// Greets the caller"));
    }
}

const BUILTIN_WIT_TYPES: &[&str] = &[
    "bool", "string", "char", "s8", "s16", "s32", "s64", "u8", "u16", "u32", "u64", "f32", "f64",
    "tuple", "list", "option", "result", "future", "stream", "func", "any",
];

fn is_builtin_wit_type(name: &str) -> bool {
    BUILTIN_WIT_TYPES.contains(&name)
}

const ENUMS_AS_STRINGS: &[&str] = &["scheme-type"];

fn collect_type_references(ty: &Ty, out: &mut Vec<(String, String)>) {
    match ty {
        Ty::Reference(reference) => collect_type_references(&reference.ty, out),
        Ty::Vec(vec_ty) => collect_type_references(vec_ty.ty.as_ref(), out),
        Ty::Array(array) => collect_type_references(array.elem.as_ref(), out),
        Ty::Slice(slice) => collect_type_references(slice.elem.as_ref(), out),
        Ty::Tuple(tuple) => tuple
            .types
            .iter()
            .for_each(|ty| collect_type_references(ty, out)),
        Ty::Expr(expr) => collect_type_references_expr(expr, out),
        Ty::Value(_) => {}
        _ => {}
    }
}

fn collect_type_references_expr(expr: &Expr, out: &mut Vec<(String, String)>) {
    match expr.kind() {
        ExprKind::Locator(locator) => {
            if let Some((namespace, name)) = locator_namespace_and_name(locator) {
                out.push((namespace, name));
            }
        }
        ExprKind::Invoke(invoke) => {
            match &invoke.target {
                ExprInvokeTarget::Function(locator) => {
                    if let Some((namespace, name)) = locator_namespace_and_name(locator) {
                        out.push((namespace, name));
                    }
                }
                ExprInvokeTarget::Type(ty) => collect_type_references(ty, out),
                ExprInvokeTarget::Expr(inner) => collect_type_references_expr(inner, out),
                _ => {}
            }
            for arg in &invoke.args {
                collect_type_references_expr(arg, out);
            }
        }
        ExprKind::Value(value) => match value.as_ref() {
            Value::Type(ty) => collect_type_references(ty, out),
            _ => {}
        },
        _ => {}
    }
}

fn locator_namespace_and_name(locator: &Locator) -> Option<(String, String)> {
    match locator {
        Locator::Path(path) => {
            if path.segments.is_empty() {
                return None;
            }
            let mut segments: Vec<String> = path
                .segments
                .iter()
                .map(|seg| sanitize_identifier(seg.as_str()))
                .collect();
            if segments.len() < 2 {
                return None;
            }
            let name = segments.pop()?;
            let namespace = segments.join("-");
            if namespace.starts_with("fp-core-error")
                || namespace.starts_with("fp-core-ast")
                || namespace.starts_with("fp-core-span")
            {
                return None;
            }
            Some((namespace, name))
        }
        Locator::ParameterPath(path) => {
            if path.segments.is_empty() {
                return None;
            }
            let mut segments: Vec<String> = path
                .segments
                .iter()
                .map(|seg| sanitize_identifier(seg.ident.as_str()))
                .collect();
            if segments.len() < 2 {
                return None;
            }
            let name = segments.pop()?;
            let namespace = segments.join("-");
            if namespace.starts_with("fp-core-error")
                || namespace.starts_with("fp-core-ast")
                || namespace.starts_with("fp-core-span")
            {
                return None;
            }
            Some((namespace, name))
        }
        _ => None,
    }
}

#[derive(Debug)]
enum InterfaceEntry {
    Record {
        name: String,
        fields: Vec<(String, String)>,
        docs: Vec<String>,
    },
    Variant {
        name: String,
        cases: Vec<(String, Option<String>)>,
        docs: Vec<String>,
    },
    Alias {
        name: String,
        target: String,
        docs: Vec<String>,
    },
    Function {
        name: String,
        params: Vec<(String, String)>,
        result: Option<String>,
        docs: Vec<String>,
    },
}

fn interface_name(parent: Option<&str>, ident: &Ident) -> String {
    let mut base = ident_to_wit(ident);
    if let Some(parent) = parent {
        base = sanitize_identifier(&format!("{parent}-{base}"));
    }
    base
}

fn ident_to_wit(ident: &Ident) -> String {
    sanitize_identifier(ident.as_str())
}

const WIT_KEYWORDS: &[&str] = &[
    "as",
    "async",
    "bool",
    "case",
    "component",
    "const",
    "consume",
    "enum",
    "export",
    "extern",
    "f32",
    "f64",
    "flags",
    "func",
    "future",
    "handle",
    "import",
    "include",
    "interface",
    "list",
    "option",
    "own",
    "package",
    "record",
    "repeat",
    "resource",
    "result",
    "s8",
    "s16",
    "s32",
    "s64",
    "stream",
    "string",
    "tuple",
    "type",
    "u8",
    "u16",
    "u32",
    "u64",
    "union",
    "use",
    "variant",
    "world",
];

fn sanitize_identifier(raw: &str) -> String {
    let mut result = String::new();
    let mut prev_is_alnum = false;
    for ch in raw.chars() {
        match ch {
            'a'..='z' | '0'..='9' => {
                result.push(ch);
                prev_is_alnum = true;
            }
            'A'..='Z' => {
                if prev_is_alnum && !result.ends_with('-') {
                    result.push('-');
                }
                result.push(ch.to_ascii_lowercase());
                prev_is_alnum = true;
            }
            '_' | '-' => {
                if !result.ends_with('-') {
                    result.push('-');
                }
                prev_is_alnum = false;
            }
            _ => {
                if !result.ends_with('-') {
                    result.push('-');
                }
                prev_is_alnum = false;
            }
        }
    }
    while result.contains("--") {
        result = result.replace("--", "-");
    }
    result = result.trim_matches('-').to_string();
    if result.is_empty() {
        result.push_str("item");
    }
    if result.chars().next().unwrap().is_ascii_digit() {
        result.insert(0, '_');
    }
    if WIT_KEYWORDS.contains(&result.as_str()) {
        result.push_str("-value");
    }
    result
}

fn sanitize_type_identifier(raw: &str) -> String {
    let ident = sanitize_identifier(raw);
    match ident.as_str() {
        "bool-value" if raw.eq_ignore_ascii_case("bool") => "bool".to_string(),
        "char-value" if raw.eq_ignore_ascii_case("char") => "char".to_string(),
        "string-value" if raw.eq_ignore_ascii_case("string") => "string".to_string(),
        _ if raw.eq_ignore_ascii_case("str") => "string".to_string(),
        "option-value" if raw.eq_ignore_ascii_case("option") => "option".to_string(),
        "result-value" if raw.eq_ignore_ascii_case("result") => "result".to_string(),
        "list-value" if raw.eq_ignore_ascii_case("list") => "list".to_string(),
        "tuple-value" if raw.eq_ignore_ascii_case("tuple") => "tuple".to_string(),
        other => other.to_string(),
    }
}

fn impl_interface_name(parent: Option<&str>, impl_block: &ItemImpl) -> Option<String> {
    if impl_block.trait_ty.is_some() {
        return None;
    }

    let target = match impl_block.self_ty.kind() {
        ExprKind::Locator(locator) => locator_to_ident(locator).cloned(),
        _ => None,
    }?;

    Some(interface_name(parent, &target))
}

fn locator_to_ident(locator: &Locator) -> Option<&Ident> {
    match locator {
        Locator::Ident(ident) => Some(ident),
        Locator::Path(path) => path.segments.last(),
        _ => None,
    }
}

fn locator_to_ident_from_expr(expr: &Expr) -> Option<&Ident> {
    match expr.kind() {
        ExprKind::Locator(locator) => locator_to_ident(locator),
        _ => None,
    }
}

fn ty_to_wit_with_self(ty: &Ty, self_name: Option<&str>) -> String {
    match ty {
        Ty::Primitive(p) => match p {
            TypePrimitive::Bool => "bool".to_string(),
            TypePrimitive::Char => "char".to_string(),
            TypePrimitive::String => "string".to_string(),
            TypePrimitive::Int(int_ty) => match int_ty {
                TypeInt::I8 => "s8".to_string(),
                TypeInt::I16 => "s16".to_string(),
                TypeInt::I32 => "s32".to_string(),
                TypeInt::I64 => "s64".to_string(),
                TypeInt::U8 => "u8".to_string(),
                TypeInt::U16 => "u16".to_string(),
                TypeInt::U32 => "u32".to_string(),
                TypeInt::U64 => "u64".to_string(),
                TypeInt::BigInt => "s64".to_string(),
            },
            TypePrimitive::Decimal(dec) => match dec {
                ast::DecimalType::F32 => "f32".to_string(),
                ast::DecimalType::F64 => "f64".to_string(),
                _ => "f64".to_string(),
            },
            TypePrimitive::List => "list<string>".to_string(),
        },
        Ty::Reference(reference) => ty_to_wit_with_self(&reference.ty, self_name),
        Ty::Tuple(tuple) => {
            let inner = tuple
                .types
                .iter()
                .map(|ty| ty_to_wit_with_self(ty, self_name))
                .collect::<Vec<_>>()
                .join(", ");
            format!("tuple<{inner}>")
        }
        Ty::Vec(vec_ty) => {
            let inner = ty_to_wit_with_self(vec_ty.ty.as_ref(), self_name);
            format!("list<{inner}>")
        }
        Ty::QuoteToken(_) => "json".to_string(),
        Ty::Array(array) => {
            let inner = ty_to_wit_with_self(array.elem.as_ref(), self_name);
            format!("list<{inner}>")
        }
        Ty::Slice(slice) => {
            let inner = ty_to_wit_with_self(slice.elem.as_ref(), self_name);
            format!("list<{inner}>")
        }
        Ty::Struct(s) => normalize_type_name(sanitize_type_identifier(s.name.as_str())),
        Ty::Enum(e) => normalize_type_name(sanitize_type_identifier(e.name.as_str())),
        Ty::Unit(_) => "tuple<>".to_string(),
        Ty::Any(_) | Ty::TypeBinaryOp(_) | Ty::AnyBox(_) => "json".to_string(),
        Ty::Expr(expr) => expr_to_wit_type(expr, self_name).unwrap_or_else(|| "json".to_string()),
        Ty::Value(_) => "json".to_string(),
        Ty::Unknown(_) | Ty::Nothing(_) => "json".to_string(),
        Ty::Type(_) | Ty::TypeBounds(_) | Ty::ImplTraits(_) => "json".to_string(),
        Ty::Structural(_) => "json".to_string(),
        Ty::Function(_) => "func".to_string(),
    }
}

fn expr_to_wit_type(expr: &Expr, self_name: Option<&str>) -> Option<String> {
    match expr.kind() {
        ExprKind::Locator(locator) => locator_to_wit_type(locator, self_name),
        ExprKind::Value(value) => match value.as_ref() {
            Value::Type(ty) => Some(ty_to_wit_with_self(ty, self_name)),
            _ => None,
        },
        ExprKind::Invoke(invoke) => {
            let base = match &invoke.target {
                ExprInvokeTarget::Function(locator) => locator_to_wit_type(locator, self_name),
                ExprInvokeTarget::Type(ty) => Some(ty_to_wit_with_self(ty, self_name)),
                ExprInvokeTarget::Expr(inner) => expr_to_wit_type(inner, self_name),
                _ => None,
            }?;

            let mut args = Vec::new();
            for arg in &invoke.args {
                if let Some(arg_ty) = expr_to_wit_type(arg, self_name) {
                    args.push(arg_ty);
                } else if let Some(ty) = arg.ty() {
                    args.push(ty_to_wit_with_self(ty, self_name));
                }
            }

            if args.is_empty() {
                Some(base)
            } else {
                Some(format!("{base}<{}>", args.join(", ")))
            }
        }
        _ => None,
    }
}

fn locator_to_wit_type(locator: &Locator, self_name: Option<&str>) -> Option<String> {
    match locator {
        Locator::Ident(ident) if ident.as_str() == "Self" => {
            self_name.map(|name| sanitize_identifier(name))
        }
        Locator::Ident(ident) => {
            if matches!(ident.as_str(), "Error" | "Span") {
                return Some("string".to_string());
            }
            Some(normalize_type_name(sanitize_type_identifier(
                ident.as_str(),
            )))
        }
        Locator::Path(path) => {
            if let Some(first) = path.segments.first() {
                if matches!(first.as_str(), "std" | "core" | "alloc") {
                    if path
                        .segments
                        .last()
                        .map(|ident| ident.as_str())
                        .map(|name| name == "Error")
                        .unwrap_or(false)
                    {
                        return Some("string".to_string());
                    }
                }
            }
            let segments: Vec<String> = path
                .segments
                .iter()
                .map(|seg| sanitize_identifier(seg.as_str()))
                .collect();
            if matches!(segments.first().map(String::as_str), Some("clap-complete")) {
                return Some("string".to_string());
            }
            if segments.len() >= 2 && segments[0] == "fp-core" {
                if matches!(segments[1].as_str(), "error" | "ast" | "span") {
                    return Some("string".to_string());
                }
            }
            path.segments
                .last()
                .map(|ident| normalize_type_name(sanitize_type_identifier(ident.as_str())))
        }
        _ => None,
    }
}

fn normalize_type_name(name: String) -> String {
    match name.as_str() {
        "pathbuf" | "path" | "path-buf" => "string".to_string(),
        _ => name,
    }
}
