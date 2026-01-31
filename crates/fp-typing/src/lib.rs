use fp_core::config;
use std::collections::{HashMap, HashSet};

pub mod typing;
pub use typing::types::{TypingDiagnostic, TypingDiagnosticLevel, TypingOutcome};

use crate::typing::scheme::TypeScheme;
use fp_core::ast::*;
use fp_core::ast::{AttributesExt, Ident, Locator};
use fp_core::context::SharedScopedContext;
use fp_core::diagnostics::Diagnostic;
use fp_core::error::{Error, Result};
use fp_core::module::path::{parse_segments, resolve_item_path, segments_to_key};
use fp_core::span::Span;
// intrinsic and op kinds handled in submodules
fn typing_error(msg: impl Into<String>) -> Error {
    Error::from(msg.into())
}

pub(crate) type TypeVarId = usize;

fn attrs_has_name(attrs: &[Attribute], name: &str) -> bool {
    attrs.iter().any(|attr| match &attr.meta {
        AttrMeta::Path(path) => path.last().as_str() == name,
        AttrMeta::NameValue(nv) => nv.name.last().as_str() == name,
        AttrMeta::List(list) => list.name.last().as_str() == name,
    })
}

fn detect_lossy_mode() -> bool {
    config::lossy_mode()
}

fn default_extern_prelude() -> HashSet<String> {
    ["std", "core", "alloc"]
        .into_iter()
        .map(|name| name.to_string())
        .collect()
}

pub trait TypeResolutionHook {
    fn resolve_symbol(&mut self, name: &str) -> bool;
}

use crate::typing::unify::{FunctionTerm, TypeTerm, TypeVar, TypeVarKind};

// TypeScheme moved to typing/scheme.rs

#[derive(Clone, Debug)]
struct MethodRecord {
    receiver_ty: Option<Ty>,
    scheme: Option<TypeScheme>,
}

#[derive(Clone, Debug)]
struct ImplContext {
    struct_name: String,
    self_ty: Ty,
}

// SchemeType moved to typing/scheme.rs

#[derive(Clone)]
enum EnvEntry {
    Mono(TypeVarId),
    Poly(TypeScheme),
}

struct PatternBinding {
    name: String,
    var: TypeVarId,
}

struct PatternInfo {
    var: TypeVarId,
    bindings: Vec<PatternBinding>,
}

impl PatternInfo {
    fn new(var: TypeVarId) -> Self {
        Self {
            var,
            bindings: Vec::new(),
        }
    }

    fn with_binding(mut self, name: String, var: TypeVarId) -> Self {
        self.bindings.push(PatternBinding { name, var });
        self
    }

    #[allow(dead_code)]
    fn extend_bindings(&mut self, other: PatternInfo) {
        self.bindings.extend(other.bindings);
    }
}

// Typing diagnostics/outcome are defined in typing/types.rs and re-exported above.

struct FunctionTypeInfo {
    params: Vec<TypeVarId>,
    ret: TypeVarId,
}

struct LoopContext {
    result_var: TypeVarId,
    saw_break: bool,
}

impl LoopContext {
    fn new(result_var: TypeVarId) -> Self {
        Self {
            result_var,
            saw_break: false,
        }
    }
}

pub struct AstTypeInferencer<'ctx> {
    ctx: Option<&'ctx SharedScopedContext>,
    type_vars: Vec<TypeVar>,
    env: Vec<HashMap<String, EnvEntry>>,
    generic_scopes: Vec<HashSet<String>>,
    struct_defs: HashMap<String, TypeStruct>,
    enum_defs: HashMap<String, TypeEnum>,
    enum_variants: HashMap<String, Vec<String>>,
    struct_methods: HashMap<String, HashMap<String, MethodRecord>>,
    trait_method_sigs: HashMap<String, HashMap<String, FunctionSignature>>,
    function_signatures: HashMap<String, FunctionSignature>,
    extern_function_signatures: HashMap<String, FunctionSignature>,
    impl_traits: HashMap<String, HashSet<String>>,
    generic_trait_bounds: HashMap<TypeVarId, Vec<String>>,
    impl_stack: Vec<Option<ImplContext>>,
    module_path: Vec<String>,
    module_defs: HashSet<Vec<String>>,
    module_scope_depths: Vec<usize>,
    root_modules: HashSet<String>,
    extern_prelude: HashSet<String>,
    module_aliases: Vec<HashMap<String, Vec<String>>>,
    symbol_aliases: Vec<HashMap<String, String>>,
    unimplemented_symbols: HashSet<String>,
    current_level: usize,
    diagnostics: Vec<TypingDiagnostic>,
    has_errors: bool,
    literal_ints: HashSet<TypeVarId>,
    loop_stack: Vec<LoopContext>,
    lossy_mode: bool,
    hashmap_args: HashMap<TypeVarId, (TypeVarId, TypeVarId)>,
    current_span: Option<Span>,
    resolution_hook: Option<Box<dyn TypeResolutionHook + 'ctx>>,
}

impl<'ctx> AstTypeInferencer<'ctx> {
    fn register_qualified_symbol(&mut self, name: String) -> TypeVarId {
        if let Some(var) = self.lookup_env_var(&name) {
            return var;
        }
        let var = self.fresh_type_var();
        self.insert_env(name, EnvEntry::Mono(var));
        var
    }

    fn register_qualified_items(&mut self, items: &[Item], prefix: &str) {
        for item in items {
            match item.kind() {
                ItemKind::Module(module) => {
                    let next = format!("{}::{}", prefix, module.name.as_str());
                    self.register_qualified_items(&module.items, &next);
                }
                ItemKind::DefFunction(def) => {
                    let name = format!("{}::{}", prefix, def.name.as_str());
                    let var = self.register_qualified_symbol(name);
                    self.prebind_function_signature(def, var);
                }
                ItemKind::DeclFunction(decl) => {
                    let name = format!("{}::{}", prefix, decl.name.as_str());
                    let var = self.register_qualified_symbol(name);
                    self.prebind_decl_function_signature(decl, var);
                }
                ItemKind::DefConst(def) => {
                    let name = format!("{}::{}", prefix, def.name.as_str());
                    self.register_qualified_symbol(name);
                }
                ItemKind::DefStatic(def) => {
                    let name = format!("{}::{}", prefix, def.name.as_str());
                    self.register_qualified_symbol(name);
                }
                _ => {}
            }
        }
    }

    pub fn new() -> Self {
        Self {
            ctx: None,
            type_vars: Vec::new(),
            env: vec![HashMap::new()],
            generic_scopes: vec![HashSet::new()],
            struct_defs: HashMap::new(),
            enum_defs: HashMap::new(),
            enum_variants: HashMap::new(),
            struct_methods: HashMap::new(),
            trait_method_sigs: HashMap::new(),
            function_signatures: HashMap::new(),
            extern_function_signatures: HashMap::new(),
            impl_traits: HashMap::new(),
            generic_trait_bounds: HashMap::new(),
            impl_stack: Vec::new(),
            module_path: Vec::new(),
            module_defs: HashSet::new(),
            module_scope_depths: vec![0],
            root_modules: HashSet::new(),
            extern_prelude: default_extern_prelude(),
            module_aliases: vec![HashMap::new()],
            symbol_aliases: vec![HashMap::new()],
            current_level: 0,
            diagnostics: Vec::new(),
            has_errors: false,
            literal_ints: HashSet::new(),
            loop_stack: Vec::new(),
            lossy_mode: detect_lossy_mode(),
            hashmap_args: HashMap::new(),
            current_span: None,
            resolution_hook: None,
            unimplemented_symbols: HashSet::new(),
        }
    }

    pub fn with_context(mut self, ctx: &'ctx SharedScopedContext) -> Self {
        self.ctx = Some(ctx);
        self
    }

    pub fn with_extern_prelude<I, S>(mut self, names: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.set_extern_prelude(names);
        self
    }

    pub fn set_extern_prelude<I, S>(&mut self, names: I)
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.extern_prelude.clear();
        for name in names {
            self.extern_prelude.insert(name.into());
        }
    }

    pub fn set_resolution_hook(&mut self, hook: Box<dyn TypeResolutionHook + 'ctx>) {
        self.resolution_hook = Some(hook);
    }

    fn record_hashmap_args(
        &mut self,
        map_var: TypeVarId,
        key_var: TypeVarId,
        value_var: TypeVarId,
    ) {
        self.hashmap_args.insert(map_var, (key_var, value_var));
    }

    fn lookup_hashmap_args(&mut self, map_var: TypeVarId) -> Option<(TypeVarId, TypeVarId)> {
        let mut current = map_var;
        loop {
            if let Some(args) = self.hashmap_args.get(&current).copied() {
                return Some(args);
            }
            match self.type_vars.get(current).map(|var| var.kind.clone()) {
                Some(TypeVarKind::Link(next)) => current = next,
                Some(TypeVarKind::Bound(TypeTerm::Reference(inner))) => current = inner,
                _ => return None,
            }
        }
    }

    pub fn infer(&mut self, node: &mut Node) -> Result<TypingOutcome> {
        match node.kind_mut() {
            NodeKind::Expr(expr) => {
                let var = match self.infer_expr(expr) {
                    Ok(var) => var,
                    Err(err) => {
                        return Err(self.error_with_span(err, self.span_option(expr.span())))
                    }
                };
                let ty = self.resolve_to_ty(var)?;
                node.set_ty(ty);
            }
            NodeKind::Item(item) => {
                self.predeclare_item(item);
                if let Err(err) = self.infer_item(item) {
                    return Err(self.error_with_span(err, self.span_option(item.span())));
                }
                let ty = item.ty().cloned().unwrap_or_else(|| Ty::Unit(TypeUnit));
                node.set_ty(ty);
            }
            NodeKind::File(file) => {
                for item in &file.items {
                    self.predeclare_item(item);
                }
                for item in &mut file.items {
                    if let Err(err) = self.infer_item(item) {
                        return Err(self.error_with_span(err, self.span_option(item.span())));
                    }
                }
                node.set_ty(Ty::Unit(TypeUnit));
            }
            NodeKind::Query(_) => {
                node.set_ty(Ty::any());
            }
            NodeKind::Schema(_) => {
                node.set_ty(Ty::any());
            }
            NodeKind::Workspace(_) => {
                node.set_ty(Ty::any());
            }
        }
        Ok(self.finish())
    }

    /// Initialize the typer with declarations from a node without doing full inference.
    /// This is useful for preparing the typer for incremental type inference.
    pub fn initialize_from_node(&mut self, node: &Node) {
        match node.kind() {
            NodeKind::File(file) => {
                for item in &file.items {
                    self.predeclare_item(item);
                }
            }
            NodeKind::Item(item) => {
                self.predeclare_item(item);
            }
            NodeKind::Expr(_) => {
                // Nothing to predeclare for expressions
            }
            NodeKind::Query(_) | NodeKind::Schema(_) => {
                // Non-AST documents do not participate in type inference yet.
            }
            NodeKind::Workspace(_) => {}
        }
    }

    /// Initialize import aliases without running full inference.
    pub fn initialize_imports_from_node(&mut self, node: &Node) {
        match node.kind() {
            NodeKind::File(file) => {
                self.register_import_aliases_for_items(&file.items);
            }
            NodeKind::Item(item) => {
                self.register_import_aliases_for_item(item);
            }
            NodeKind::Expr(_) | NodeKind::Query(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {
            }
        }
    }

    fn register_import_aliases_for_items(&mut self, items: &[Item]) {
        for item in items {
            self.register_import_aliases_for_item(item);
        }
    }

    fn register_import_aliases_for_item(&mut self, item: &Item) {
        match item.kind() {
            ItemKind::Import(import) => self.register_import_aliases(import),
            ItemKind::Module(module) => self.register_import_aliases_for_items(&module.items),
            ItemKind::Impl(impl_block) => {
                self.register_import_aliases_for_items(&impl_block.items);
            }
            ItemKind::DefTrait(def) => {
                self.register_import_aliases_for_items(&def.items);
            }
            _ => {}
        }
    }

    /// Initialize the typer with a single item for incremental typing.
    pub fn initialize_from_item(&mut self, item: &Item) {
        self.predeclare_item(item);
    }

    fn finish(&mut self) -> TypingOutcome {
        TypingOutcome {
            diagnostics: std::mem::take(&mut self.diagnostics),
            has_errors: std::mem::replace(&mut self.has_errors, false),
        }
    }

    fn validate_struct_recursion(&mut self, name: &str, fields: &[StructuralField]) {
        let mut visiting = HashSet::new();
        for field in fields {
            let mut path = vec![field.name.as_str().to_string()];
            if let Some((path_str, chain)) = self.contains_illegal_struct_recursion(
                &field.value,
                name,
                false,
                &mut visiting,
                &mut path,
            ) {
                let location = if path_str.is_empty() {
                    "field".to_string()
                } else {
                    format!("field {}", path_str)
                };
                let chain = if chain.is_empty() {
                    String::new()
                } else {
                    let mut cycle = chain.clone();
                    if let Some(first) = chain.first() {
                        cycle.push(first.clone());
                    }
                    format!(" (cycle: {})", cycle.join(" -> "))
                };
                self.emit_error_with_span(
                    self.span_option(field.span()),
                    format!(
                        "recursive struct {} {} must use heap indirection (Box/Arc/Rc/Weak/Vec/&/Box<dyn ...>){}",
                        name, location, chain
                    ),
                );
            }
        }
    }

    fn contains_illegal_struct_recursion(
        &self,
        ty: &Ty,
        target: &str,
        heap_wrapped: bool,
        visiting: &mut HashSet<String>,
        path: &mut Vec<String>,
    ) -> Option<(String, Vec<String>)> {
        if heap_wrapped {
            return None;
        }

        if let Some(inner) = self.heap_inner_ty(ty) {
            return self.contains_illegal_struct_recursion(inner, target, true, visiting, path);
        }

        match ty {
            Ty::Struct(struct_ty) => {
                let name = struct_ty.name.as_str();
                if name == target {
                    return Some((path.join("."), vec![target.to_string()]));
                }
                if !visiting.insert(name.to_string()) {
                    return None;
                }
                let result = struct_ty.fields.iter().find_map(|field| {
                    path.push(field.name.as_str().to_string());
                    let found = self.contains_illegal_struct_recursion(
                        &field.value,
                        target,
                        false,
                        visiting,
                        path,
                    );
                    path.pop();
                    found
                });
                visiting.remove(name);
                result
            }
            Ty::Structural(structural) => structural.fields.iter().find_map(|field| {
                path.push(field.name.as_str().to_string());
                let found = self.contains_illegal_struct_recursion(
                    &field.value,
                    target,
                    false,
                    visiting,
                    path,
                );
                path.pop();
                found
            }),
            Ty::Tuple(tuple) => tuple.types.iter().find_map(|elem| {
                path.push("tuple".to_string());
                let found =
                    self.contains_illegal_struct_recursion(elem, target, false, visiting, path);
                path.pop();
                found
            }),
            Ty::Vec(vec) => self.contains_illegal_struct_recursion(
                &vec.ty,
                target,
                false,
                visiting,
                path,
            ),
            Ty::Array(array) => self.contains_illegal_struct_recursion(
                &array.elem,
                target,
                false,
                visiting,
                path,
            ),
            Ty::Slice(slice) => self.contains_illegal_struct_recursion(
                &slice.elem,
                target,
                false,
                visiting,
                path,
            ),
            Ty::Reference(reference) => self.contains_illegal_struct_recursion(
                &reference.ty,
                target,
                false,
                visiting,
                path,
            ),
            Ty::Function(function) => {
                for param in &function.params {
                    if let Some(found) =
                        self.contains_illegal_struct_recursion(param, target, false, visiting, path)
                    {
                        return Some(found);
                    }
                }
                function
                    .ret_ty
                    .as_ref()
                    .and_then(|ret| {
                        self.contains_illegal_struct_recursion(ret, target, false, visiting, path)
                    })
            }
            Ty::TypeBinaryOp(op) => self
                .contains_illegal_struct_recursion(&op.lhs, target, false, visiting, path)
                .or_else(|| {
                    self.contains_illegal_struct_recursion(&op.rhs, target, false, visiting, path)
                }),
            Ty::Expr(expr) => {
                let ExprKind::Locator(locator) = expr.kind() else {
                    return None;
                };
                if let Some(inner) = self.heap_inner_ty(ty) {
                    return self.contains_illegal_struct_recursion(
                        inner,
                        target,
                        true,
                        visiting,
                        path,
                    );
                }
                let Some(name) = self.locator_tail_name(locator) else {
                    return None;
                };
                if name == target {
                    return Some((path.join("."), vec![target.to_string()]));
                }
                let Some(def) = self.struct_defs.get(&name) else {
                    return None;
                };
                if !visiting.insert(name.clone()) {
                    return None;
                }
                let result = def.fields.iter().find_map(|field| {
                    path.push(field.name.as_str().to_string());
                    let found = self.contains_illegal_struct_recursion(
                        &field.value,
                        target,
                        false,
                        visiting,
                        path,
                    );
                    path.pop();
                    found.map(|(path_str, mut chain)| {
                        chain.insert(0, name.clone());
                        (path_str, chain)
                    })
                });
                visiting.remove(&name);
                result
            }
            _ => None,
        }
    }

    fn heap_inner_ty<'a>(&self, ty: &'a Ty) -> Option<&'a Ty> {
        match ty {
            Ty::Reference(reference) => Some(&reference.ty),
            Ty::Vec(vec) => Some(&vec.ty),
            Ty::Expr(expr) => {
                let ExprKind::Locator(Locator::ParameterPath(path)) = expr.kind() else {
                    return None;
                };
                let segment = path.segments.last()?;
                if segment.args.len() != 1 {
                    return None;
                }
                match segment.ident.as_str() {
                    "Box" | "Arc" | "Rc" | "Weak" | "Vec" => Some(&segment.args[0]),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn locator_tail_name(&self, locator: &Locator) -> Option<String> {
        match locator {
            Locator::Ident(ident) => Some(ident.as_str().to_string()),
            Locator::Path(path) => path.segments.last().map(|seg| seg.as_str().to_string()),
            Locator::ParameterPath(path) => path
                .segments
                .last()
                .map(|seg| seg.ident.as_str().to_string()),
        }
    }

    fn struct_name_variants(&self, name: &str) -> Vec<String> {
        let mut names = Vec::new();
        let mut seen = HashSet::new();
        let push = |value: String, names: &mut Vec<String>, seen: &mut HashSet<String>| {
            if seen.insert(value.clone()) {
                names.push(value);
            }
        };

        if !self.module_path.is_empty() && !name.contains("::") {
            let mut qualified = self.module_path.clone();
            qualified.push(name.to_string());
            push(qualified.join("::"), &mut names, &mut seen);
        }
        push(name.to_string(), &mut names, &mut seen);

        let short = name.rsplit("::").next().unwrap_or(name);
        if short != name {
            push(short.to_string(), &mut names, &mut seen);
        }

        names
    }

    fn lookup_struct_def_by_name(&mut self, name: &str) -> Option<(String, TypeStruct)> {
        if name == "TypeBuilder" && std::env::var("FP_DEBUG_TYPEBUILDER").is_ok() {
            let keys = self
                .struct_defs
                .keys()
                .filter(|key| key.ends_with("TypeBuilder"))
                .cloned()
                .collect::<Vec<_>>();
            eprintln!(
                "debug TypeBuilder: module_path={:?} keys={:?}",
                self.module_path, keys
            );
        }
        if let Some(def) = self.struct_defs.get(name).cloned() {
            return Some((name.to_string(), def));
        }
        if !self.module_path.is_empty() && !name.contains("::") {
            let mut qualified = self.module_path.clone();
            qualified.push(name.to_string());
            let key = segments_to_key(&qualified);
            if let Some(def) = self.struct_defs.get(&key).cloned() {
                return Some((key, def));
            }
        }
        if name.contains("::") {
            return None;
        }
        let suffix = format!("::{}", name);
        let mut match_key = None;
        for key in self.struct_defs.keys() {
            if key.ends_with(&suffix) {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        if let Some(key) = match_key {
            return self
                .struct_defs
                .get(&key)
                .cloned()
                .map(|def| (key, def));
        }
        let mut match_key = None;
        for (key, def) in &self.struct_defs {
            if def.name.as_str() == name {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        if let Some(key) = match_key {
            return self.struct_defs.get(&key).cloned().map(|def| (key, def));
        }
        for candidate in self.struct_name_variants(name) {
            if let Some(var) = self.lookup_env_var(candidate.as_str()) {
                if let Ok(ty) = self.resolve_to_ty(var) {
                    if let Ty::Struct(def) = ty {
                        return Some((candidate, def));
                    }
                }
            }
        }
        None
    }

    fn lookup_enum_def_by_name(&self, name: &str) -> Option<(String, TypeEnum)> {
        if let Some(def) = self.enum_defs.get(name).cloned() {
            return Some((name.to_string(), def));
        }
        if !self.module_path.is_empty() && !name.contains("::") {
            let mut qualified = self.module_path.clone();
            qualified.push(name.to_string());
            let key = segments_to_key(&qualified);
            if let Some(def) = self.enum_defs.get(&key).cloned() {
                return Some((key, def));
            }
        }
        if name.contains("::") {
            return None;
        }
        let suffix = format!("::{}", name);
        let mut match_key = None;
        for key in self.enum_defs.keys() {
            if key.ends_with(&suffix) {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        if let Some(key) = match_key {
            return self.enum_defs.get(&key).cloned().map(|def| (key, def));
        }
        let mut match_key = None;
        for (key, def) in &self.enum_defs {
            if def.name.as_str() == name {
                if match_key.is_some() {
                    return None;
                }
                match_key = Some(key.clone());
            }
        }
        match_key.and_then(|key| self.enum_defs.get(&key).cloned().map(|def| (key, def)))
    }

    fn record_function_signature(&mut self, name: &Ident, sig: &FunctionSignature) {
        let mut candidates = Vec::new();
        if self.module_path.is_empty() {
            candidates.push(name.as_str().to_string());
        } else {
            let mut qualified = self.module_path.clone();
            qualified.push(name.as_str().to_string());
            candidates.push(qualified.join("::"));
        }
        for candidate in candidates {
            self.function_signatures
                .insert(candidate, sig.clone());
        }
    }

    fn record_extern_function_signature(&mut self, name: &Ident, sig: &FunctionSignature) {
        let mut candidates = Vec::new();
        if self.module_path.is_empty() {
            candidates.push(name.as_str().to_string());
        } else {
            let mut qualified = self.module_path.clone();
            qualified.push(name.as_str().to_string());
            candidates.push(qualified.join("::"));
        }
        for candidate in candidates {
            self.extern_function_signatures
                .insert(candidate, sig.clone());
        }
    }

    fn record_unimplemented_symbol(&mut self, name: &Ident, attrs: &[Attribute]) {
        if !attrs_has_name(attrs, "unimplemented") {
            return;
        }
        let mut candidates = Vec::new();
        if self.module_path.is_empty() {
            candidates.push(name.as_str().to_string());
        } else {
            let mut qualified = self.module_path.clone();
            qualified.push(name.as_str().to_string());
            candidates.push(qualified.join("::"));
        }
        for candidate in candidates {
            self.unimplemented_symbols.insert(candidate);
        }
    }

    fn is_unimplemented_name(&self, name: &str) -> bool {
        self.unimplemented_symbols.contains(name)
    }

    fn resolution_segments(&self, locator: &Locator) -> Vec<String> {
        match locator {
            Locator::Ident(ident) => vec![ident.as_str().to_string()],
            Locator::Path(path) => path
                .segments
                .iter()
                .map(|seg| seg.as_str().to_string())
                .collect(),
            Locator::ParameterPath(path) => path
                .segments
                .iter()
                .map(|seg| seg.ident.as_str().to_string())
                .collect(),
        }
    }

    fn env_contains(&self, key: &str) -> bool {
        self.env.iter().rev().any(|scope| scope.contains_key(key))
    }

    fn scope_contains_non_module(&self, name: &str) -> bool {
        let module_depth = *self.module_scope_depths.last().unwrap_or(&0);
        self.env
            .iter()
            .enumerate()
            .rev()
            .any(|(idx, scope)| idx > module_depth && scope.contains_key(name))
    }

    fn item_exists_key(&self, key: &str) -> bool {
        self.struct_defs.contains_key(key)
            || self.enum_defs.contains_key(key)
            || self.function_signatures.contains_key(key)
            || self.extern_function_signatures.contains_key(key)
            || self.unimplemented_symbols.contains(key)
            || self.env_contains(key)
    }

    fn item_exists_segments(&self, segments: &[String]) -> bool {
        let key = segments_to_key(segments);
        self.item_exists_key(&key)
    }

    fn resolve_locator_key(&self, locator: &Locator) -> Option<String> {
        if let Some(qualified) = self.resolve_alias_locator(locator) {
            return Some(qualified);
        }
        let segments = self.resolution_segments(locator);
        if segments.is_empty() {
            return None;
        }
        if segments.len() == 1 {
            let parsed = parse_segments(&segments).ok()?;
            let qualified = resolve_item_path(
                &parsed,
                &self.module_path,
                &self.root_modules,
                &self.extern_prelude,
                &self.module_defs,
                |candidate| self.item_exists_segments(candidate),
                |name| self.scope_contains_non_module(name),
            )?;
            return Some(segments_to_key(&qualified));
        }
        let parsed = parse_segments(&segments).ok()?;
        let qualified = resolve_item_path(
            &parsed,
            &self.module_path,
            &self.root_modules,
            &self.extern_prelude,
            &self.module_defs,
            |candidate| self.item_exists_segments(candidate),
            |name| self.scope_contains_non_module(name),
        )?;
        Some(segments_to_key(&qualified))
    }

    fn resolve_segments_key(&self, segments: &[String]) -> Option<String> {
        if segments.is_empty() {
            return None;
        }
        if segments.len() == 1 {
            let parsed = parse_segments(segments).ok()?;
            let qualified = resolve_item_path(
                &parsed,
                &self.module_path,
                &self.root_modules,
                &self.extern_prelude,
                &self.module_defs,
                |candidate| self.item_exists_segments(candidate),
                |name| self.scope_contains_non_module(name),
            )?;
            return Some(segments_to_key(&qualified));
        }
        let parsed = parse_segments(segments).ok()?;
        let qualified = resolve_item_path(
            &parsed,
            &self.module_path,
            &self.root_modules,
            &self.extern_prelude,
            &self.module_defs,
            |candidate| self.item_exists_segments(candidate),
            |name| self.scope_contains_non_module(name),
        )?;
        Some(segments_to_key(&qualified))
    }

    fn check_unimplemented_locator(&mut self, locator: &Locator) -> bool {
        if let Some(ident) = locator.as_ident() {
            if !self.module_path.is_empty() {
                let mut qualified = self.module_path.clone();
                qualified.push(ident.as_str().to_string());
                let candidate = qualified.join("::");
                if self.is_unimplemented_name(&candidate) {
                    self.emit_warning(format!("use of unimplemented item: {}", candidate));
                    return false;
                }
            }
        }
        let Some(candidate) = self.resolve_locator_key(locator) else {
            return false;
        };
        if self.is_unimplemented_name(&candidate) {
            self.emit_warning(format!("use of unimplemented item: {}", candidate));
            return false;
        }
        false
    }

    fn lookup_function_signature(&self, locator: &Locator) -> Option<FunctionSignature> {
        let candidate = self.resolve_locator_key(locator)?;
        if let Some(sig) = self.extern_function_signatures.get(&candidate) {
            return Some(sig.clone());
        }
        self.function_signatures.get(&candidate).cloned()
    }

    fn lookup_extern_function_signature(&self, locator: &Locator) -> Option<FunctionSignature> {
        let candidate = self.resolve_locator_key(locator)?;
        self.extern_function_signatures.get(&candidate).cloned()
    }

    fn resolve_impl_context(&mut self, self_ty: &Expr) -> Option<ImplContext> {
        let resolved_name = match self_ty.kind() {
            ExprKind::Locator(locator) => self.resolve_locator_key(locator),
            _ => None,
        };
        let name = resolved_name
            .or_else(|| self.struct_name_from_expr(self_ty))
            .unwrap_or_default();

        if name.is_empty() {
            self.emit_error("impl self type must resolve to a struct or enum");
            return None;
        }

        if let Some((resolved, def)) = self.lookup_struct_def_by_name(&name) {
            return Some(ImplContext {
                struct_name: resolved,
                self_ty: Ty::Struct(def),
            });
        }
        if let Some((resolved, def)) = self.lookup_enum_def_by_name(&name) {
            return Some(ImplContext {
                struct_name: resolved,
                self_ty: Ty::Enum(def),
            });
        }
        if let Some(ty) = self.resolve_impl_self_from_env(&name) {
            return Some(ty);
        }

        for candidate in self.struct_name_variants(&name) {
            if let Some(def) = self.struct_defs.get(&candidate).cloned() {
                return Some(ImplContext {
                    struct_name: candidate,
                    self_ty: Ty::Struct(def),
                });
            }
        }
        for candidate in self.struct_name_variants(&name) {
            if let Some(def) = self.enum_defs.get(&candidate).cloned() {
                return Some(ImplContext {
                    struct_name: candidate,
                    self_ty: Ty::Enum(def),
                });
            }
        }

        {
            let placeholder = TypeStruct {
                name: Ident::new(name.clone()),
                generics_params: Vec::new(),
                fields: Vec::new(),
            };
            self.emit_warning(format!(
                "impl target {} is not a known struct or enum",
                name
            ));
            Some(ImplContext {
                struct_name: name,
                self_ty: Ty::Struct(placeholder),
            })
        }
    }

    fn resolve_impl_self_from_env(&mut self, name: &str) -> Option<ImplContext> {
        let mut candidates = Vec::new();
        candidates.push(name.to_string());
        if !self.module_path.is_empty() && !name.contains("::") {
            let mut qualified = self.module_path.clone();
            qualified.push(name.to_string());
            candidates.push(segments_to_key(&qualified));
        }
        for candidate in candidates {
            if let Some(var) = self.lookup_env_var(&candidate) {
                if let Ok(ty) = self.resolve_to_ty(var) {
                    match ty {
                        Ty::Struct(def) => {
                            return Some(ImplContext {
                                struct_name: candidate,
                                self_ty: Ty::Struct(def),
                            })
                        }
                        Ty::Enum(def) => {
                            return Some(ImplContext {
                                struct_name: candidate,
                                self_ty: Ty::Enum(def),
                            })
                        }
                        _ => {}
                    }
                }
            }
        }
        None
    }

    fn ty_for_receiver(&self, ctx: &ImplContext, receiver: &FunctionParamReceiver) -> Ty {
        match receiver {
            FunctionParamReceiver::Implicit
            | FunctionParamReceiver::Value
            | FunctionParamReceiver::MutValue => ctx.self_ty.clone(),
            FunctionParamReceiver::Ref | FunctionParamReceiver::RefStatic => Ty::Reference(
                TypeReference {
                    ty: Box::new(ctx.self_ty.clone()),
                    mutability: Some(false),
                    lifetime: None,
                }
                .into(),
            ),
            FunctionParamReceiver::RefMut | FunctionParamReceiver::RefMutStatic => Ty::Reference(
                TypeReference {
                    ty: Box::new(ctx.self_ty.clone()),
                    mutability: Some(true),
                    lifetime: None,
                }
                .into(),
            ),
        }
    }

    fn register_method_stub(&mut self, ctx: &ImplContext, func: &ItemDefFunction) {
        let receiver_ty = func
            .sig
            .receiver
            .as_ref()
            .map(|receiver| self.ty_for_receiver(ctx, receiver));
        for candidate in self.struct_name_variants(&ctx.struct_name) {
            let entry = self.struct_methods.entry(candidate).or_default();
            entry
                .entry(func.name.as_str().to_string())
                .or_insert(MethodRecord {
                    receiver_ty: receiver_ty.clone(),
                    scheme: None,
                });
        }
    }

    fn peel_reference(mut ty: Ty) -> Ty {
        loop {
            match ty {
                Ty::Reference(reference) => {
                    ty = (*reference.ty).clone();
                }
                other => return other,
            }
        }
    }

    fn predeclare_item(&mut self, item: &Item) {
        if std::env::var("FP_DEBUG_TYPEBUILDER").is_ok() {
            match item.kind() {
                ItemKind::DefStruct(def) if def.name.as_str().contains("TypeBuilder") => {
                    eprintln!(
                        "debug TypeBuilder predeclare: DefStruct module_path={:?}",
                        self.module_path
                    );
                }
                ItemKind::DefConst(def) if def.name.as_str().contains("TypeBuilder") => {
                    eprintln!(
                        "debug TypeBuilder predeclare: DefConst module_path={:?}",
                        self.module_path
                    );
                }
                ItemKind::DefType(def) if def.name.as_str().contains("TypeBuilder") => {
                    eprintln!(
                        "debug TypeBuilder predeclare: DefType module_path={:?}",
                        self.module_path
                    );
                }
                ItemKind::DefStructural(def) if def.name.as_str().contains("TypeBuilder") => {
                    eprintln!(
                        "debug TypeBuilder predeclare: DefStructural module_path={:?}",
                        self.module_path
                    );
                }
                _ => {}
            }
        }
        match item.kind() {
            ItemKind::Macro(mac) => {
                self.predeclare_macro_item(mac);
            }
            ItemKind::DefStruct(def) => {
                self.record_unimplemented_symbol(&def.name, &def.attrs);
                self.insert_struct_def(&def.name, def.value.clone());
                let var = self.symbol_var(&def.name);
                let ty = Ty::Struct(def.value.clone());
                if let Ok(struct_var) = self.type_from_ast_ty(&ty) {
                    let _ = self.unify(var, struct_var);
                }
            }
            ItemKind::DefStructural(def) => {
                self.record_unimplemented_symbol(&def.name, &def.attrs);
                let struct_ty = TypeStruct {
                    name: def.name.clone(),
                    generics_params: Vec::new(),
                    fields: def.value.fields.clone(),
                };
                self.insert_struct_def(&def.name, struct_ty);
                self.register_symbol(&def.name);
            }
            ItemKind::DefType(def) => {
                self.record_unimplemented_symbol(&def.name, &def.attrs);
                // Type aliases / type-level expressions introduce a named type.
                // The concrete shape (e.g. structural fields) is resolved during `infer_item`.
                self.register_symbol(&def.name);
            }
            ItemKind::DefEnum(def) => {
                self.record_unimplemented_symbol(&def.name, &def.attrs);
                let enum_name = self
                    .qualified_name(def.name.as_str())
                    .unwrap_or_else(|| def.name.as_str().to_string());
                self.enum_defs.insert(enum_name.clone(), def.value.clone());
                self.register_symbol(&def.name);

                let mut variant_keys = Vec::new();
                for variant in &def.value.variants {
                    let qualified = format!("{}::{}", enum_name, variant.name.as_str());
                    variant_keys.push(qualified.clone());
                    if self.lookup_env_var(&qualified).is_none() {
                        let var = self.fresh_type_var();
                        self.insert_env(qualified.clone(), EnvEntry::Mono(var));
                    }
                }
                self.enum_variants.insert(enum_name, variant_keys);
            }
            ItemKind::DefTrait(def) => {
                self.register_symbol(&def.name);
                let entry = self
                    .trait_method_sigs
                    .entry(def.name.as_str().to_string())
                    .or_default();
                for member in &def.items {
                    match member.kind() {
                        ItemKind::DeclFunction(decl) => {
                            if let Some(name) = decl.sig.name.as_ref() {
                                entry.insert(name.as_str().to_string(), decl.sig.clone());
                            }
                        }
                        ItemKind::DefFunction(func) => {
                            entry.insert(func.name.as_str().to_string(), func.sig.clone());
                        }
                        _ => {}
                    }
                }
            }
            ItemKind::DefConst(def) => {
                self.register_symbol(&def.name);
            }
            ItemKind::DefStatic(def) => {
                self.register_symbol(&def.name);
                if let Ty::Struct(ty) = &def.ty {
                    self.insert_struct_def(&ty.name, ty.clone());
                }
            }
            ItemKind::DefFunction(def) => {
                self.record_unimplemented_symbol(&def.name, &def.attrs);
                let in_impl = self.impl_stack.last().is_some();
                if !in_impl {
                    self.record_function_signature(&def.name, &def.sig);
                }
                let fn_var = if let Some(ctx) = self.impl_stack.last().cloned().flatten() {
                    let key = format!("{}::{}", ctx.struct_name, def.name.as_str());
                    if let Some(var) = self.lookup_env_var(&key) {
                        var
                    } else {
                        let var = self.fresh_type_var();
                        self.insert_env(key, EnvEntry::Mono(var));
                        var
                    }
                } else if in_impl {
                    self.fresh_type_var()
                } else {
                    if !in_impl {
                        self.register_symbol(&def.name);
                    }
                    self.symbol_var(&def.name)
                };
                if def.sig.generics_params.is_empty() {
                    self.prebind_function_signature(def, fn_var);
                } else {
                    let fn_key = self
                        .impl_stack
                        .last()
                        .cloned()
                        .flatten()
                        .map(|ctx| format!("{}::{}", ctx.struct_name, def.name.as_str()));
                    self.enter_scope();
                    for param in &def.sig.generics_params {
                        let var = self.register_generic_param(param.name.as_str());
                        let bounds = Self::extract_trait_bounds(&param.bounds);
                        if !bounds.is_empty() {
                            self.generic_trait_bounds.insert(var, bounds);
                        }
                    }
                    let mut ok = true;
                    let mut param_vars = Vec::new();
                    for param in &def.sig.params {
                        match self.type_from_ast_ty(&param.ty) {
                            Ok(var) => param_vars.push(var),
                            Err(_) => {
                                ok = false;
                                break;
                            }
                        }
                    }
                    let ret_var = if ok {
                        if let Some(ret_ty) = def.sig.ret_ty.as_ref() {
                            self.type_from_ast_ty(ret_ty).ok()
                        } else {
                            Some(self.unit_type_var())
                        }
                    } else {
                        None
                    };
                    if ok {
                        if let Some(ret_var) = ret_var {
                            self.bind(
                                fn_var,
                                TypeTerm::Function(FunctionTerm {
                                    params: param_vars,
                                    ret: ret_var,
                                }),
                            );
                        } else {
                            ok = false;
                        }
                    }
                    self.exit_scope();
                    if ok {
                        if let Ok(scheme) = self.generalize(fn_var) {
                            if let Some(key) = fn_key.as_ref() {
                                self.replace_env_entry(key.as_str(), EnvEntry::Poly(scheme));
                            } else {
                                self.replace_env_entry(def.name.as_str(), EnvEntry::Poly(scheme));
                            }
                        }
                    }
                }
            }
            ItemKind::DeclFunction(decl) => {
                let in_impl = self.impl_stack.last().is_some();
                if !in_impl {
                    self.record_function_signature(&decl.name, &decl.sig);
                    if decl.sig.abi == Abi::C {
                        self.record_extern_function_signature(&decl.name, &decl.sig);
                    }
                    self.register_symbol(&decl.name);
                }
                let fn_var = if let Some(ctx) = self.impl_stack.last().cloned().flatten() {
                    let key = format!("{}::{}", ctx.struct_name, decl.name.as_str());
                    if let Some(var) = self.lookup_env_var(&key) {
                        var
                    } else {
                        let var = self.fresh_type_var();
                        self.insert_env(key, EnvEntry::Mono(var));
                        var
                    }
                } else if in_impl {
                    self.fresh_type_var()
                } else {
                    self.symbol_var(&decl.name)
                };
                if decl.sig.generics_params.is_empty() && decl.sig.receiver.is_none() {
                    self.prebind_decl_function_signature(decl, fn_var);
                }
            }
            ItemKind::Module(module) => {
                self.record_module_def(module.name.as_str());
                self.push_module_path(module.name.as_str());
                self.enter_scope();
                self.module_scope_depths
                    .push(self.env.len().saturating_sub(1));
                for child in &module.items {
                    self.predeclare_item(child);
                }
                self.exit_scope();
                self.module_scope_depths.pop();
                self.pop_module_path();
                self.register_qualified_items(&module.items, module.name.as_str());
            }
            ItemKind::Impl(impl_block) => {
                let ctx = self.resolve_impl_context(&impl_block.self_ty);
                self.impl_stack.push(ctx.clone());
                if let Some(ref ctx) = ctx {
                    for child in &impl_block.items {
                        if let ItemKind::DefFunction(func) = child.kind() {
                            self.register_method_stub(ctx, func);
                            for candidate in self.struct_name_variants(&ctx.struct_name) {
                                let key = format!("{}::{}", candidate, func.name.as_str());
                                if self.lookup_env_var(&key).is_none() {
                                    let var = self.fresh_type_var();
                                    self.insert_env(key, EnvEntry::Mono(var));
                                    self.prebind_function_signature(func, var);
                                }
                            }
                        }
                    }
                }

                self.enter_scope();
                for child in &impl_block.items {
                    self.predeclare_item(child);
                }
                self.exit_scope();
                self.impl_stack.pop();
            }
            ItemKind::Expr(expr) => {
                if let ExprKind::Struct(struct_expr) = expr.kind() {
                    if let Some(name) = self.struct_name_from_expr(&struct_expr.name) {
                        if let Some(def) = self.struct_defs.get(&name).cloned() {
                            self.struct_defs.insert(name, def);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn predeclare_macro_item(&mut self, mac: &ItemMacro) {
        let macro_name = mac
            .invocation
            .path
            .segments
            .last()
            .map(|ident| ident.as_str());
        let Some(macro_name) = macro_name else {
            return;
        };
        let tokens = tokenize_macro_tokens(&mac.invocation.tokens);
        match macro_name {
            "common_struct" => {
                if let Some(name) = find_ident_after_keyword(&tokens, "struct") {
                    self.register_placeholder_struct(&name);
                }
            }
            "common_enum" => {
                if let Some(name) = find_ident_after_keyword(&tokens, "enum") {
                    self.register_placeholder_enum(&name);
                }
            }
            "plain_value" => {
                if let Some(name) = find_first_type_ident(&tokens) {
                    self.register_placeholder_struct(&name);
                }
            }
            _ => {}
        }
    }

    fn register_placeholder_struct(&mut self, name: &str) {
        let key = self
            .qualified_name(name)
            .unwrap_or_else(|| name.to_string());
        if self.struct_defs.contains_key(&key) {
            return;
        }
        let ty = TypeStruct {
            name: Ident::new(name),
            generics_params: Vec::new(),
            fields: Vec::new(),
        };
        self.struct_defs.insert(key, ty);
        self.register_symbol(&Ident::new(name));
    }

    fn register_placeholder_enum(&mut self, name: &str) {
        let key = self
            .qualified_name(name)
            .unwrap_or_else(|| name.to_string());
        if self.enum_defs.contains_key(&key) {
            return;
        }
        let ty = TypeEnum {
            name: Ident::new(name),
            generics_params: Vec::new(),
            variants: Vec::new(),
        };
        self.enum_defs.insert(key, ty);
        self.register_symbol(&Ident::new(name));
    }

    fn register_import_aliases(&mut self, import: &ItemImport) {
        let entries = match self.expand_import_tree(&import.tree, Vec::new()) {
            Ok(entries) => entries,
            Err(err) => {
                self.emit_error(format!("failed to expand import tree: {}", err));
                return;
            }
        };

        for (path_segments, alias) in entries {
            if let Some(first) = path_segments.first() {
                if first == "std" || first == "core" || first == "alloc" {
                    continue;
                }
            }
            if path_segments.is_empty() {
                continue;
            }
            let qualified = path_segments.join("::");
            if self.lookup_env_var(&qualified).is_some() {
                self.insert_symbol_alias(&alias, qualified);
                continue;
            }
            if self.module_defs.contains(&path_segments) {
                self.insert_module_alias(&alias, path_segments);
                continue;
            }
            if self.lossy_mode {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Any);
                self.insert_env(qualified.clone(), EnvEntry::Mono(var));
                self.insert_symbol_alias(&alias, qualified);
                self.emit_warning(format!("unresolved import: {}", alias));
            } else {
                self.emit_error(format!("unresolved import: {}", qualified));
            }
        }
    }

    fn insert_module_alias(&mut self, alias: &str, path: Vec<String>) {
        if let Some(scope) = self.module_aliases.last_mut() {
            scope.insert(alias.to_string(), path);
        }
    }

    fn insert_symbol_alias(&mut self, alias: &str, qualified: String) {
        if let Some(scope) = self.symbol_aliases.last_mut() {
            scope.insert(alias.to_string(), qualified);
        }
    }

    fn expand_import_tree(
        &self,
        tree: &ItemImportTree,
        base: Vec<String>,
    ) -> Result<Vec<(Vec<String>, String)>> {
        match tree {
            ItemImportTree::Path(path) => self.expand_import_segments(&path.segments, base),
            ItemImportTree::Group(group) => {
                let mut results = Vec::new();
                for item in &group.items {
                    results.extend(self.expand_import_tree(item, base.clone())?);
                }
                Ok(results)
            }
            ItemImportTree::Root => self.expand_import_segments(&[], Vec::new()),
            ItemImportTree::SelfMod => self.expand_import_segments(&[], self.module_path.clone()),
            ItemImportTree::SuperMod => self.expand_import_segments(&[], self.parent_module_path()),
            ItemImportTree::Crate => self.expand_import_segments(&[], Vec::new()),
            ItemImportTree::Glob => Err(typing_error("glob imports are not yet supported")),
            _ => self.expand_import_segments(std::slice::from_ref(tree), base),
        }
    }

    fn expand_import_segments(
        &self,
        segments: &[ItemImportTree],
        base: Vec<String>,
    ) -> Result<Vec<(Vec<String>, String)>> {
        if segments.is_empty() {
            return Ok(Vec::new());
        }

        let first = &segments[0];
        let rest = &segments[1..];
        match first {
            ItemImportTree::Ident(ident) => {
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
            ItemImportTree::Rename(rename) => {
                if !rest.is_empty() {
                    return Err(typing_error("rename segments must be terminal"));
                }
                let mut new_base = base;
                new_base.push(rename.from.name.clone());
                Ok(vec![(new_base, rename.to.name.clone())])
            }
            ItemImportTree::Group(group) => {
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
            ItemImportTree::Path(path) => {
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
            ItemImportTree::Root => self.expand_import_segments(rest, Vec::new()),
            ItemImportTree::SelfMod => self.expand_import_segments(rest, self.module_path.clone()),
            ItemImportTree::SuperMod => {
                self.expand_import_segments(rest, self.parent_module_path())
            }
            ItemImportTree::Crate => self.expand_import_segments(rest, Vec::new()),
            ItemImportTree::Glob => Err(typing_error("glob imports are not yet supported")),
        }
    }

    fn prebind_function_signature(&mut self, func: &ItemDefFunction, fn_var: TypeVarId) {
        if !func.sig.generics_params.is_empty() || func.sig.receiver.is_some() {
            return;
        }

        let root = self.find(fn_var);
        if matches!(
            self.type_vars[root].kind,
            TypeVarKind::Bound(TypeTerm::Function(_))
        ) {
            return;
        }

        let mut param_vars = Vec::new();
        for param in &func.sig.params {
            match self.type_from_ast_ty(&param.ty) {
                Ok(var) => param_vars.push(var),
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare parameter type for {}: {}",
                        func.name, err
                    ));
                    return;
                }
            }
        }

        let ret_var = if let Some(ret_ty) = &func.sig.ret_ty {
            match self.type_from_ast_ty(ret_ty) {
                Ok(var) => var,
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare return type for {}: {}",
                        func.name, err
                    ));
                    return;
                }
            }
        } else {
            let unit = self.fresh_type_var();
            self.bind(unit, TypeTerm::Unit);
            unit
        };

        self.bind(
            fn_var,
            TypeTerm::Function(FunctionTerm {
                params: param_vars,
                ret: ret_var,
            }),
        );
    }

    fn prebind_decl_function_signature(&mut self, decl: &ItemDeclFunction, fn_var: TypeVarId) {
        if !decl.sig.generics_params.is_empty() || decl.sig.receiver.is_some() {
            return;
        }

        let root = self.find(fn_var);
        if matches!(
            self.type_vars[root].kind,
            TypeVarKind::Bound(TypeTerm::Function(_))
        ) {
            return;
        }

        let mut param_vars = Vec::new();
        for param in &decl.sig.params {
            match self.type_from_ast_ty(&param.ty) {
                Ok(var) => param_vars.push(var),
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare parameter type for {}: {}",
                        decl.name, err
                    ));
                    return;
                }
            }
        }

        let ret_var = if let Some(ret_ty) = &decl.sig.ret_ty {
            match self.type_from_ast_ty(ret_ty) {
                Ok(var) => var,
                Err(err) => {
                    self.emit_error(format!(
                        "failed to predeclare return type for {}: {}",
                        decl.name, err
                    ));
                    return;
                }
            }
        } else {
            let unit = self.fresh_type_var();
            self.bind(unit, TypeTerm::Unit);
            unit
        };

        self.bind(
            fn_var,
            TypeTerm::Function(FunctionTerm {
                params: param_vars,
                ret: ret_var,
            }),
        );
    }

    fn infer_item(&mut self, item: &mut Item) -> Result<()> {
        let span = item.span();
        let previous = self.current_span;
        let active = self.span_or_previous(span, previous);
        self.current_span = active;
        let result = (|| {
            let ty = match item.kind_mut() {
                ItemKind::DefStruct(def) => {
                    self.validate_struct_recursion(def.name.as_str(), &def.value.fields);
                    self.insert_struct_def(&def.name, def.value.clone());
                    let ty = Ty::Struct(def.value.clone());
                    let placeholder = self.symbol_var(&def.name);
                    let var = self.type_from_ast_ty(&ty)?;
                    self.unify(placeholder, var)?;
                    self.generalize_symbol(def.name.as_str(), placeholder)?;
                    ty
                }
                ItemKind::DefStructural(def) => {
                    self.validate_struct_recursion(def.name.as_str(), &def.value.fields);
                    let struct_ty = TypeStruct {
                        name: def.name.clone(),
                        generics_params: Vec::new(),
                        fields: def.value.fields.clone(),
                    };
                    self.insert_struct_def(&def.name, struct_ty.clone());
                    let ty = Ty::Struct(struct_ty);
                    let placeholder = self.symbol_var(&def.name);
                    let var = self.type_from_ast_ty(&ty)?;
                    self.unify(placeholder, var)?;
                    self.generalize_symbol(def.name.as_str(), placeholder)?;
                    ty
                }
                ItemKind::DefType(def) => {
                    // Resolve the RHS to a concrete type; if it is structural, materialize it as a
                    // named struct so that later term-level syntax like `Foo { ... }` can type-check.
                    let placeholder = self.symbol_var(&def.name);
                    let value_var = self.type_from_ast_ty(&def.value)?;
                    let resolved = self.resolve_to_ty(value_var)?;

                    let normalized = match resolved {
                        Ty::Structural(structural) => {
                            let struct_ty = TypeStruct {
                                name: def.name.clone(),
                                generics_params: Vec::new(),
                                fields: structural.fields.clone(),
                            };
                            self.insert_struct_def(&def.name, struct_ty.clone());
                            Ty::Struct(struct_ty)
                        }
                        Ty::Struct(struct_ty) => {
                            self.insert_struct_def(&def.name, struct_ty.clone());
                            Ty::Struct(struct_ty)
                        }
                        Ty::Enum(enum_ty) => {
                            self.insert_enum_def(&def.name, enum_ty.clone());
                            Ty::Enum(enum_ty)
                        }
                        other => other,
                    };

                    let var = self.type_from_ast_ty(&normalized)?;
                    self.unify(placeholder, var)?;
                    self.generalize_symbol(def.name.as_str(), placeholder)?;
                    normalized
                }
                ItemKind::DefEnum(def) => {
                    self.enter_scope();
                    if !def.value.generics_params.is_empty() {
                        for param in &def.value.generics_params {
                            let var = self.register_generic_param(param.name.as_str());
                            let bounds = Self::extract_trait_bounds(&param.bounds);
                            if !bounds.is_empty() {
                                self.generic_trait_bounds.insert(var, bounds);
                            }
                        }
                    }

                    self.insert_enum_def(&def.name, def.value.clone());
                    let ty = Ty::Enum(def.value.clone());
                    let placeholder = self.symbol_var(&def.name);
                    let var = self.type_from_ast_ty(&ty)?;
                    self.unify(placeholder, var)?;
                    self.generalize_symbol(def.name.as_str(), placeholder)?;

                    let enum_name = self
                        .qualified_name(def.name.as_str())
                        .unwrap_or_else(|| def.name.as_str().to_string());
                    if let Some(variant_keys) = self.enum_variants.get(&enum_name).cloned() {
                        let enum_var = placeholder;
                        for (variant, qualified) in
                            def.value.variants.iter().zip(variant_keys.into_iter())
                        {
                            if let Some(variant_var) = self.lookup_env_var(qualified.as_str()) {
                                let variant_type_var = if matches!(variant.value, Ty::Unit(_)) {
                                    enum_var
                                } else if let Ty::Tuple(tuple) = &variant.value {
                                    let mut param_vars = Vec::new();
                                    for elem in &tuple.types {
                                        param_vars.push(self.type_from_ast_ty(elem)?);
                                    }
                                    let fn_var = self.fresh_type_var();
                                    self.bind(
                                        fn_var,
                                        TypeTerm::Function(FunctionTerm {
                                            params: param_vars,
                                            ret: enum_var,
                                        }),
                                    );
                                    fn_var
                                } else {
                                    let payload_var = self.type_from_ast_ty(&variant.value)?;
                                    let fn_var = self.fresh_type_var();
                                    self.bind(
                                        fn_var,
                                        TypeTerm::Function(FunctionTerm {
                                            params: vec![payload_var],
                                            ret: enum_var,
                                        }),
                                    );
                                    fn_var
                                };
                                let _ = self.unify(variant_var, variant_type_var);
                                let _ = self.generalize_symbol(qualified.as_str(), variant_var);
                            }
                        }
                    }

                    self.exit_scope();

                    ty
                }
                ItemKind::DefConst(def) => {
                    let placeholder = self.symbol_var(&def.name);
                    if let Some(annot) = def.ty.as_ref() {
                        def.value.set_ty(annot.clone());
                    }
                    let expr_var = {
                        let mut value = def.value.as_mut();
                        self.infer_expr(&mut value)?
                    };

                    if let Some(annot) = &def.ty {
                        let annot_var = self.type_from_ast_ty(annot)?;
                        self.unify(expr_var, annot_var)?;
                    }

                    self.unify(placeholder, expr_var)?;
                    let ty = self.resolve_to_ty(expr_var)?;
                    def.ty_annotation = Some(ty.clone());
                    def.ty.get_or_insert(ty.clone());
                    self.generalize_symbol(def.name.as_str(), placeholder)?;
                    ty
                }
                ItemKind::DefStatic(def) => {
                    let placeholder = self.symbol_var(&def.name);
                    let expr_var = {
                        let mut value = def.value.as_mut();
                        self.infer_expr(&mut value)?
                    };
                    let ty_var = self.type_from_ast_ty(&def.ty)?;
                    self.unify(expr_var, ty_var)?;
                    self.unify(placeholder, expr_var)?;
                    let ty = self.resolve_to_ty(expr_var)?;
                    def.ty_annotation = Some(ty.clone());
                    self.generalize_symbol(def.name.as_str(), placeholder)?;
                    ty
                }
                ItemKind::DefFunction(func) => self.infer_function(func)?,
                ItemKind::DeclConst(decl) => {
                    let ty = decl.ty.clone();
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::DeclStatic(decl) => {
                    let ty = decl.ty.clone();
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::DeclType(decl) => {
                    let ty = Ty::TypeBounds(decl.bounds.clone());
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::DeclFunction(decl) => {
                    self.validate_extern_c_signature(&decl.sig);
                    let ty = self.ty_from_function_signature(&decl.sig)?;
                    decl.ty_annotation = Some(ty.clone());
                    ty
                }
                ItemKind::Module(module) => {
                    self.push_module_path(module.name.as_str());
                    self.enter_scope();
                    self.module_scope_depths
                        .push(self.env.len().saturating_sub(1));
                    for child in &module.items {
                        self.predeclare_item(child);
                    }
                    for child in &mut module.items {
                        self.infer_item(child)?;
                    }
                    self.exit_scope();
                    self.module_scope_depths.pop();
                    self.pop_module_path();
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Import(import) => {
                    self.register_import_aliases(import);
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Macro(_) => {
                    if self.lossy_mode {
                        Ty::Unit(TypeUnit)
                    } else {
                        self.emit_error("macro items are not yet supported");
                        Ty::Unknown(TypeUnknown)
                    }
                }
                ItemKind::DefTrait(trait_def) => {
                    let trait_name = trait_def.name.as_str().to_string();
                    self.enter_scope();

                    // Provide `Self` inside trait methods as a generic parameter
                    // bounded by the trait itself.
                    let self_var = self.register_generic_param("Self");
                    self.generic_trait_bounds
                        .insert(self_var, vec![trait_name.clone()]);

                    for member in &mut trait_def.items {
                        match member.kind_mut() {
                            ItemKind::DeclFunction(decl) => {
                                let ty = self.ty_from_function_signature(&decl.sig)?;
                                decl.ty_annotation = Some(ty);
                            }
                            ItemKind::DefFunction(func) => {
                                self.infer_trait_method(func)?;
                            }
                            _ => {}
                        }
                    }

                    self.exit_scope();
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Impl(impl_block) => {
                    let ctx = self.resolve_impl_context(&impl_block.self_ty);

                    if let (Some(ctx), Some(trait_ty)) =
                        (ctx.as_ref(), impl_block.trait_ty.as_ref())
                    {
                        let trait_name = trait_ty.to_string();
                        self.impl_traits
                            .entry(ctx.struct_name.clone())
                            .or_default()
                            .insert(trait_name.clone());

                        if let Some(methods) = self.trait_method_sigs.get(&trait_name).cloned() {
                            for (method_name, sig) in methods {
                                if sig.receiver.is_none() {
                                    continue;
                                }
                                // Ensure default trait methods are callable as inherent methods
                                // on this concrete receiver type.
                                let scheme = self.scheme_from_method_signature(&sig)?;
                                let receiver_ty = sig
                                    .receiver
                                    .as_ref()
                                    .map(|receiver| self.ty_for_receiver(ctx, receiver));
                                for candidate in self.struct_name_variants(&ctx.struct_name) {
                                    let entry = self.struct_methods.entry(candidate).or_default();
                                    if entry.contains_key(&method_name) {
                                        continue;
                                    }
                                    entry.insert(
                                        method_name.clone(),
                                        MethodRecord {
                                            receiver_ty: receiver_ty.clone(),
                                            scheme: Some(scheme.clone()),
                                        },
                                    );
                                }
                            }
                        }
                    }

                    self.impl_stack.push(ctx.clone());
                    self.enter_scope();
                    for child in &impl_block.items {
                        self.predeclare_item(child);
                    }
                    for child in &mut impl_block.items {
                        self.infer_item(child)?;
                    }
                    self.exit_scope();
                    self.impl_stack.pop();
                    Ty::Unit(TypeUnit)
                }
                ItemKind::Expr(expr) => {
                    if let ExprKind::Splice(splice) = expr.kind_mut() {
                        let token_var = self.infer_expr(splice.token.as_mut())?;
                        let token_ty = self.resolve_to_ty(token_var)?;
                        if !self.is_item_quote(&token_ty) {
                            match token_ty {
                                Ty::Quote(quote) => {
                                    self.emit_error(format!(
                                        "splice in item position requires item token, found {:?}",
                                        quote.kind
                                    ));
                                }
                                _ => self.emit_error("splice expects a quote token expression"),
                            }
                        }
                        Ty::Unit(TypeUnit)
                    } else {
                        let var = self.infer_expr(expr)?;
                        self.resolve_to_ty(var)?
                    }
                }
                _ => {
                    self.emit_error("type inference for item not implemented");
                    Ty::Unknown(TypeUnknown)
                }
            };

            item.set_ty(ty);
            Ok(())
        })();
        self.current_span = previous;
        result.map_err(|err| self.error_with_span(err, active))
    }

    fn infer_function(&mut self, func: &mut ItemDefFunction) -> Result<Ty> {
        self.validate_extern_c_signature(&func.sig);
        let is_lang_item = func.attrs.find_by_name("lang").is_some();
        let impl_ctx = self.impl_stack.last().cloned().flatten();
        let fn_key = impl_ctx
            .as_ref()
            .map(|ctx| format!("{}::{}", ctx.struct_name, func.name.as_str()));
        let fn_var = if let Some(key) = fn_key.as_ref() {
            if let Some(var) = self.lookup_env_var(key.as_str()) {
                var
            } else {
                let var = self.fresh_type_var();
                self.insert_env(key.clone(), EnvEntry::Mono(var));
                var
            }
        } else {
            self.symbol_var(&func.name)
        };
        let param_count = func.sig.params.len();
        let existing_signature = {
            let root = self.find(fn_var);
            match self.type_vars[root].kind.clone() {
                TypeVarKind::Bound(TypeTerm::Function(func_term)) => {
                    if func_term.params.len() == param_count {
                        Some(func_term)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        };

        self.enter_scope();

        let mut receiver_ty: Option<Ty> = None;
        if let Some(receiver) = func.sig.receiver.as_ref() {
            if let Some(ctx) = impl_ctx.as_ref() {
                let receiver_type = self.ty_for_receiver(ctx, receiver);
                let self_var = self.fresh_type_var();
                let expected = self.type_from_ast_ty(&receiver_type)?;
                self.unify(self_var, expected)?;
                self.insert_env("self".to_string(), EnvEntry::Mono(self_var));
                receiver_ty = Some(receiver_type);
            } else {
                self.emit_error(format!(
                    "method {} defined without an impl context",
                    func.name
                ));
            }
        }

        if !func.sig.generics_params.is_empty() {
            for param in &func.sig.generics_params {
                let var = self.register_generic_param(param.name.as_str());
                let bounds = Self::extract_trait_bounds(&param.bounds);
                if !bounds.is_empty() {
                    self.generic_trait_bounds.insert(var, bounds);
                }
            }
        }

        let mut param_vars = Vec::new();
        for (idx, param) in func.sig.params.iter_mut().enumerate() {
            let var = existing_signature
                .as_ref()
                .and_then(|sig| sig.params.get(idx).cloned())
                .unwrap_or_else(|| self.fresh_type_var());
            let annot_var = self.type_from_ast_ty(&param.ty)?;
            self.unify(var, annot_var)?;
            self.insert_env(param.name.as_str().to_string(), EnvEntry::Mono(var));
            let resolved = self.resolve_to_ty(var)?;
            param.ty_annotation = Some(resolved);
            param_vars.push(var);
        }

        let body_var = if is_lang_item {
            if let Some(ret) = &func.sig.ret_ty {
                self.type_from_ast_ty(ret)?
            } else {
                self.fresh_type_var()
            }
        } else if let Some(kind) = func.sig.quote_kind {
            let body_block = func.body.as_ref().clone().into_block();
            let mut quote_expr = Expr::from(ExprKind::Quote(ExprQuote {
                span: Span::null(),
                block: body_block,
                kind: Some(kind),
            }));
            self.infer_expr(&mut quote_expr)?
        } else {
            let mut body = func.body.as_mut();
            self.infer_expr(&mut body)?
        };

        let ret_var = if let Some(existing) = existing_signature.as_ref().map(|sig| sig.ret) {
            self.unify(existing, body_var)?;
            if let Some(ret) = &func.sig.ret_ty {
                let annot_var = self.type_from_ast_ty(ret)?;
                self.unify(existing, annot_var)?;
            }
            existing
        } else if let Some(ret) = &func.sig.ret_ty {
            let annot_var = self.type_from_ast_ty(ret)?;
            self.unify(body_var, annot_var)?;
            annot_var
        } else {
            body_var
        };

        let ret_ty = self.resolve_to_ty(ret_var.clone())?;
        func.sig.ret_ty.get_or_insert(ret_ty.clone());

        self.exit_scope();

        let mut param_tys = Vec::new();
        for var in &param_vars {
            param_tys.push(self.resolve_to_ty(*var)?);
        }

        self.bind(
            fn_var,
            TypeTerm::Function(FunctionTerm {
                params: param_vars.clone(),
                ret: ret_var,
            }),
        );

        let scheme = self.generalize(fn_var)?;
        let scheme_env = scheme.clone();
        if let Some(key) = fn_key.as_ref() {
            self.replace_env_entry(key.as_str(), EnvEntry::Poly(scheme_env));
        } else {
            self.replace_env_entry(func.name.as_str(), EnvEntry::Poly(scheme_env));
        }

        if let Some(ctx) = impl_ctx.as_ref() {
            for candidate in self.struct_name_variants(&ctx.struct_name) {
                let entry = self.struct_methods.entry(candidate).or_default();
                let record = entry
                    .entry(func.name.as_str().to_string())
                    .or_insert(MethodRecord {
                        receiver_ty: receiver_ty.clone(),
                        scheme: None,
                    });
                if record.receiver_ty.is_none() && receiver_ty.is_some() {
                    record.receiver_ty = receiver_ty.clone();
                }
                record.scheme = Some(scheme.clone());
            }
        }

        let func_ty = TypeFunction {
            params: param_tys.clone(),
            generics_params: func.sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty.clone())),
        };

        func.ty = Some(func_ty.clone());
        let ty = Ty::Function(func_ty);
        func.ty_annotation = Some(ty.clone());
        Ok(ty)
    }

    fn infer_trait_method(&mut self, func: &mut ItemDefFunction) -> Result<Ty> {
        let fn_var = self.symbol_var(&func.name);

        self.enter_scope();

        if let Some(receiver) = func.sig.receiver.as_ref() {
            let self_ty = Ty::locator(Locator::ident("Self"));
            let receiver_type = match receiver {
                FunctionParamReceiver::Implicit
                | FunctionParamReceiver::Value
                | FunctionParamReceiver::MutValue => self_ty,
                FunctionParamReceiver::Ref | FunctionParamReceiver::RefStatic => Ty::Reference(
                    TypeReference {
                        ty: Box::new(self_ty),
                        mutability: Some(false),
                        lifetime: None,
                    }
                    .into(),
                ),
                FunctionParamReceiver::RefMut | FunctionParamReceiver::RefMutStatic => {
                    Ty::Reference(
                        TypeReference {
                            ty: Box::new(self_ty),
                            mutability: Some(true),
                            lifetime: None,
                        }
                        .into(),
                    )
                }
            };

            let self_var = self.fresh_type_var();
            let expected = self.type_from_ast_ty(&receiver_type)?;
            self.unify(self_var, expected)?;
            self.insert_env("self".to_string(), EnvEntry::Mono(self_var));
        }

        if !func.sig.generics_params.is_empty() {
            for param in &func.sig.generics_params {
                let var = self.register_generic_param(param.name.as_str());
                let bounds = Self::extract_trait_bounds(&param.bounds);
                if !bounds.is_empty() {
                    self.generic_trait_bounds.insert(var, bounds);
                }
            }
        }

        let mut param_vars = Vec::new();
        for param in func.sig.params.iter_mut() {
            let var = self.fresh_type_var();
            let annot_var = self.type_from_ast_ty(&param.ty)?;
            self.unify(var, annot_var)?;
            self.insert_env(param.name.as_str().to_string(), EnvEntry::Mono(var));
            let resolved = self.resolve_to_ty(var)?;
            param.ty_annotation = Some(resolved);
            param_vars.push(var);
        }

        let body_var = {
            let mut body = func.body.as_mut();
            self.infer_expr(&mut body)?
        };

        let ret_var = if let Some(ret) = &func.sig.ret_ty {
            let annot_var = self.type_from_ast_ty(ret)?;
            self.unify(body_var, annot_var)?;
            annot_var
        } else {
            body_var
        };

        self.exit_scope();

        self.bind(
            fn_var,
            TypeTerm::Function(FunctionTerm {
                params: param_vars.clone(),
                ret: ret_var,
            }),
        );

        let scheme = self.generalize(fn_var)?;
        self.replace_env_entry(func.name.as_str(), EnvEntry::Poly(scheme));

        let mut param_tys = Vec::new();
        for var in &param_vars {
            param_tys.push(self.resolve_to_ty(*var)?);
        }
        let ret_ty = self.resolve_to_ty(ret_var)?;
        func.sig.ret_ty.get_or_insert(ret_ty.clone());

        let func_ty = TypeFunction {
            params: param_tys,
            generics_params: func.sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty)),
        };
        func.ty = Some(func_ty.clone());
        let ty = Ty::Function(func_ty);
        func.ty_annotation = Some(ty.clone());
        Ok(ty)
    }

    // infer_expr moved to typing::infer_expr

    // infer_block moved to typing::infer_stmt

    // infer_if moved to typing::infer_stmt

    // infer_binop moved to typing::infer_expr

    // infer_unop moved to typing::infer_expr

    // infer_loop moved to typing::infer_stmt

    // infer_while moved to typing::infer_stmt

    // moved: infer_reference, infer_dereference, infer_index, infer_range, infer_splat, infer_splat_dict

    // moved: infer_intrinsic

    // moved: infer_closure

    // infer_match moved to typing::infer_stmt

    // moved: infer_invoke

    fn apply_pattern_generalization(&mut self, info: &PatternInfo) -> Result<()> {
        for binding in &info.bindings {
            let scheme = self.generalize(binding.var)?;
            self.replace_env_entry(&binding.name, EnvEntry::Poly(scheme));
        }
        Ok(())
    }

    // generalize moved to typing/unify.rs

    // build_scheme_type moved to typing/unify.rs

    // scheme_from_term moved to typing/unify.rs

    // instantiate_scheme moved to typing/unify.rs

    // instantiate_scheme_type moved to typing/unify.rs

    fn scheme_from_method_signature(&mut self, sig: &FunctionSignature) -> Result<TypeScheme> {
        let fn_var = self.fresh_type_var();
        let mut param_vars = Vec::new();
        for param in &sig.params {
            param_vars.push(self.type_from_ast_ty(&param.ty)?);
        }
        let ret_var = if let Some(ret) = sig.ret_ty.as_ref() {
            self.type_from_ast_ty(ret)?
        } else {
            self.unit_type_var()
        };
        self.bind(
            fn_var,
            TypeTerm::Function(FunctionTerm {
                params: param_vars,
                ret: ret_var,
            }),
        );
        self.generalize(fn_var)
    }

    fn extract_trait_bounds(bounds: &TypeBounds) -> Vec<String> {
        bounds
            .bounds
            .iter()
            .filter_map(|expr| match expr.kind() {
                ExprKind::Locator(locator) => Some(locator.to_string()),
                ExprKind::Value(value) => match value.as_ref() {
                    Value::Type(Ty::Expr(inner)) => match inner.kind() {
                        ExprKind::Locator(locator) => Some(locator.to_string()),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            })
            .collect()
    }

    fn register_generic_param(&mut self, name: &str) -> TypeVarId {
        let var = self.fresh_type_var();
        self.insert_env(name.to_string(), EnvEntry::Mono(var));
        if let Some(scope) = self.generic_scopes.last_mut() {
            scope.insert(name.to_string());
        }
        var
    }

    // unused: generic_name_in_scope (removed)

    fn insert_env(&mut self, name: String, entry: EnvEntry) {
        if let Some(scope) = self.env.last_mut() {
            scope.insert(name, entry);
        }
    }

    fn replace_env_entry(&mut self, name: &str, entry: EnvEntry) {
        for scope in self.env.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), entry);
                return;
            }
        }
        if let Some(scope) = self.env.last_mut() {
            scope.insert(name.to_string(), entry);
        }
    }

    fn enter_scope(&mut self) {
        self.current_level += 1;
        self.env.push(HashMap::new());
        self.generic_scopes.push(HashSet::new());
        self.module_aliases.push(HashMap::new());
        self.symbol_aliases.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.env.pop();
        self.generic_scopes.pop();
        self.module_aliases.pop();
        self.symbol_aliases.pop();
        if self.current_level > 0 {
            self.current_level -= 1;
        }
    }

    fn push_module_path(&mut self, name: &str) {
        self.module_path.push(name.to_string());
    }

    fn pop_module_path(&mut self) {
        self.module_path.pop();
    }

    fn record_module_def(&mut self, name: &str) {
        let mut path = self.module_path.clone();
        path.push(name.to_string());
        self.module_defs.insert(path);
        if self.module_path.is_empty() {
            self.root_modules.insert(name.to_string());
        }
    }

    fn qualified_name(&self, name: &str) -> Option<String> {
        if self.module_path.is_empty() {
            None
        } else {
            let mut qualified = self.module_path.clone();
            qualified.push(name.to_string());
            Some(qualified.join("::"))
        }
    }

    fn insert_struct_def(&mut self, name: &Ident, def: TypeStruct) {
        let key = if name.as_str().contains("::") {
            name.as_str().to_string()
        } else {
            self.qualified_name(name.as_str())
                .unwrap_or_else(|| name.as_str().to_string())
        };
        self.struct_defs.insert(key, def);
    }

    fn insert_enum_def(&mut self, name: &Ident, def: TypeEnum) {
        let key = if name.as_str().contains("::") {
            name.as_str().to_string()
        } else {
            self.qualified_name(name.as_str())
                .unwrap_or_else(|| name.as_str().to_string())
        };
        self.enum_defs.insert(key, def);
    }

    fn parent_module_path(&self) -> Vec<String> {
        let mut parent = self.module_path.clone();
        parent.pop();
        parent
    }

    // fresh_type_var moved to typing/unify.rs

    // unit_type_var moved to typing/unify.rs

    // nothing_type_var moved to typing/unify.rs

    // bind moved to typing/unify.rs

    // find moved to typing/unify.rs

    // unify moved to typing/unify.rs

    // occurs_in_term moved to typing/unify.rs

    // occurs_in moved to typing/unify.rs

    // unify_terms moved to typing/unify.rs

    // resolve_to_ty moved to typing/unify.rs

    // term_to_ty moved to typing/unify.rs

    // type_from_ast_ty moved to typing/unify.rs

    fn lookup_associated_function(&mut self, locator: &Locator) -> Result<Option<TypeVarId>> {
        if let Locator::Path(path) = locator {
            if path.segments.len() >= 2 {
                if let Some(method_segment) = path.segments.last() {
                    let method_name = method_segment.as_str();
                    let struct_segments = path
                        .segments
                        .iter()
                        .take(path.segments.len() - 1)
                        .map(|seg| seg.as_str().to_string())
                        .collect::<Vec<_>>();
                    if let Some(struct_name) = self.resolve_segments_key(&struct_segments) {
                        for candidate in self.struct_name_variants(&struct_name) {
                            let qualified = format!("{}::{}", candidate, method_name);
                            if let Some(var) = self.lookup_env_var(&qualified) {
                                return Ok(Some(var));
                            }
                            if let Some(methods) = self.struct_methods.get(&candidate) {
                                if let Some(record) = methods.get(method_name) {
                                    if let Some(scheme) = record.scheme.as_ref() {
                                        return Ok(Some(self.instantiate_scheme(&scheme.clone())));
                                    }
                                    if let Some(var) = self.lookup_env_var(method_name) {
                                        return Ok(Some(var));
                                    }
                                }
                            }
                        }

                        // Enum tuple variant constructors: `Enum::Variant(...)`.
                        if let Some(enum_def) = self.enum_defs.get(&struct_name).cloned() {
                            if let Some(variant) = enum_def
                                .variants
                                .iter()
                                .find(|v| v.name.as_str() == method_name)
                            {
                                self.enter_scope();
                                if !enum_def.generics_params.is_empty() {
                                    for param in &enum_def.generics_params {
                                        let var = self.register_generic_param(param.name.as_str());
                                        let bounds = Self::extract_trait_bounds(&param.bounds);
                                        if !bounds.is_empty() {
                                            self.generic_trait_bounds.insert(var, bounds);
                                        }
                                    }
                                }
                                let mut params = Vec::new();
                                match &variant.value {
                                    Ty::Unit(_) => {}
                                    Ty::Tuple(tuple_ty) => params.extend(tuple_ty.types.clone()),
                                    other => params.push(other.clone()),
                                }

                                let func_ty = Ty::Function(TypeFunction {
                                    params,
                                    generics_params: Vec::new(),
                                    ret_ty: Some(Box::new(Ty::Enum(enum_def.clone()))),
                                });
                                let func_var = self.type_from_ast_ty(&func_ty)?;
                                self.exit_scope();
                                return Ok(Some(func_var));
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    fn lookup_locator(&mut self, locator: &Locator) -> Result<TypeVarId> {
        if self.check_unimplemented_locator(locator) {
            return Ok(self.error_type_var());
        }
        if let Locator::Path(path) = locator {
            if path.segments.len() >= 2 {
                let variant_name = path.segments.last().map(|seg| seg.as_str());
                let enum_segments = path
                    .segments
                    .iter()
                    .take(path.segments.len() - 1)
                    .map(|seg| seg.as_str().to_string())
                    .collect::<Vec<_>>();
                if let (Some(variant_name), Some(enum_key)) =
                    (variant_name, self.resolve_segments_key(&enum_segments))
                {
                    if let Some(enum_def) = self.enum_defs.get(&enum_key).cloned() {
                        if enum_def
                            .variants
                            .iter()
                            .any(|v| v.name.as_str() == variant_name)
                        {
                            let var = self.fresh_type_var();
                            self.bind(var, TypeTerm::Enum(enum_def));
                            return Ok(var);
                        }
                    }
                }
            }
        }
        if let Some(ident) = locator.as_ident() {
            let name = ident.as_str();
            if let Some(var) = self.lookup_env_var(name) {
                return Ok(var);
            }
            if !self.module_path.is_empty() {
                let mut qualified = self.module_path.clone();
                qualified.push(name.to_string());
                let qualified = qualified.join("::");
                if let Some(var) = self.lookup_env_var(&qualified) {
                    return Ok(var);
                }
            }
        }
        let key = match self.resolve_locator_key(locator) {
            Some(key) => key,
            None => {
                self.emit_error(format!("unresolved symbol: {}", locator));
                return Ok(self.error_type_var());
            }
        };
        if let Some(key) = self.resolve_locator_key(locator) {
            if self.struct_defs.contains_key(&key) || self.enum_defs.contains_key(&key) {
                let var = self.fresh_type_var();
                self.bind(var, TypeTerm::Custom(Ty::Type(TypeType::new(Span::null()))));
                return Ok(var);
            }
        }
        if let Some(var) = self.lookup_env_var(&key) {
            return Ok(var);
        }
        self.emit_error(format!("unresolved symbol: {}", key));
        Ok(self.error_type_var())
    }

    fn resolve_alias_locator(&self, locator: &Locator) -> Option<String> {
        match locator {
            Locator::Ident(ident) => self.lookup_symbol_alias(ident.as_str()),
            Locator::Path(path) => {
                if let Some(first) = path.segments.first() {
                    if let Some(module_path) = self.lookup_module_alias(first.as_str()) {
                        let mut qualified = module_path;
                        qualified.extend(
                            path.segments
                                .iter()
                                .skip(1)
                                .map(|seg| seg.as_str().to_string()),
                        );
                        return Some(qualified.join("::"));
                    }
                    if let Some(symbol_path) = self.lookup_symbol_alias(first.as_str()) {
                        let mut qualified = symbol_path
                            .split("::")
                            .map(|seg| seg.to_string())
                            .collect::<Vec<_>>();
                        qualified.extend(
                            path.segments
                                .iter()
                                .skip(1)
                                .map(|seg| seg.as_str().to_string()),
                        );
                        return Some(qualified.join("::"));
                    }
                }
                None
            }
            Locator::ParameterPath(path) => {
                if let Some(first) = path.segments.first() {
                    if let Some(module_path) = self.lookup_module_alias(first.ident.as_str()) {
                        let mut qualified = module_path;
                        qualified.extend(
                            path.segments
                            .iter()
                            .skip(1)
                            .map(|seg| seg.ident.as_str().to_string()),
                        );
                        return Some(qualified.join("::"));
                    }
                    if let Some(symbol_path) = self.lookup_symbol_alias(first.ident.as_str()) {
                        let mut qualified = symbol_path
                            .split("::")
                            .map(|seg| seg.to_string())
                            .collect::<Vec<_>>();
                        qualified.extend(
                            path.segments
                                .iter()
                                .skip(1)
                                .map(|seg| seg.ident.as_str().to_string()),
                        );
                        return Some(qualified.join("::"));
                    }
                }
                None
            }
        }
    }

    fn lookup_symbol_alias(&self, name: &str) -> Option<String> {
        for scope in self.symbol_aliases.iter().rev() {
            if let Some(target) = scope.get(name) {
                return Some(target.clone());
            }
        }
        None
    }

    fn lookup_module_alias(&self, name: &str) -> Option<Vec<String>> {
        for scope in self.module_aliases.iter().rev() {
            if let Some(path) = scope.get(name) {
                return Some(path.clone());
            }
        }
        None
    }

    fn lookup_env_var(&mut self, name: &str) -> Option<TypeVarId> {
        if let Some(var) = self.lookup_env_var_direct(name) {
            return Some(var);
        }
        let should_retry = self
            .resolution_hook
            .as_mut()
            .map(|hook| hook.resolve_symbol(name))
            .unwrap_or(false);
        if should_retry {
            return self.lookup_env_var_direct(name);
        }
        None
    }

    fn lookup_env_var_direct(&mut self, name: &str) -> Option<TypeVarId> {
        for scope in self.env.iter().rev() {
            if let Some(entry) = scope.get(name) {
                return Some(match entry {
                    EnvEntry::Mono(var) => *var,
                    EnvEntry::Poly(scheme) => {
                        let scheme_clone = scheme.clone();
                        self.instantiate_scheme(&scheme_clone)
                    }
                });
            }
        }
        None
    }

    fn symbol_var(&mut self, name: &Ident) -> TypeVarId {
        let key = name.as_str().to_string();
        if let Some(var) = self.lookup_env_var(&key) {
            return var;
        }
        let var = self.fresh_type_var();
        self.insert_env(key, EnvEntry::Mono(var));
        var
    }

    fn register_symbol(&mut self, name: &Ident) {
        let key = name.as_str().to_string();
        let var = self.fresh_type_var();
        if let Some(scope) = self.env.last_mut() {
            scope.entry(key).or_insert(EnvEntry::Mono(var));
        }
    }

    fn emit_error(&mut self, message: impl Into<String>) {
        let span = self.current_span;
        self.emit_error_with_span(span, message);
    }

    fn emit_error_with_span(&mut self, span: Option<Span>, message: impl Into<String>) {
        let message = message.into();
        if self.lossy_mode {
            if let Some(span) = span {
                self.diagnostics
                    .push(TypingDiagnostic::warning_with_span(message, span));
            } else {
                self.diagnostics.push(TypingDiagnostic::warning(message));
            }
        } else {
            self.has_errors = true;
            if let Some(span) = span {
                self.diagnostics
                    .push(TypingDiagnostic::error_with_span(message, span));
            } else {
                self.diagnostics.push(TypingDiagnostic::error(message));
            }
        }
    }

    fn span_option(&self, span: Span) -> Option<Span> {
        if span.is_null() {
            None
        } else {
            Some(span)
        }
    }

    fn span_or_previous(&self, span: Span, previous: Option<Span>) -> Option<Span> {
        if span.is_null() {
            previous
        } else {
            Some(span)
        }
    }

    fn error_with_span(&self, err: Error, span: Option<Span>) -> Error {
        let Some(span) = span else {
            return err;
        };
        if let Error::Diagnostic(ref diagnostic) = err {
            if diagnostic.span.is_some() {
                return err;
            }
        }
        Error::diagnostic(Diagnostic::error(err.to_string()).with_span(span))
    }

    fn error_with_current_span(&self, message: impl Into<String>) -> Error {
        let message = message.into();
        if let Some(span) = self.current_span {
            Error::diagnostic(Diagnostic::error(message).with_span(span))
        } else {
            Error::from(message)
        }
    }

    #[allow(dead_code)]
    fn emit_warning(&mut self, message: impl Into<String>) {
        self.diagnostics.push(TypingDiagnostic::warning(message));
    }

    fn error_type_var(&mut self) -> TypeVarId {
        let var = self.fresh_type_var();
        self.bind(var, TypeTerm::Unknown);
        var
    }

    // unused: primitive_from_name (removed)

    fn expect_reference(&mut self, var: TypeVarId, context: &str) -> Result<TypeVarId> {
        let root = self.find(var);
        match self.type_vars[root].kind.clone() {
            TypeVarKind::Unbound { .. } => {
                let inner = self.fresh_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Reference(inner));
                Ok(inner)
            }
            TypeVarKind::Bound(TypeTerm::Reference(inner)) => Ok(inner),
            TypeVarKind::Link(next) => self.expect_reference(next, context),
            _other => {
                self.emit_error(format!("expected reference value for {}", context));
                let placeholder = self.error_type_var();
                self.type_vars[root].kind = TypeVarKind::Bound(TypeTerm::Reference(placeholder));
                Ok(placeholder)
            }
        }
    }

    fn generalize_symbol(&mut self, name: &str, var: TypeVarId) -> Result<()> {
        let scheme = self.generalize(var)?;
        self.replace_env_entry(name, EnvEntry::Poly(scheme));
        Ok(())
    }

    // ensure_numeric moved to typing::solver

    // ensure_bool moved to typing::solver

    // ensure_integer moved to typing::solver

    // ensure_function moved to typing::solver

    fn ty_from_function_signature(&mut self, sig: &FunctionSignature) -> Result<Ty> {
        self.validate_extern_c_signature(sig);
        let mut params = Vec::new();
        for param in &sig.params {
            params.push(param.ty.clone());
        }
        let ret_ty = sig.ret_ty.clone().unwrap_or_else(|| Ty::Unit(TypeUnit));
        Ok(Ty::Function(TypeFunction {
            params,
            generics_params: sig.generics_params.clone(),
            ret_ty: Some(Box::new(ret_ty)),
        }))
    }

    fn validate_extern_c_signature(&mut self, sig: &FunctionSignature) {
        if !matches!(sig.abi, Abi::C) {
            return;
        }
        for param in &sig.params {
            if self.is_disallowed_c_string_type(&param.ty) {
                self.emit_error(format!(
                    "extern \"C\" functions must use &CStr for string parameters: {}",
                    param.name
                ));
            }
        }
        if let Some(ret_ty) = &sig.ret_ty {
            if self.is_disallowed_c_string_type(ret_ty) {
                self.emit_error(
                    "extern \"C\" functions must use &CStr for string return types",
                );
            }
        }
    }

    fn is_disallowed_c_string_type(&self, ty: &Ty) -> bool {
        if self.is_cstr_reference(ty) {
            return false;
        }
        if self.is_string_like_type(ty) {
            return true;
        }
        if let Ty::Reference(reference) = ty {
            if self.is_string_like_type(reference.ty.as_ref()) {
                return true;
            }
        }
        false
    }

    fn is_cstr_reference(&self, ty: &Ty) -> bool {
        let Ty::Reference(reference) = ty else {
            return false;
        };
        self.type_name(reference.ty.as_ref()) == Some("CStr")
    }

    fn is_string_like_type(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Primitive(TypePrimitive::String) => true,
            _ => matches!(
                self.type_name(ty),
                Some("str") | Some("String") | Some("string")
            ),
        }
    }

    fn type_name<'a>(&self, ty: &'a Ty) -> Option<&'a str> {
        match ty {
            Ty::Struct(struct_ty) => Some(struct_ty.name.as_str()),
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Locator(locator) => match locator {
                    Locator::Ident(ident) => Some(ident.as_str()),
                    Locator::Path(path) => path.segments.last().map(|seg| seg.as_str()),
                    Locator::ParameterPath(path) => path.last().map(|seg| seg.ident.as_str()),
                },
                _ => None,
            },
            _ => None,
        }
    }

    fn struct_name_from_expr(&self, expr: &Expr) -> Option<String> {
        match expr.kind() {
            ExprKind::Locator(locator) => {
                let name = match locator {
                    Locator::ParameterPath(path) => path
                        .segments
                        .last()
                        .map(|seg| seg.ident.as_str().to_string())?,
                    Locator::Path(path) => {
                        path.segments.last().map(|seg| seg.as_str().to_string())?
                    }
                    Locator::Ident(ident) => ident.as_str().to_string(),
                };
                if name == "Self" {
                    self.impl_stack
                        .last()
                        .and_then(|ctx| ctx.as_ref())
                        .map(|ctx| ctx.struct_name.clone())
                } else {
                    Some(name)
                }
            }
            ExprKind::Value(value) => match &**value {
                Value::Type(Ty::Struct(struct_ty)) => Some(struct_ty.name.as_str().to_string()),
                Value::Type(Ty::Enum(enum_ty)) => Some(enum_ty.name.as_str().to_string()),
                _ => None,
            },
            _ => None,
        }
    }
}

fn tokenize_macro_tokens(tokens: &str) -> Vec<&str> {
    tokens.split_whitespace().collect()
}

fn is_ident_token(token: &str) -> bool {
    let mut chars = token.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first.is_ascii_alphabetic() || first == '_') {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn find_ident_after_keyword(tokens: &[&str], keyword: &str) -> Option<String> {
    let mut iter = tokens.iter().peekable();
    while let Some(token) = iter.next() {
        if *token == keyword {
            for next in iter.by_ref() {
                if is_ident_token(next) {
                    return Some(next.to_string());
                }
            }
            break;
        }
    }
    None
}

fn find_first_type_ident(tokens: &[&str]) -> Option<String> {
    for token in tokens {
        if is_ident_token(token) {
            if token
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
            {
                return Some((*token).to_string());
            }
        }
    }
    None
}

/// Infer the fragment kind for an unkinded quote based on its block shape.
/// - Single trailing expression and no statements => Expr
/// - All items at top level => Item
/// - Otherwise => Stmt
// moved to typing::infer_expr::infer_quote_kind

impl<'ctx> AstTypeInferencer<'ctx> {
    pub fn infer_expression(&mut self, expr: &mut Expr) -> Result<()> {
        let var = self.infer_expr(expr)?;
        let ty = self.resolve_to_ty(var)?;
        expr.set_ty(ty);
        Ok(())
    }

    pub fn push_scope(&mut self) {
        self.env.push(HashMap::new());
        self.generic_scopes.push(HashSet::new());
        self.current_level += 1;
    }

    pub fn pop_scope(&mut self) {
        self.env.pop();
        self.generic_scopes.pop();
        if self.current_level > 0 {
            self.current_level -= 1;
        }
    }

    pub fn bind_variable(&mut self, name: &str, ty: Ty) {
        let type_var = match self.type_from_ast_ty(&ty) {
            Ok(var) => var,
            Err(_) => self.fresh_type_var(),
        };
        if let Some(current_env) = self.env.last_mut() {
            current_env.insert(name.to_string(), EnvEntry::Mono(type_var));
        }
    }
}

pub fn annotate_with_prelude<I, S>(node: &mut Node, extern_prelude: I) -> Result<TypingOutcome>
where
    I: IntoIterator<Item = S>,
    S: Into<String>,
{
    let mut inferencer = AstTypeInferencer::new().with_extern_prelude(extern_prelude);
    inferencer.infer(node)
}

pub fn annotate(node: &mut Node) -> Result<TypingOutcome> {
    annotate_with_prelude(node, default_extern_prelude())
}
