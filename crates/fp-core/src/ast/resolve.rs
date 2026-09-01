//! AST-stage name resolution.
//!
//! This module owns the resolver data structures used between parsing/macro
//! expansion and AST→HIR lowering. HIR receives resolved identities; it does
//! not perform first-time lexical or module lookup.

use super::{ExprId, ItemId, Span};
use crate::ast::path::QualifiedPath;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    Type,
    Value,
    Macro,
}

/// The shared symbol representation used by all compiler stages.
pub use crate::hir::Symbol;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstRes {
    Module(QualifiedPath),
    Item(ItemId),
    /// A definition identity allocated during AST/package resolution. HIR
    /// lowering consumes this directly; it does not resolve the name again.
    Def(crate::hir::DefId),
    Local(u64),
    Parameter(u64),
    Generic(u64),
    Builtin(String),
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binding {
    Module {
        target: QualifiedPath,
        span: Span,
    },
    Definition {
        target: ItemId,
        namespace: Namespace,
        span: Span,
    },
    DefinitionId {
        target: crate::hir::DefId,
        namespace: Namespace,
        span: Span,
    },
    Import {
        target: AstRes,
        namespace: Namespace,
        span: Span,
    },
    Alias {
        target: ItemId,
        span: Span,
    },
    EnumVariant {
        enum_item: ItemId,
        variant: ItemId,
        span: Span,
    },
    AssociatedItem {
        owner: ItemId,
        item: ItemId,
        namespace: Namespace,
        span: Span,
    },
    ExternCrate {
        package: String,
        span: Span,
    },
    Builtin {
        name: String,
        namespace: Namespace,
    },
    Local {
        id: u64,
        namespace: Namespace,
        span: Span,
    },
    Parameter {
        id: u64,
        namespace: Namespace,
        span: Span,
    },
    Generic {
        id: u64,
        namespace: Namespace,
        span: Span,
    },
    Macro {
        id: ItemId,
        span: Span,
    },
    Error {
        namespace: Namespace,
        span: Span,
    },
}

impl Binding {
    pub fn namespace(&self) -> Namespace {
        match self {
            Self::Module { .. }
            | Self::Definition {
                namespace: Namespace::Type,
                ..
            }
            | Self::DefinitionId {
                namespace: Namespace::Type,
                ..
            }
            | Self::Alias { .. }
            | Self::EnumVariant { .. }
            | Self::AssociatedItem {
                namespace: Namespace::Type,
                ..
            }
            | Self::Generic {
                namespace: Namespace::Type,
                ..
            }
            | Self::Builtin {
                namespace: Namespace::Type,
                ..
            } => Namespace::Type,
            Self::Definition { namespace, .. }
            | Self::DefinitionId { namespace, .. }
            | Self::Import { namespace, .. }
            | Self::AssociatedItem { namespace, .. }
            | Self::Local { namespace, .. }
            | Self::Parameter { namespace, .. }
            | Self::Generic { namespace, .. }
            | Self::Builtin { namespace, .. }
            | Self::Error { namespace, .. } => *namespace,
            Self::ExternCrate { .. } => Namespace::Value,
            Self::Macro { .. } => Namespace::Macro,
        }
    }

    fn same_target(&self, other: &Self) -> bool {
        self == other
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DeclarationRules {
    pub allow_type_value_overlap: bool,
    pub allow_identical_imports: bool,
    pub glob_imports_are_weak: bool,
    pub register_struct_constructors: bool,
}

impl Default for DeclarationRules {
    fn default() -> Self {
        Self {
            allow_type_value_overlap: true,
            allow_identical_imports: true,
            glob_imports_are_weak: true,
            register_struct_constructors: true,
        }
    }
}

impl DeclarationRules {
    pub const fn rust() -> Self {
        Self {
            allow_type_value_overlap: true,
            allow_identical_imports: true,
            glob_imports_are_weak: true,
            register_struct_constructors: true,
        }
    }

    pub const fn ferro() -> Self {
        Self::rust()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ResolutionRules {
    pub explicit_import_beats_glob: bool,
    pub definition_beats_glob: bool,
    pub allow_parent_module_lookup: bool,
    pub use_extern_prelude: bool,
    pub use_language_prelude: bool,
    pub macro_ancestor_lookup: bool,
}

impl Default for ResolutionRules {
    fn default() -> Self {
        Self {
            explicit_import_beats_glob: true,
            definition_beats_glob: true,
            allow_parent_module_lookup: false,
            use_extern_prelude: true,
            use_language_prelude: true,
            macro_ancestor_lookup: true,
        }
    }
}

impl ResolutionRules {
    pub const fn rust() -> Self {
        Self {
            explicit_import_beats_glob: true,
            definition_beats_glob: true,
            allow_parent_module_lookup: false,
            use_extern_prelude: true,
            use_language_prelude: true,
            macro_ancestor_lookup: true,
        }
    }

    pub const fn ferro() -> Self {
        Self::rust()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeclarationOutcome {
    Inserted,
    IdenticalImport,
    Conflict,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolutionResult {
    Found(AstRes),
    Ambiguous,
    NotFound,
}

#[derive(Debug, Clone)]
pub struct ImportSpec {
    pub module: QualifiedPath,
    pub name: Symbol,
    pub target: QualifiedPath,
    pub namespace: Namespace,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LocalScopeId(u32);

#[derive(Debug, Clone)]
pub struct ModuleTree {
    pub symbols: HashMap<Symbol, Vec<Binding>>,
    children: HashMap<Symbol, ModuleTree>,
}

impl Default for ModuleTree {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleTree {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            children: HashMap::new(),
        }
    }

    pub fn ensure_module(&mut self, path: &QualifiedPath) -> &mut ModuleTree {
        let mut current = self;
        for segment in &path.segments {
            current = current
                .children
                .entry(Symbol::from(segment.as_str()))
                .or_default();
        }
        current
    }

    pub fn module(&self, path: &QualifiedPath) -> Option<&ModuleTree> {
        let mut current = self;
        for segment in &path.segments {
            current = current.children.get(&Symbol::from(segment.as_str()))?;
        }
        Some(current)
    }

    pub fn module_mut(&mut self, path: &QualifiedPath) -> Option<&mut ModuleTree> {
        let mut current = self;
        for segment in &path.segments {
            current = current.children.get_mut(&Symbol::from(segment.as_str()))?;
        }
        Some(current)
    }

    pub fn bindings(
        &self,
        module: &QualifiedPath,
    ) -> impl Iterator<Item = (&Symbol, &Vec<Binding>)> {
        self.module(module)
            .into_iter()
            .flat_map(|m| m.symbols.iter())
    }

    pub fn candidates(&self, module: &QualifiedPath, symbol: &str) -> Option<&[Binding]> {
        self.module(module)?
            .symbols
            .get(&Symbol::from(symbol))
            .map(Vec::as_slice)
    }

    pub fn declare(
        &mut self,
        module: &QualifiedPath,
        symbol: impl Into<Symbol>,
        binding: Binding,
        rules: DeclarationRules,
    ) -> DeclarationOutcome {
        let symbol = symbol.into();
        let entries = self
            .ensure_module(module)
            .symbols
            .entry(symbol)
            .or_default();
        if rules.allow_identical_imports && entries.iter().any(|old| old.same_target(&binding)) {
            return DeclarationOutcome::IdenticalImport;
        }
        if entries
            .iter()
            .any(|old| old.namespace() == binding.namespace())
        {
            entries.push(binding);
            return DeclarationOutcome::Conflict;
        }
        if !rules.allow_type_value_overlap
            && binding.namespace() != Namespace::Macro
            && entries
                .iter()
                .any(|old| old.namespace() != Namespace::Macro)
        {
            return DeclarationOutcome::Conflict;
        }
        entries.push(binding);
        DeclarationOutcome::Inserted
    }

    pub fn resolve(
        &self,
        module: &QualifiedPath,
        symbol: &str,
        namespace: Namespace,
        rules: ResolutionRules,
    ) -> ResolutionResult {
        let mut current = Some(module.clone());
        while let Some(path) = current {
            if let Some(entries) = self
                .module(&path)
                .and_then(|m| m.symbols.get(&Symbol::from(symbol)))
            {
                let matching: Vec<_> = entries
                    .iter()
                    .filter(|binding| binding.namespace() == namespace)
                    .collect();
                match matching.as_slice() {
                    [] => {}
                    [binding] => return ResolutionResult::Found(binding_to_res(binding)),
                    _ => return ResolutionResult::Ambiguous,
                }
            }
            current = rules
                .allow_parent_module_lookup
                .then(|| path.parent_n(1))
                .flatten();
        }
        ResolutionResult::NotFound
    }

    pub fn resolve_path(
        &self,
        module: &QualifiedPath,
        path: &QualifiedPath,
        namespace: Namespace,
        rules: ResolutionRules,
    ) -> ResolutionResult {
        let Some((first, rest)) = path.segments.split_first() else {
            return ResolutionResult::NotFound;
        };
        let mut result = self.resolve(module, first, namespace, rules);
        for segment in rest {
            let AstRes::Module(next) = (match result {
                ResolutionResult::Found(res) => res,
                _ => return ResolutionResult::NotFound,
            }) else {
                return ResolutionResult::NotFound;
            };
            result = self.resolve(&next, segment, namespace, rules);
        }
        result
    }

    /// Resolve a path for a value/type use and require a terminal semantic
    /// identity. Modules are valid only as intermediate path segments; a
    /// module (or an unresolved AST item placeholder) at the final position
    /// is reported as an error instead of leaking an intermediate result to
    /// lowering.
    pub fn resolve_path_final(
        &self,
        module: &QualifiedPath,
        path: &QualifiedPath,
        namespace: Namespace,
        rules: ResolutionRules,
    ) -> ResolutionResult {
        match self.resolve_path(module, path, namespace, rules) {
            ResolutionResult::Found(AstRes::Module(_))
            | ResolutionResult::Found(AstRes::Item(_)) => {
                ResolutionResult::Found(AstRes::Error)
            }
            result => result,
        }
    }
}

fn binding_to_res(binding: &Binding) -> AstRes {
    match binding {
        Binding::Module { target, .. } => AstRes::Module(target.clone()),
        Binding::Definition { target, .. } | Binding::Alias { target, .. } => AstRes::Item(*target),
        Binding::DefinitionId { target, .. } => AstRes::Def(target.clone()),
        Binding::Import { target, .. } => target.clone(),
        Binding::EnumVariant { variant, .. } | Binding::AssociatedItem { item: variant, .. } => {
            AstRes::Item(*variant)
        }
        Binding::ExternCrate { package, .. } => AstRes::Builtin(package.clone()),
        Binding::Builtin { name, .. } => AstRes::Builtin(name.clone()),
        Binding::Local { id, .. } => AstRes::Local(*id),
        Binding::Parameter { id, .. } => AstRes::Parameter(*id),
        Binding::Generic { id, .. } => AstRes::Generic(*id),
        Binding::Macro { id, .. } => AstRes::Item(*id),
        Binding::Error { .. } => AstRes::Error,
    }
}

#[derive(Debug, Clone)]
struct LocalNode {
    parent: Option<LocalScopeId>,
    symbols: HashMap<Symbol, Vec<Binding>>,
}

#[derive(Debug, Clone)]
pub struct LocalScope {
    nodes: Vec<LocalNode>,
    current: LocalScopeId,
}

impl Default for LocalScope {
    fn default() -> Self {
        Self::new()
    }
}

impl LocalScope {
    pub fn new() -> Self {
        let root = LocalNode {
            parent: None,
            symbols: HashMap::new(),
        };
        Self {
            nodes: vec![root],
            current: LocalScopeId(0),
        }
    }

    pub fn enter(&mut self) {
        let id = LocalScopeId(self.nodes.len() as u32);
        self.nodes.push(LocalNode {
            parent: Some(self.current),
            symbols: HashMap::new(),
        });
        self.current = id;
    }

    pub fn leave(&mut self) {
        if let Some(parent) = self.nodes[self.current.0 as usize].parent {
            self.current = parent;
        }
    }

    pub fn declare(
        &mut self,
        symbol: impl Into<Symbol>,
        binding: Binding,
        rules: DeclarationRules,
    ) -> DeclarationOutcome {
        let entries = self.nodes[self.current.0 as usize]
            .symbols
            .entry(symbol.into())
            .or_default();
        if entries
            .iter()
            .any(|old| old.namespace() == binding.namespace())
        {
            entries.push(binding);
            return DeclarationOutcome::Conflict;
        }
        let _ = rules;
        entries.push(binding);
        DeclarationOutcome::Inserted
    }

    pub fn resolve(
        &self,
        symbol: &str,
        namespace: Namespace,
        _rules: ResolutionRules,
    ) -> ResolutionResult {
        let mut current = Some(self.current);
        while let Some(id) = current {
            let node = &self.nodes[id.0 as usize];
            if let Some(entries) = node.symbols.get(&Symbol::from(symbol)) {
                let matching: Vec<_> = entries
                    .iter()
                    .filter(|binding| binding.namespace() == namespace)
                    .collect();
                match matching.as_slice() {
                    [] => {}
                    [binding] => return ResolutionResult::Found(binding_to_res(binding)),
                    _ => return ResolutionResult::Ambiguous,
                }
            }
            current = node.parent;
        }
        ResolutionResult::NotFound
    }
}

/// The explicit AST-stage resolver.  It owns temporary local scopes and the
/// package module tree, and is the only component that performs name lookup.
#[derive(Debug)]
pub struct AstResolver<'a> {
    package_id: Option<crate::hir::PackageId>,
    next_def_id: u32,
    item_def_ids: HashMap<ItemId, crate::hir::DefId>,
    pub modules: &'a mut ModuleTree,
    pub locals: LocalScope,
    pub declaration_rules: DeclarationRules,
    pub resolution_rules: ResolutionRules,
    resolutions: HashMap<ItemId, AstRes>,
    expr_resolutions: HashMap<ExprId, AstRes>,
}

impl<'a> AstResolver<'a> {
    pub fn from_provider(
        modules: &'a mut ModuleTree,
        provider: &dyn crate::ast::package::provider::PackageProvider,
    ) -> Self {
        Self::new(
            modules,
            provider.declaration_rules(),
            provider.resolution_rules(),
        )
    }

    pub fn new(
        modules: &'a mut ModuleTree,
        declaration_rules: DeclarationRules,
        resolution_rules: ResolutionRules,
    ) -> Self {
        Self {
            package_id: None,
            next_def_id: 1,
            item_def_ids: HashMap::new(),
            modules,
            locals: LocalScope::new(),
            declaration_rules,
            resolution_rules,
            resolutions: HashMap::new(),
            expr_resolutions: HashMap::new(),
        }
    }

    pub fn for_package(
        package_id: crate::hir::PackageId,
        modules: &'a mut ModuleTree,
        declaration_rules: DeclarationRules,
        resolution_rules: ResolutionRules,
    ) -> Self {
        let mut resolver = Self::new(modules, declaration_rules, resolution_rules);
        resolver.package_id = Some(package_id);
        resolver
    }

    fn item_def_id(&mut self, item: ItemId) -> crate::hir::DefId {
        if let Some(def_id) = self.item_def_ids.get(&item) {
            return def_id.clone();
        }
        let package_id = self
            .package_id
            .clone()
            .expect("package-aware resolver required for DefId allocation");
        let def_id = crate::hir::DefId::new(package_id, self.next_def_id);
        self.next_def_id += 1;
        self.item_def_ids.insert(item, def_id.clone());
        def_id
    }

    fn declare_item_definition(
        &mut self,
        module: &QualifiedPath,
        name: impl Into<Symbol>,
        item: ItemId,
        namespace: Namespace,
        span: Span,
    ) -> DeclarationOutcome {
        if self.package_id.is_some() {
            let def_id = self.item_def_id(item);
            self.declare_definition_id(module, name, def_id, namespace, span)
        } else {
            self.declare_definition(module, name, item, namespace, span)
        }
    }

    pub fn enter_scope(&mut self) {
        self.locals.enter();
    }
    pub fn leave_scope(&mut self) {
        self.locals.leave();
    }
    pub fn declare_local(
        &mut self,
        name: impl Into<Symbol>,
        binding: Binding,
    ) -> DeclarationOutcome {
        self.locals.declare(name, binding, self.declaration_rules)
    }
    pub fn declare_module(
        &mut self,
        module: &QualifiedPath,
        name: impl Into<Symbol>,
        binding: Binding,
    ) -> DeclarationOutcome {
        self.modules
            .declare(module, name, binding, self.declaration_rules)
    }

    pub fn declare_definition(
        &mut self,
        module: &QualifiedPath,
        name: impl Into<Symbol>,
        item: ItemId,
        namespace: Namespace,
        span: Span,
    ) -> DeclarationOutcome {
        self.declare_module(
            module,
            name,
            Binding::Definition {
                target: item,
                namespace,
                span,
            },
        )
    }

    /// Register a definition whose final identity was allocated during AST
    /// package construction. Consumers can carry the resulting `DefId`
    /// directly into HIR without a second name-resolution pass.
    pub fn declare_definition_id(
        &mut self,
        module: &QualifiedPath,
        name: impl Into<Symbol>,
        def_id: crate::hir::DefId,
        namespace: Namespace,
        span: Span,
    ) -> DeclarationOutcome {
        self.declare_module(
            module,
            name,
            Binding::DefinitionId {
                target: def_id,
                namespace,
                span,
            },
        )
    }

    pub fn declare_macro(
        &mut self,
        module: &QualifiedPath,
        name: impl Into<Symbol>,
        item: ItemId,
        span: Span,
    ) -> DeclarationOutcome {
        self.declare_module(module, name, Binding::Macro { id: item, span })
    }

    pub fn declare_import(
        &mut self,
        module: &QualifiedPath,
        name: impl Into<Symbol>,
        target: AstRes,
        namespace: Namespace,
        span: Span,
    ) -> DeclarationOutcome {
        self.declare_module(
            module,
            name,
            Binding::Import {
                target,
                namespace,
                span,
            },
        )
    }

    pub fn resolve(
        &self,
        module: &QualifiedPath,
        name: &str,
        namespace: Namespace,
    ) -> ResolutionResult {
        match self.locals.resolve(name, namespace, self.resolution_rules) {
            ResolutionResult::NotFound => {
                self.modules
                    .resolve(module, name, namespace, self.resolution_rules)
            }
            result => result,
        }
    }

    pub fn resolve_macro(&self, module: &QualifiedPath, name: &str) -> ResolutionResult {
        self.modules.resolve(
            module,
            name,
            Namespace::Macro,
            ResolutionRules {
                allow_parent_module_lookup: self.resolution_rules.macro_ancestor_lookup,
                ..self.resolution_rules
            },
        )
    }

    pub fn resolve_expr(
        &mut self,
        expr: ExprId,
        module: &QualifiedPath,
        name: &str,
        namespace: Namespace,
    ) -> ResolutionResult {
        let result = self.resolve(module, name, namespace);
        if let ResolutionResult::Found(res) = &result {
            self.record_expr_resolution(expr, res.clone());
        }
        result
    }

    /// Resolve a qualified path by resolving its first segment and traversing
    /// module nodes for the remaining segments.  No package-wide suffix scan
    /// is performed.
    pub fn resolve_path(
        &self,
        module: &QualifiedPath,
        path: &QualifiedPath,
        namespace: Namespace,
    ) -> ResolutionResult {
        let Some((first, rest)) = path.segments.split_first() else {
            return ResolutionResult::NotFound;
        };
        let mut result = self.resolve(module, first, Namespace::Type);
        if rest.is_empty() {
            result = self.resolve(module, first, namespace);
        }
        for segment in rest {
            let ResolutionResult::Found(AstRes::Module(next)) = result else {
                return ResolutionResult::NotFound;
            };
            result = self
                .modules
                .resolve(&next, segment, namespace, self.resolution_rules);
        }
        result
    }

    pub fn record_resolution(&mut self, id: ItemId, result: AstRes) {
        self.resolutions.insert(id, result);
    }
    pub fn resolution(&self, id: ItemId) -> Option<&AstRes> {
        self.resolutions.get(&id)
    }

    pub fn resolution_table(&self) -> &HashMap<ItemId, AstRes> {
        &self.resolutions
    }

    pub fn record_expr_resolution(&mut self, id: ExprId, result: AstRes) {
        self.expr_resolutions.insert(id, result);
    }

    pub fn expr_resolution(&self, id: ExprId) -> Option<&AstRes> {
        self.expr_resolutions.get(&id)
    }

    pub fn resolve_path_final(
        &self,
        module: &QualifiedPath,
        path: &QualifiedPath,
        namespace: Namespace,
    ) -> ResolutionResult {
        self.modules
            .resolve_path_final(module, path, namespace, self.resolution_rules)
    }

    pub fn expr_resolution_table(&self) -> &HashMap<ExprId, AstRes> {
        &self.expr_resolutions
    }

    /// Collect declarations from parsed package items. This is intentionally
    /// AST-only; HIR identities are assigned later by the lowering boundary.
    pub fn collect_package_items(&mut self, items: &[crate::ast::package::PackageItem]) {
        for package_item in items {
            let module = package_item.module_path.clone();
            self.modules.ensure_module(&module);
            self.collect_item(&module, &package_item.item);
        }
    }

    /// Resolve imports to a fixed point. The callback supplies the target
    /// lookup (and may itself observe bindings installed by earlier rounds).
    /// Keeping the worklist here makes import order independent and preserves
    /// ambiguity through the normal declaration operation.
    pub fn resolve_imports(
        &mut self,
        pending: &mut Vec<ImportSpec>,
        mut lookup: impl FnMut(&Self, &ImportSpec) -> Option<AstRes>,
    ) {
        loop {
            let mut progress = false;
            let mut unresolved = Vec::new();
            for import in pending.drain(..) {
                if let Some(target) = lookup(self, &import) {
                    self.declare_import(
                        &import.module,
                        import.name,
                        target,
                        import.namespace,
                        import.span,
                    );
                    progress = true;
                } else {
                    unresolved.push(import);
                }
            }
            *pending = unresolved;
            if !progress || pending.is_empty() {
                break;
            }
        }
    }

    fn collect_item(&mut self, module: &QualifiedPath, item: &super::Item) {
        use super::ItemKind;
        let span = item.span();
        let id = item.id();
        // Declarations are resolved identities too: recording the item here
        // lets downstream stages consume the AST result table without having
        // to reconstruct an identity from the syntax node.
        if self.package_id.is_none() {
            self.record_resolution(id, AstRes::Item(id));
        } else {
            let def_id = self.item_def_id(id);
            self.record_resolution(id, AstRes::Def(def_id));
        }
        match item.kind() {
            ItemKind::Module(child) => {
                let child_path = module.with_segment(child.name.name.clone());
                self.modules.ensure_module(&child_path);
                self.declare_module(
                    module,
                    &child.name,
                    Binding::Module {
                        target: child_path.clone(),
                        span,
                    },
                );
                for nested in &child.items {
                    self.collect_item(&child_path, nested);
                }
            }
            ItemKind::DefStruct(def) => {
                self.declare_item_definition(module, &def.name, id, Namespace::Type, span);
            }
            ItemKind::DefStructural(def) => {
                self.declare_item_definition(module, &def.name, id, Namespace::Type, span);
            }
            ItemKind::DefEnum(def) => {
                self.declare_item_definition(module, &def.name, id, Namespace::Type, span);
            }
            ItemKind::DefType(def) => {
                self.declare_item_definition(module, &def.name, id, Namespace::Type, span);
            }
            ItemKind::OpaqueType(def) => {
                self.declare_item_definition(module, &def.name, id, Namespace::Type, span);
            }
            ItemKind::DefTrait(def) => {
                self.declare_item_definition(module, &def.name, id, Namespace::Type, span);
            }
            ItemKind::DefConst(def) => {
                self.declare_item_definition(module, &def.name, id, Namespace::Value, span);
            }
            ItemKind::DefStatic(def) => {
                self.declare_item_definition(module, &def.name, id, Namespace::Value, span);
            }
            ItemKind::DefFunction(def) => {
                self.declare_item_definition(module, &def.name, id, Namespace::Value, span);
            }
            ItemKind::Macro(mac) => {
                if let Some(name) = mac.declared_name.as_ref() {
                    self.declare_macro(module, name, id, span);
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span() -> Span {
        Span::null()
    }

    #[test]
    fn shared_symbol_map_keeps_namespaces_distinct() {
        let mut tree = ModuleTree::new();
        let root = QualifiedPath::new(Vec::new());
        let rules = DeclarationRules::default();
        assert_eq!(
            tree.declare(
                &root,
                "Thing",
                Binding::Definition {
                    target: 1,
                    namespace: Namespace::Type,
                    span: span()
                },
                rules
            ),
            DeclarationOutcome::Inserted
        );
        assert_eq!(
            tree.declare(
                &root,
                "Thing",
                Binding::Definition {
                    target: 2,
                    namespace: Namespace::Value,
                    span: span()
                },
                rules
            ),
            DeclarationOutcome::Inserted
        );
        assert!(matches!(
            tree.resolve(&root, "Thing", Namespace::Type, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Item(1))
        ));
        assert!(matches!(
            tree.resolve(&root, "Thing", Namespace::Value, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Item(2))
        ));
    }

    #[test]
    fn locals_shadow_module_bindings() {
        let mut modules = ModuleTree::new();
        let root = QualifiedPath::new(Vec::new());
        let mut resolver = AstResolver::new(
            &mut modules,
            DeclarationRules::default(),
            ResolutionRules::default(),
        );
        resolver.declare_module(
            &root,
            "x",
            Binding::Definition {
                target: 1,
                namespace: Namespace::Value,
                span: span(),
            },
        );
        resolver.enter_scope();
        resolver.declare_local(
            "x",
            Binding::Local {
                id: 9,
                namespace: Namespace::Value,
                span: span(),
            },
        );
        assert_eq!(
            resolver.resolve(&root, "x", Namespace::Value),
            ResolutionResult::Found(AstRes::Local(9))
        );
    }

    #[test]
    fn conflicting_bindings_are_ambiguous() {
        let mut tree = ModuleTree::new();
        let root = QualifiedPath::new(Vec::new());
        let rules = DeclarationRules::default();
        tree.declare(
            &root,
            "x",
            Binding::Definition {
                target: 1,
                namespace: Namespace::Value,
                span: span(),
            },
            rules,
        );
        assert_eq!(
            tree.declare(
                &root,
                "x",
                Binding::Definition {
                    target: 2,
                    namespace: Namespace::Value,
                    span: span()
                },
                rules
            ),
            DeclarationOutcome::Conflict
        );
        assert_eq!(
            tree.resolve(&root, "x", Namespace::Value, ResolutionRules::default()),
            ResolutionResult::Ambiguous
        );
    }

    #[test]
    fn nested_modules_resolve_qualified_paths() {
        let mut tree = ModuleTree::new();
        let root = QualifiedPath::new(Vec::new());
        let nested = QualifiedPath::new(vec!["m".into()]);
        tree.ensure_module(&nested);
        tree.declare(
            &root,
            "m",
            Binding::Module {
                target: nested.clone(),
                span: span(),
            },
            DeclarationRules::default(),
        );
        tree.declare(
            &nested,
            "Thing",
            Binding::Definition {
                target: 42,
                namespace: Namespace::Type,
                span: span(),
            },
            DeclarationRules::default(),
        );
        let resolver = AstResolver::new(
            &mut tree,
            DeclarationRules::default(),
            ResolutionRules::default(),
        );
        assert_eq!(
            resolver.resolve_path(
                &root,
                &QualifiedPath::new(vec!["m".into(), "Thing".into()]),
                Namespace::Type,
            ),
            ResolutionResult::Found(AstRes::Item(42))
        );
    }

    #[test]
    fn parent_module_lookup_is_policy_controlled() {
        let mut tree = ModuleTree::new();
        let root = QualifiedPath::new(Vec::new());
        let child = QualifiedPath::new(vec!["child".into()]);
        tree.declare(
            &root,
            "x",
            Binding::Definition {
                target: 7,
                namespace: Namespace::Value,
                span: span(),
            },
            DeclarationRules::default(),
        );
        let no_parent = ResolutionRules {
            allow_parent_module_lookup: false,
            ..ResolutionRules::default()
        };
        assert_eq!(
            tree.resolve(&child, "x", Namespace::Value, no_parent),
            ResolutionResult::NotFound
        );
        let with_parent = ResolutionRules {
            allow_parent_module_lookup: true,
            ..ResolutionRules::default()
        };
        assert_eq!(
            tree.resolve(&child, "x", Namespace::Value, with_parent),
            ResolutionResult::Found(AstRes::Item(7))
        );
    }

    #[test]
    fn macro_and_value_bindings_share_keys_but_not_namespace() {
        let mut tree = ModuleTree::new();
        let root = QualifiedPath::new(Vec::new());
        let rules = DeclarationRules::default();
        tree.declare(
            &root,
            "log",
            Binding::Definition {
                target: 1,
                namespace: Namespace::Value,
                span: span(),
            },
            rules,
        );
        tree.declare(
            &root,
            "log",
            Binding::Macro {
                id: 2,
                span: span(),
            },
            rules,
        );
        assert_eq!(
            tree.resolve(&root, "log", Namespace::Value, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Item(1))
        );
        assert_eq!(
            tree.resolve(&root, "log", Namespace::Macro, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Item(2))
        );
    }

    fn root() -> QualifiedPath {
        QualifiedPath::new(Vec::new())
    }

    // Rust equivalents covered by the cases below:
    //
    // ```rust
    // mod m { pub struct Thing; }                 // nested qualified path
    // fn f() { let x = 1; }                       // lexical local
    // struct x; const x: i32 = 0;                 // type/value namespaces
    // macro_rules! log { () => {} }               // macro namespace
    // use crate::m::Thing as Alias;               // alias/import target
    // enum E { Some }                             // enum variant
    // impl E { const CONST: i32 = 0; }            // associated item
    // extern crate dep;                           // extern prelude binding
    // type T = i32;                               // generic/type lookup shape
    // fn f<T>(arg: T) { let _ = arg; }             // generic + parameter scopes
    // mod child { super::x; }                     // parent lookup policy
    // fn missing() { unknown_name(); }             // unresolved name
    // fn ambiguous() { use a::*; use b::*; x; }    // ambiguity retention
    // macro_rules! m { () => {} }                 // macro/value separation
    // type Bad = Missing;                         // error propagation
    // mod a { mod b {} }                          // idempotent module creation
    // use crate::m::x;                             // module-local bindings
    // fn path() { m::Thing; }                      // qualified path traversal
    // fn value() { f(); }                          // single-segment value path
    // ```

    fn definition(id: u64, namespace: Namespace) -> Binding {
        Binding::Definition {
            target: id,
            namespace,
            span: span(),
        }
    }

    #[test]
    fn missing_name_is_not_found() {
        let tree = ModuleTree::new();
        assert_eq!(
            tree.resolve(
                &root(),
                "missing",
                Namespace::Value,
                ResolutionRules::default()
            ),
            ResolutionResult::NotFound
        );
    }

    #[test]
    fn same_namespace_definitions_are_ambiguous() {
        let mut tree = ModuleTree::new();
        let path = root();
        tree.declare(
            &path,
            "x",
            definition(1, Namespace::Value),
            DeclarationRules::default(),
        );
        tree.declare(
            &path,
            "x",
            definition(2, Namespace::Value),
            DeclarationRules::default(),
        );
        assert_eq!(
            tree.resolve(&path, "x", Namespace::Value, ResolutionRules::default()),
            ResolutionResult::Ambiguous
        );
    }

    #[test]
    fn type_and_value_names_can_coexist() {
        let mut tree = ModuleTree::new();
        let path = root();
        tree.declare(
            &path,
            "x",
            definition(1, Namespace::Type),
            DeclarationRules::default(),
        );
        tree.declare(
            &path,
            "x",
            definition(2, Namespace::Value),
            DeclarationRules::default(),
        );
        assert!(matches!(
            tree.resolve(&path, "x", Namespace::Type, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Item(1))
        ));
        assert!(matches!(
            tree.resolve(&path, "x", Namespace::Value, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Item(2))
        ));
    }

    #[test]
    fn identical_binding_is_idempotent() {
        let mut tree = ModuleTree::new();
        let path = root();
        let binding = definition(1, Namespace::Type);
        assert_eq!(
            tree.declare(&path, "x", binding.clone(), DeclarationRules::default()),
            DeclarationOutcome::Inserted
        );
        assert_eq!(
            tree.declare(&path, "x", binding, DeclarationRules::default()),
            DeclarationOutcome::IdenticalImport
        );
    }

    #[test]
    fn local_inner_scope_shadows_outer_scope() {
        let mut modules = ModuleTree::new();
        let mut resolver = AstResolver::new(
            &mut modules,
            DeclarationRules::default(),
            ResolutionRules::default(),
        );
        let path = root();
        resolver.declare_local(
            "x",
            Binding::Local {
                id: 1,
                namespace: Namespace::Value,
                span: span(),
            },
        );
        resolver.enter_scope();
        resolver.declare_local(
            "x",
            Binding::Local {
                id: 2,
                namespace: Namespace::Value,
                span: span(),
            },
        );
        assert_eq!(
            resolver.resolve(&path, "x", Namespace::Value),
            ResolutionResult::Found(AstRes::Local(2))
        );
    }

    #[test]
    fn leaving_scope_restores_outer_binding() {
        let mut modules = ModuleTree::new();
        let mut resolver = AstResolver::new(
            &mut modules,
            DeclarationRules::default(),
            ResolutionRules::default(),
        );
        let path = root();
        resolver.declare_local(
            "x",
            Binding::Local {
                id: 1,
                namespace: Namespace::Value,
                span: span(),
            },
        );
        resolver.enter_scope();
        resolver.declare_local(
            "x",
            Binding::Local {
                id: 2,
                namespace: Namespace::Value,
                span: span(),
            },
        );
        resolver.leave_scope();
        assert_eq!(
            resolver.resolve(&path, "x", Namespace::Value),
            ResolutionResult::Found(AstRes::Local(1))
        );
    }

    #[test]
    fn parameter_binding_resolves() {
        let mut modules = ModuleTree::new();
        let mut resolver = AstResolver::new(
            &mut modules,
            DeclarationRules::default(),
            ResolutionRules::default(),
        );
        resolver.declare_local(
            "arg",
            Binding::Parameter {
                id: 3,
                namespace: Namespace::Value,
                span: span(),
            },
        );
        assert_eq!(
            resolver.resolve(&root(), "arg", Namespace::Value),
            ResolutionResult::Found(AstRes::Parameter(3))
        );
    }

    #[test]
    fn generic_type_binding_resolves() {
        let mut modules = ModuleTree::new();
        let mut resolver = AstResolver::new(
            &mut modules,
            DeclarationRules::default(),
            ResolutionRules::default(),
        );
        resolver.declare_local(
            "T",
            Binding::Generic {
                id: 4,
                namespace: Namespace::Type,
                span: span(),
            },
        );
        assert_eq!(
            resolver.resolve(&root(), "T", Namespace::Type),
            ResolutionResult::Found(AstRes::Generic(4))
        );
    }

    #[test]
    fn builtin_binding_resolves() {
        let mut tree = ModuleTree::new();
        let path = root();
        tree.declare(
            &path,
            "i32",
            Binding::Builtin {
                name: "i32".into(),
                namespace: Namespace::Type,
            },
            DeclarationRules::default(),
        );
        assert_eq!(
            tree.resolve(&path, "i32", Namespace::Type, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Builtin("i32".into()))
        );
    }

    #[test]
    fn alias_binding_resolves_to_item() {
        let mut tree = ModuleTree::new();
        let path = root();
        tree.declare(
            &path,
            "Alias",
            Binding::Alias {
                target: 5,
                span: span(),
            },
            DeclarationRules::default(),
        );
        assert_eq!(
            tree.resolve(&path, "Alias", Namespace::Type, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Item(5))
        );
    }

    #[test]
    fn enum_variant_binding_resolves_to_variant_item() {
        let mut tree = ModuleTree::new();
        let path = root();
        tree.declare(
            &path,
            "Some",
            Binding::EnumVariant {
                enum_item: 6,
                variant: 7,
                span: span(),
            },
            DeclarationRules::default(),
        );
        assert_eq!(
            tree.resolve(&path, "Some", Namespace::Type, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Item(7))
        );
    }

    #[test]
    fn associated_value_binding_resolves() {
        let mut tree = ModuleTree::new();
        let path = root();
        tree.declare(
            &path,
            "CONST",
            Binding::AssociatedItem {
                owner: 8,
                item: 9,
                namespace: Namespace::Value,
                span: span(),
            },
            DeclarationRules::default(),
        );
        assert_eq!(
            tree.resolve(&path, "CONST", Namespace::Value, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Item(9))
        );
    }

    #[test]
    fn extern_crate_binding_resolves_as_builtin() {
        let mut tree = ModuleTree::new();
        let path = root();
        tree.declare(
            &path,
            "dep",
            Binding::ExternCrate {
                package: "dep".into(),
                span: span(),
            },
            DeclarationRules::default(),
        );
        assert_eq!(
            tree.resolve(&path, "dep", Namespace::Value, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Builtin("dep".into()))
        );
    }

    #[test]
    fn error_binding_propagates_error_result() {
        let mut tree = ModuleTree::new();
        let path = root();
        tree.declare(
            &path,
            "bad",
            Binding::Error {
                namespace: Namespace::Type,
                span: span(),
            },
            DeclarationRules::default(),
        );
        assert_eq!(
            tree.resolve(&path, "bad", Namespace::Type, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Error)
        );
    }

    #[test]
    fn macro_ancestor_lookup_can_be_enabled() {
        let mut tree = ModuleTree::new();
        let parent = root();
        let child = QualifiedPath::new(vec!["child".into()]);
        tree.declare(
            &parent,
            "m",
            Binding::Macro {
                id: 10,
                span: span(),
            },
            DeclarationRules::default(),
        );
        let mut resolver = AstResolver::new(
            &mut tree,
            DeclarationRules::default(),
            ResolutionRules {
                macro_ancestor_lookup: true,
                ..ResolutionRules::default()
            },
        );
        assert_eq!(
            resolver.resolve_macro(&child, "m"),
            ResolutionResult::Found(AstRes::Item(10))
        );
    }

    #[test]
    fn macro_lookup_does_not_return_value_binding() {
        let mut tree = ModuleTree::new();
        let path = root();
        tree.declare(
            &path,
            "m",
            definition(11, Namespace::Value),
            DeclarationRules::default(),
        );
        assert_eq!(
            tree.resolve(&path, "m", Namespace::Macro, ResolutionRules::default()),
            ResolutionResult::NotFound
        );
    }

    #[test]
    fn resolved_definition_identity_is_preserved_for_hir() {
        let mut tree = ModuleTree::new();
        let path = root();
        let def_id = crate::hir::DefId::new(crate::package::PackageId::new("pkg"), 21);
        tree.declare(
            &path,
            "Thing",
            Binding::DefinitionId {
                target: def_id.clone(),
                namespace: Namespace::Type,
                span: span(),
            },
            DeclarationRules::default(),
        );
        assert_eq!(
            tree.resolve(&path, "Thing", Namespace::Type, ResolutionRules::default()),
            ResolutionResult::Found(AstRes::Def(def_id))
        );
    }

    #[test]
    fn single_segment_path_uses_requested_namespace() {
        let mut tree = ModuleTree::new();
        let path = root();
        tree.declare(
            &path,
            "f",
            definition(12, Namespace::Value),
            DeclarationRules::default(),
        );
        let resolver = AstResolver::new(
            &mut tree,
            DeclarationRules::default(),
            ResolutionRules::default(),
        );
        assert_eq!(
            resolver.resolve_path(
                &path,
                &QualifiedPath::new(vec!["f".into()]),
                Namespace::Value
            ),
            ResolutionResult::Found(AstRes::Item(12))
        );
    }

    #[test]
    fn qualified_path_with_missing_module_is_not_found() {
        let mut tree = ModuleTree::new();
        let path = root();
        let resolver = AstResolver::new(
            &mut tree,
            DeclarationRules::default(),
            ResolutionRules::default(),
        );
        assert_eq!(
            resolver.resolve_path(
                &path,
                &QualifiedPath::new(vec!["missing".into(), "Thing".into()]),
                Namespace::Type
            ),
            ResolutionResult::NotFound
        );
    }

    #[test]
    fn ensure_module_is_idempotent() {
        let mut tree = ModuleTree::new();
        let path = QualifiedPath::new(vec!["a".into(), "b".into()]);
        tree.ensure_module(&path)
            .symbols
            .insert(Symbol::from("x"), vec![definition(13, Namespace::Value)]);
        assert!(tree
            .module(&path)
            .unwrap()
            .symbols
            .contains_key(&Symbol::from("x")));
        assert!(tree.module(&path).is_some());
    }

    #[test]
    fn bindings_iterates_only_selected_module() {
        let mut tree = ModuleTree::new();
        let path = QualifiedPath::new(vec!["m".into()]);
        tree.declare(
            &path,
            "x",
            definition(14, Namespace::Value),
            DeclarationRules::default(),
        );
        let names: Vec<_> = tree.bindings(&path).map(|(name, _)| name.clone()).collect();
        assert_eq!(names, vec![Symbol::from("x")]);
    }
}
