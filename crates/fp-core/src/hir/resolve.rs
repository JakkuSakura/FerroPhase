//! AST-stage name resolution.
//!
//! This module owns the resolver data structures used between parsing/macro
//! expansion and AST→HIR lowering. HIR receives resolved identities; it does
//! not perform first-time lexical or module lookup.

use crate::ast::path::QualifiedPath;
use crate::span::Span;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    Type,
    Value,
    Macro,
}

/// The shared symbol representation used by all compiler stages.
pub use crate::hir::Symbol;

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    Module {
        target: QualifiedPath,
        def_id: crate::hir::DefId,
        span: Span,
    },
    Definition {
        target: crate::hir::DefId,
        namespace: Namespace,
        span: Span,
    },
    Import {
        target: crate::hir::Res,
        namespace: Namespace,
        span: Span,
    },
    Alias {
        target: crate::hir::DefId,
        span: Span,
    },
    EnumVariant {
        enum_item: crate::hir::DefId,
        variant: crate::hir::DefId,
        span: Span,
    },
    AssociatedItem {
        owner: crate::hir::DefId,
        item: crate::hir::DefId,
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
        id: crate::hir::HirId,
        namespace: Namespace,
        span: Span,
    },
    Parameter {
        id: crate::hir::HirId,
        namespace: Namespace,
        span: Span,
    },
    Generic {
        id: crate::hir::DefId,
        namespace: Namespace,
        span: Span,
    },
    Macro {
        id: crate::hir::DefId,
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

#[derive(Debug, Clone, PartialEq)]
pub enum ResolutionResult {
    Found(crate::hir::Res),
    Ambiguous,
    NotFound,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LocalScopeId(u32);

#[derive(Debug, Clone, PartialEq)]
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

    /// Find the source path associated with a module definition.  Module
    /// paths are an implementation detail of traversal; the public
    /// resolution result carries the module's `DefId`.
    pub fn path_for_module(&self, def_id: &crate::hir::DefId) -> Option<QualifiedPath> {
        fn visit(tree: &ModuleTree, def_id: &crate::hir::DefId) -> Option<QualifiedPath> {
            for bindings in tree.symbols.values() {
                for binding in bindings {
                    if let Binding::Module {
                        def_id: id, target, ..
                    } = binding
                    {
                        if id == def_id {
                            return Some(target.clone());
                        }
                    }
                }
            }
            for child in tree.children.values() {
                if let Some(path) = visit(child, def_id) {
                    return Some(path);
                }
            }
            None
        }
        visit(self, def_id)
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
            let crate::hir::Res::Module(next) = (match result {
                ResolutionResult::Found(res) => res,
                _ => return ResolutionResult::NotFound,
            }) else {
                return ResolutionResult::NotFound;
            };
            let Some(next_path) = self.path_for_module(&next) else {
                return ResolutionResult::NotFound;
            };
            result = self.resolve(&next_path, segment, namespace, rules);
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
            ResolutionResult::Found(crate::hir::Res::Module(_)) => {
                ResolutionResult::Found(crate::hir::Res::Error)
            }
            result => result,
        }
    }
}

fn binding_to_res(binding: &Binding) -> crate::hir::Res {
    match binding {
        Binding::Module { def_id, .. } => crate::hir::Res::Module(def_id.clone()),
        Binding::Definition { target, .. } | Binding::Alias { target, .. } => {
            crate::hir::Res::Def(target.clone())
        }
        Binding::Import { target, .. } => target.clone(),
        Binding::EnumVariant { variant, .. } | Binding::AssociatedItem { item: variant, .. } => {
            crate::hir::Res::Def(variant.clone())
        }
        Binding::ExternCrate { package, .. } => crate::hir::Res::BuiltinName(package.clone()),
        Binding::Builtin { name, .. } => crate::hir::Res::BuiltinName(name.clone()),
        Binding::Local { id, .. } => crate::hir::Res::Local(id.clone()),
        Binding::Parameter { id, .. } => crate::hir::Res::Parameter(id.clone()),
        Binding::Generic { id, .. } => crate::hir::Res::Generic(id.clone()),
        Binding::Macro { id, .. } => crate::hir::Res::Def(id.clone()),
        Binding::Error { .. } => crate::hir::Res::Error,
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

#[cfg(test)]
mod tests {
    use super::*;

    fn span() -> Span {
        Span::null()
    }

    fn def(id: u32, namespace: Namespace) -> Binding {
        Binding::Definition {
            target: crate::hir::DefId::local(id),
            namespace,
            span: span(),
        }
    }

    #[test]
    fn shared_symbol_map_keeps_namespaces_distinct() {
        let mut tree = ModuleTree::new();
        let root = QualifiedPath::new(Vec::new());
        assert_eq!(
            tree.declare(
                &root,
                "Thing",
                def(1, Namespace::Type),
                DeclarationRules::rust()
            ),
            DeclarationOutcome::Inserted
        );
        assert_eq!(
            tree.declare(
                &root,
                "Thing",
                def(2, Namespace::Value),
                DeclarationRules::rust()
            ),
            DeclarationOutcome::Inserted
        );
        assert!(matches!(
            tree.resolve(&root, "Thing", Namespace::Type, ResolutionRules::rust()),
            ResolutionResult::Found(crate::hir::Res::Def(id)) if id == crate::hir::DefId::local(1)
        ));
        assert!(matches!(
            tree.resolve(&root, "Thing", Namespace::Value, ResolutionRules::rust()),
            ResolutionResult::Found(crate::hir::Res::Def(id)) if id == crate::hir::DefId::local(2)
        ));
    }

    #[test]
    fn conflicting_bindings_are_ambiguous() {
        let mut tree = ModuleTree::new();
        let root = QualifiedPath::new(Vec::new());
        tree.declare(
            &root,
            "x",
            def(1, Namespace::Value),
            DeclarationRules::rust(),
        );
        assert_eq!(
            tree.declare(
                &root,
                "x",
                def(2, Namespace::Value),
                DeclarationRules::rust()
            ),
            DeclarationOutcome::Conflict
        );
        assert_eq!(
            tree.resolve(&root, "x", Namespace::Value, ResolutionRules::rust()),
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
                def_id: crate::hir::DefId::local(7),
                span: span(),
            },
            DeclarationRules::rust(),
        );
        tree.declare(
            &nested,
            "Thing",
            def(42, Namespace::Type),
            DeclarationRules::rust(),
        );
        assert!(matches!(
            tree.resolve_path(
                &root,
                &QualifiedPath::new(vec!["m".into(), "Thing".into()]),
                Namespace::Type,
                ResolutionRules::rust(),
            ),
            ResolutionResult::Found(crate::hir::Res::Def(id)) if id == crate::hir::DefId::local(42)
        ));
    }

    #[test]
    fn parent_module_lookup_is_policy_controlled() {
        let mut tree = ModuleTree::new();
        let root = QualifiedPath::new(Vec::new());
        let child = QualifiedPath::new(vec!["child".into()]);
        tree.declare(
            &root,
            "x",
            def(7, Namespace::Value),
            DeclarationRules::rust(),
        );
        let no_parent = ResolutionRules {
            allow_parent_module_lookup: false,
            ..ResolutionRules::rust()
        };
        assert_eq!(
            tree.resolve(&child, "x", Namespace::Value, no_parent),
            ResolutionResult::NotFound
        );
        let with_parent = ResolutionRules {
            allow_parent_module_lookup: true,
            ..ResolutionRules::rust()
        };
        assert!(matches!(
            tree.resolve(&child, "x", Namespace::Value, with_parent),
            ResolutionResult::Found(crate::hir::Res::Def(id)) if id == crate::hir::DefId::local(7)
        ));
    }

    #[test]
    fn macro_and_value_bindings_use_separate_namespaces() {
        let mut tree = ModuleTree::new();
        let root = QualifiedPath::new(Vec::new());
        tree.declare(
            &root,
            "log",
            def(1, Namespace::Value),
            DeclarationRules::rust(),
        );
        tree.declare(
            &root,
            "log",
            Binding::Macro {
                id: crate::hir::DefId::local(2),
                span: span(),
            },
            DeclarationRules::rust(),
        );
        assert!(matches!(
            tree.resolve(&root, "log", Namespace::Value, ResolutionRules::rust()),
            ResolutionResult::Found(crate::hir::Res::Def(id)) if id == crate::hir::DefId::local(1)
        ));
        assert!(matches!(
            tree.resolve(&root, "log", Namespace::Macro, ResolutionRules::rust()),
            ResolutionResult::Found(crate::hir::Res::Def(id)) if id == crate::hir::DefId::local(2)
        ));
    }
}
