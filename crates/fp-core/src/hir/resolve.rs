//! AST-stage name resolution.
//!
//! This module owns the resolver data structures used between parsing/macro
//! expansion and AST→HIR lowering. HIR receives resolved identities; it does
//! not perform first-time lexical or module lookup.

use crate::ast::path::InPackagePath;
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

/// Identity-based module namespace data. Each module definition owns its
/// direct named children; no source/module path is required for traversal.
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleData {
    children: HashMap<crate::hir::DefId, Vec<(Symbol, Namespace, crate::hir::Res)>>,
}

impl Default for ModuleData {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleData {
    pub fn new() -> Self {
        let mut data = Self {
            children: HashMap::new(),
        };
        data.children.insert(Self::virtual_root(), Vec::new());
        data
    }

    pub fn virtual_root() -> crate::hir::DefId {
        crate::hir::DefId::local(0)
    }

    pub fn virtual_root_for(package_id: crate::ast::package::PackageId) -> crate::hir::DefId {
        crate::hir::DefId::new(package_id, 0)
    }

    pub fn children(
        &self,
        module: &crate::hir::DefId,
    ) -> Option<&[(Symbol, Namespace, crate::hir::Res)]> {
        self.children.get(module).map(Vec::as_slice)
    }

    pub fn module_ids(&self) -> impl Iterator<Item = &crate::hir::DefId> {
        self.children.keys()
    }

    pub fn set_children(
        &mut self,
        module: crate::hir::DefId,
        children: Vec<(Symbol, Namespace, crate::hir::Res)>,
    ) {
        self.children.insert(module, children);
    }

    pub fn add_child(
        &mut self,
        module: crate::hir::DefId,
        name: impl Into<Symbol>,
        namespace: Namespace,
        resolution: crate::hir::Res,
    ) {
        self.children
            .entry(module)
            .or_default()
            .push((name.into(), namespace, resolution));
    }

    pub fn copy_children(&mut self, source: &crate::hir::DefId, destination: &crate::hir::DefId) {
        let entries = self
            .children(source)
            .map(|children| children.to_vec())
            .unwrap_or_default();
        self.children
            .entry(destination.clone())
            .or_default()
            .extend(entries);
    }

    pub fn resolve_child(
        &self,
        module: &crate::hir::DefId,
        name: &str,
        namespace: Namespace,
    ) -> ResolutionResult {
        let Some(children) = self.children(module) else {
            return ResolutionResult::NotFound(ResolutionNotFound::ModuleDefinition(
                module.clone(),
            ));
        };
        let matches: Vec<_> = children
            .iter()
            .filter(|(symbol, child_namespace, _)| {
                symbol.as_str() == name && *child_namespace == namespace
            })
            .map(|(_, _, resolution)| resolution.clone())
            .collect();
        match matches.as_slice() {
            [resolution] => ResolutionResult::Found(crate::hir::Path {
                res: resolution.clone(),
                segments: vec![crate::hir::PathSegment {
                    name: name.into(),
                    args: None,
                    infer_args: true,
                    res: resolution.clone(),
                }],
            }),
            [] => ResolutionResult::NotFound(ResolutionNotFound::Symbol {
                module: InPackagePath::new(Vec::new()),
                symbol: Symbol::from(name),
                namespace,
            }),
            _ => ResolutionResult::Ambiguous,
        }
    }

    pub fn resolve_module(
        &self,
        root: &crate::hir::DefId,
        path: &[String],
        namespace: Namespace,
    ) -> ResolutionResult {
        let Some((last, parents)) = path.split_last() else {
            return ResolutionResult::NotFound(ResolutionNotFound::EmptyPath);
        };
        let mut module = root.clone();
        for segment in parents {
            let Some(next) = self.children(&module).and_then(|children| {
                children.iter().find_map(|(name, _, resolution)| {
                    (name.as_str() == segment && matches!(resolution, crate::hir::Res::Module(_)))
                        .then(|| resolution.clone())
                })
            }) else {
                return ResolutionResult::NotFound(ResolutionNotFound::Symbol {
                    module: InPackagePath::new(parents.to_vec()),
                    symbol: Symbol::from(last.as_str()),
                    namespace,
                });
            };
            let crate::hir::Res::Module(next) = next else {
                unreachable!();
            };
            module = next;
        }
        let Some(children) = self.children(&module) else {
            return ResolutionResult::NotFound(ResolutionNotFound::ModuleDefinition(module));
        };
        let matches: Vec<_> = children
            .iter()
            .filter(|(name, child_namespace, _)| {
                name.as_str() == last && *child_namespace == namespace
            })
            .map(|(_, _, resolution)| resolution.clone())
            .collect();
        match matches.as_slice() {
            [resolution] => ResolutionResult::Found(crate::hir::Path {
                res: resolution.clone(),
                segments: vec![crate::hir::PathSegment {
                    name: last.as_str().into(),
                    args: None,
                    infer_args: true,
                    res: resolution.clone(),
                }],
            }),
            [] => ResolutionResult::NotFound(ResolutionNotFound::Symbol {
                module: InPackagePath::new(parents.to_vec()),
                symbol: Symbol::from(last.as_str()),
                namespace,
            }),
            _ => ResolutionResult::Ambiguous,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    Module {
        target: InPackagePath,
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

impl ModuleData {
    pub fn declare(
        &mut self,
        module: &crate::hir::DefId,
        name: impl Into<Symbol>,
        binding: Binding,
        _rules: DeclarationRules,
    ) -> DeclarationOutcome {
        let name: Symbol = name.into();
        let namespace = binding.namespace();
        let resolution = binding_to_res(&binding);
        let entries = self.children.entry(module.clone()).or_default();
        if entries
            .iter()
            .any(|(symbol, child_namespace, _)| *child_namespace == namespace && *symbol == name)
        {
            return DeclarationOutcome::Conflict;
        }
        entries.push((name, namespace, resolution));
        DeclarationOutcome::Inserted
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
    Found(crate::hir::Path),
    Ambiguous,
    NotFound(ResolutionNotFound),
}

impl ResolutionResult {
    pub fn is_not_found(&self) -> bool {
        matches!(self, Self::NotFound(_))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResolutionNotFound {
    EmptyPath,
    Package(crate::ast::package::PackageId),
    Symbol {
        module: InPackagePath,
        symbol: Symbol,
        namespace: Namespace,
    },
    Local {
        symbol: Symbol,
        namespace: Namespace,
    },
    ExpectedModule {
        path: InPackagePath,
        found: crate::hir::Res,
    },
    ModuleDefinition(crate::hir::DefId),
    InvalidParent {
        location: InPackagePath,
        depth: usize,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LocalScopeId(u32);

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
                    [binding] => {
                        return ResolutionResult::Found(crate::hir::Path {
                            res: binding_to_res(binding),
                            segments: Vec::new(),
                        });
                    }
                    _ => return ResolutionResult::Ambiguous,
                }
            }
            current = node.parent;
        }
        ResolutionResult::NotFound(ResolutionNotFound::Local {
            symbol: Symbol::from(symbol),
            namespace,
        })
    }
}

#[cfg(all(test, feature = "legacy_resolve_tests"))]
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
        let mut tree = ModuleData::new();
        let root = InPackagePath::new(Vec::new());
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
        let mut tree = ModuleData::new();
        let root = InPackagePath::new(Vec::new());
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
        let mut tree = ModuleData::new();
        let root = InPackagePath::new(Vec::new());
        let nested = InPackagePath::new(vec!["m".into()]);
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
                &InPackagePath::new(vec!["m".into(), "Thing".into()]),
                Namespace::Type,
                ResolutionRules::rust(),
            ),
            ResolutionResult::Found(crate::hir::Res::Def(id)) if id == crate::hir::DefId::local(42)
        ));
        tree.declare(
            &nested,
            "value",
            def(43, Namespace::Value),
            DeclarationRules::rust(),
        );
        assert!(matches!(
            tree.resolve_path(
                &root,
                &InPackagePath::new(vec!["m".into(), "value".into()]),
                Namespace::Value,
                ResolutionRules::rust(),
            ),
            ResolutionResult::Found(crate::hir::Res::Def(id)) if id == crate::hir::DefId::local(43)
        ));
        tree.declare(
            &nested,
            "make_value",
            Binding::Macro {
                id: crate::hir::DefId::local(44),
                span: span(),
            },
            DeclarationRules::rust(),
        );
        assert!(matches!(
            tree.resolve_path(
                &root,
                &InPackagePath::new(vec!["m".into(), "make_value".into()]),
                Namespace::Macro,
                ResolutionRules::rust(),
            ),
            ResolutionResult::Found(crate::hir::Res::Def(id)) if id == crate::hir::DefId::local(44)
        ));
    }

    #[test]
    fn parent_module_lookup_is_policy_controlled() {
        let mut tree = ModuleData::new();
        let root = InPackagePath::new(Vec::new());
        let child = InPackagePath::new(vec!["child".into()]);
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
            ResolutionResult::NotFound(ResolutionNotFound::Symbol {
                module: child.clone(),
                symbol: Symbol::from("x"),
                namespace: Namespace::Value,
            })
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
        let mut tree = ModuleData::new();
        let root = InPackagePath::new(Vec::new());
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
