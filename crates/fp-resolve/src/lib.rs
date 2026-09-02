//! AST name-resolution orchestration and temporary worklist state.
//!
//! Stable resolution data lives in `fp-core`; this crate owns the algorithms
//! that populate it before AST-to-HIR lowering.

use fp_core::ast::package::PackageId;
use fp_core::ast::path::QualifiedPath;
use fp_core::ast::program::AstProgram;
use fp_core::hir::resolve::{
    Binding, DeclarationOutcome, DeclarationRules, LocalScope, ModuleTree, Namespace,
    ResolutionRules,
};
use fp_core::hir;
use fp_core::span::Span;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::rc::Rc;
use std::cell::{RefCell};
use fp_core::hir::Symbol;

pub struct AstResolver<'hir> {
    package: Rc<RefCell<fp_core::ast::package::AstPackage>>,
    hir_package: &'hir mut hir::HirPackage,
    pub locals: LocalScope,
    pub declaration_rules: DeclarationRules,
    pub resolution_rules: ResolutionRules,
    resolutions: HashMap<QualifiedPath, hir::Res>,
    /// Workspace registry used for extern-prelude lookup.  Keeping the
    /// registry (rather than cloned module trees) means imports resolve
    /// against the live AST packages and avoids a second, stale resolution
    /// snapshot.
    ast_program: Rc<AstProgram>,
}

impl<'hir> AstResolver<'hir> {
    pub fn new(
        ast_package_id: PackageId,
        hir_package: &'hir mut hir::HirPackage,
        declaration_rules: DeclarationRules,
        resolution_rules: ResolutionRules,
        ast_program: Rc<AstProgram>,
    ) -> Self {
        Self {
            package: ast_program.get_ast_package(&ast_package_id),
            hir_package,
            locals: LocalScope::new(),
            declaration_rules,
            resolution_rules,
            resolutions: HashMap::new(),
            ast_program,
        }
    }

    fn package_tree(&self) -> &ModuleTree {
        &self.hir_package.module_tree
    }

    fn package_tree_mut(&mut self) -> &mut ModuleTree {
        &mut self.hir_package.module_tree
    }

    fn item_def_id(&mut self) -> hir::DefId {
        self.hir_package.next_def_id()
    }

    pub fn declare_module(
        &mut self,
        module: &QualifiedPath,
        name: impl Into<Symbol>,
        binding: Binding,
    ) -> DeclarationOutcome {
        self.package_tree_mut()
            .declare(module, name, binding, self.declaration_rules)
    }

    pub fn declare_import(
        &mut self,
        module: &QualifiedPath,
        name: impl Into<Symbol>,
        target: hir::Res,
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

    pub fn collect_package_items(&mut self, items: &[fp_core::ast::package::PackageItem]) {
        for package_item in items {
            let module = package_item.module_path.clone();
            self.package_tree_mut().ensure_module(&module);
            self.collect_item(&module, &package_item.item);
        }
    }

    pub fn resolution_table(&self) -> &HashMap<QualifiedPath, hir::Res> {
        &self.resolutions
    }

    fn declare_definition(
        &mut self,
        module: &QualifiedPath,
        name: impl Into<Symbol>,
        namespace: Namespace,
        span: Span,
    ) -> hir::DefId {
        let target = self.item_def_id();
        let name: Symbol = name.into();
        let path = module.with_segment(name.to_string());
        self.resolutions.insert(path, hir::Res::Def(target.clone()));
        self.declare_module(
            module,
            name,
            Binding::Definition {
                target: target.clone(),
                namespace,
                span,
            },
        );
        target
    }

    fn collect_item(&mut self, module: &QualifiedPath, item: &fp_core::ast::Item) {
        use fp_core::ast::ItemKind;

        let span = item.span();
        match item.kind() {
            ItemKind::Module(child) => {
                let child_path = module.with_segment(child.name.name.clone());
                let module_def_id = self.item_def_id();
                self.resolutions
                    .insert(child_path.clone(), hir::Res::Def(module_def_id.clone()));
                self.package_tree_mut().ensure_module(&child_path);
                self.declare_module(
                    module,
                    &child.name,
                    Binding::Module {
                        target: child_path.clone(),
                        def_id: module_def_id,
                        span,
                    },
                );
                for nested in &child.items {
                    self.collect_item(&child_path, nested);
                }
            }
            ItemKind::DefStruct(def) => {
                self.declare_definition(module, &def.name, Namespace::Type, span);
            }
            ItemKind::DefStructural(def) => {
                self.declare_definition(module, &def.name, Namespace::Type, span);
            }
            ItemKind::DefEnum(def) => {
                self.declare_definition(module, &def.name, Namespace::Type, span);
            }
            ItemKind::DefType(def) => {
                self.declare_definition(module, &def.name, Namespace::Type, span);
            }
            ItemKind::OpaqueType(def) => {
                self.declare_definition(module, &def.name, Namespace::Type, span);
            }
            ItemKind::DefTrait(def) => {
                self.declare_definition(module, &def.name, Namespace::Type, span);
            }
            ItemKind::DefConst(def) => {
                self.declare_definition(module, &def.name, Namespace::Value, span);
            }
            ItemKind::DefStatic(def) => {
                self.declare_definition(module, &def.name, Namespace::Value, span);
            }
            ItemKind::DefFunction(def) => {
                self.declare_definition(module, &def.name, Namespace::Value, span);
            }
            ItemKind::Macro(mac) => {
                if let Some(name) = mac.declared_name.as_ref() {
                    let target = self.item_def_id();
                    self.declare_module(module, name, Binding::Macro { id: target, span });
                }
            }
            _ => {}
        }
    }

    pub fn collect_imports(
        &self,
        items: &[fp_core::ast::package::PackageItem],
        worklist: &mut ResolutionWorklist,
    ) {
        for item in items {
            self.collect_import_item(&item.module_path, &item.item, worklist);
        }
    }

    pub fn resolve_worklist(&mut self, worklist: &mut ResolutionWorklist) {
        let mut deferred = VecDeque::new();
        let mut made_progress = false;
        while let Some(directive) = worklist.queue.pop_front() {
            if directive.kind == ImportKind::Glob {
                let members = self
                    .package_tree()
                    .module(&directive.target)
                    .map(|source| {
                        source
                            .symbols
                            .keys()
                            .filter_map(|name| {
                                match source.resolve(
                                    &QualifiedPath::new(Vec::new()),
                                    name.as_str(),
                                    directive.namespace,
                                    self.resolution_rules,
                                ) {
                                    fp_core::hir::resolve::ResolutionResult::Found(res) => {
                                        Some((name.clone(), res))
                                    }
                                    _ => None,
                                }
                            })
                            .collect::<Vec<_>>()
                    });
                let Some(members) = members else {
                    deferred.push_back(directive);
                    if worklist.queue.is_empty() && !made_progress {
                        break;
                    }
                    continue;
                };
                if members.is_empty() {
                    deferred.push_back(directive);
                } else {
                    for (name, target) in members {
                        self.declare_import(
                            &directive.module,
                            name,
                            target,
                            directive.namespace,
                            directive.span,
                        );
                        made_progress = true;
                    }
                }
                if worklist.queue.is_empty() {
                    if !made_progress {
                        break;
                    }
                    made_progress = false;
                    worklist.queue.extend(deferred.drain(..));
                }
                continue;
            }
            // Imports may legally bind a module itself (`use crate::foo as bar`),
            // so do not apply value/type terminal checks here. Those checks are
            // reserved for expression/type references at lowering time.
            let target = match self.package_tree().resolve_path(
                &directive.module,
                &directive.target,
                directive.namespace,
                self.resolution_rules,
            ) {
                fp_core::hir::resolve::ResolutionResult::Found(res) => Some(res),
                _ => None,
            };
            let target = target.or_else(|| {
                let (root, rest) = directive.target.segments.split_first()?;
                let package_id = self
                    .ast_program
                    .crates()
                    .keys()
                    .find(|id| id.as_str().replace('-', "_") == root.as_str())?
                    .clone();
                match self.ast_program.resolve_module_path(
                    &package_id,
                    &QualifiedPath::new(Vec::new()),
                    &QualifiedPath::from_slice(rest),
                    directive.namespace,
                ) {
                    fp_core::hir::resolve::ResolutionResult::Found(res) => Some(res),
                    _ => None,
                }
            });
            if let Some(target) = target {
                self.declare_import(
                    &directive.module,
                    directive.name,
                    target,
                    directive.namespace,
                    directive.span,
                );
                made_progress = true;
                worklist.queue.extend(deferred.drain(..));
            } else {
                deferred.push_back(directive);
                if worklist.queue.is_empty() {
                    if !made_progress {
                        break;
                    }
                    made_progress = false;
                    worklist.queue.extend(deferred.drain(..));
                }
            }
        }
        worklist.queue.extend(deferred);
    }

    fn collect_import_item(
        &self,
        module: &QualifiedPath,
        item: &fp_core::ast::Item,
        worklist: &mut ResolutionWorklist,
    ) {
        use fp_core::ast::ItemKind;
        if let ItemKind::Import(import) = item.kind() {
            let mut leaves = Vec::new();
            let base = match &import.style {
                fp_core::ast::ItemImportStyle::Plain => module.clone(),
                fp_core::ast::ItemImportStyle::From(from) => {
                    let mut base = module.clone();
                    for _ in 0..from.level {
                        let _ = base.pop();
                    }
                    self.import_path(module, &from.module, base)
                }
            };
            self.collect_tree(module, base, &import.tree, &mut leaves);
            for (target, name, kind) in leaves {
                for namespace in [Namespace::Type, Namespace::Value] {
                    worklist.push(ImportDirective {
                        module: module.clone(),
                        name: Symbol::from(name.as_str()),
                        target: target.clone(),
                        namespace,
                        kind,
                        visibility: import.visibility.clone(),
                        span: import.span(),
                    });
                }
            }
        } else if let ItemKind::Module(child) = item.kind() {
            let child_module = module.with_segment(child.name.name.clone());
            for nested in &child.items {
                self.collect_import_item(&child_module, nested, worklist);
            }
        }
    }

    fn import_path(
        &self,
        module: &QualifiedPath,
        path: &fp_core::ast::ItemImportPath,
        mut base: QualifiedPath,
    ) -> QualifiedPath {
        for segment in &path.segments {
            match segment {
                fp_core::ast::ItemImportTree::Root => base = QualifiedPath::new(Vec::new()),
                fp_core::ast::ItemImportTree::Crate => {
                    base = module
                        .head()
                        .filter(|head| {
                            self.package_tree()
                                .module(&QualifiedPath::new(vec![(*head).to_owned()]))
                                .is_some()
                        })
                        .map(|head| QualifiedPath::new(vec![head.to_owned()]))
                        .unwrap_or_else(|| QualifiedPath::new(Vec::new()));
                }
                fp_core::ast::ItemImportTree::SelfMod => base = module.clone(),
                fp_core::ast::ItemImportTree::SuperMod => {
                    let _ = base.pop();
                }
                fp_core::ast::ItemImportTree::Ident(ident) => base.push(ident.name.clone()),
                fp_core::ast::ItemImportTree::Path(nested) => {
                    base = self.import_path(module, nested, base)
                }
                _ => {}
            }
        }
        base
    }

    fn collect_tree(
        &self,
        module: &QualifiedPath,
        prefix: QualifiedPath,
        tree: &fp_core::ast::ItemImportTree,
        out: &mut Vec<(QualifiedPath, Symbol, ImportKind)>,
    ) {
        use fp_core::ast::ItemImportTree;
        match tree {
            ItemImportTree::Root | ItemImportTree::Crate => {}
            ItemImportTree::SelfMod => {
                if let Some(name) = prefix.tail().map(Symbol::from) {
                    out.push((prefix, name, ImportKind::Single));
                }
            }
            ItemImportTree::SuperMod => {
                let target = prefix
                    .parent_n(1)
                    .unwrap_or_else(|| QualifiedPath::new(Vec::new()));
                if let Some(name) = target.tail().map(Symbol::from) {
                    out.push((target, name, ImportKind::Single));
                }
            }
            ItemImportTree::Ident(ident) => {
                let target = prefix.with_segment(ident.name.clone());
                out.push((
                    target,
                    Symbol::from(ident.name.as_str()),
                    ImportKind::Single,
                ));
            }
            ItemImportTree::Rename(rename) => {
                out.push((
                    prefix.with_segment(rename.from.name.clone()),
                    Symbol::from(rename.to.name.as_str()),
                    ImportKind::Single,
                ));
            }
            ItemImportTree::Glob => out.push((prefix, Symbol::from(""), ImportKind::Glob)),
            ItemImportTree::Group(group) => {
                for member in &group.items {
                    self.collect_tree(module, prefix.clone(), member, out);
                }
            }
            ItemImportTree::Path(path) => {
                let mut current = prefix;
                for (index, member) in path.segments.iter().enumerate() {
                    if index + 1 == path.segments.len() {
                        self.collect_tree(module, current.clone(), member, out);
                    } else {
                        current = self.import_path(
                            module,
                            &fp_core::ast::ItemImportPath {
                                segments: vec![member.clone()],
                            },
                            current,
                        );
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportDirective {
    pub module: QualifiedPath,
    pub name: Symbol,
    pub target: QualifiedPath,
    pub namespace: Namespace,
    pub kind: ImportKind,
    pub visibility: fp_core::ast::Visibility,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportKind {
    Single,
    Glob,
}

#[derive(Debug, Default)]
pub struct ResolutionWorklist {
    queue: VecDeque<ImportDirective>,
}

impl ResolutionWorklist {
    pub fn push(&mut self, directive: ImportDirective) {
        self.queue.push_back(directive);
    }

    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    pub fn len(&self) -> usize {
        self.queue.len()
    }
}

pub struct Resolver {
    program: Rc<AstProgram>,
}

impl Resolver {
    pub fn new(program: Rc<AstProgram>) -> Self {
        Self { program }
    }

    pub fn resolve_package(
        &self,
        package_id: &PackageId,
        hir_package: &mut hir::HirPackage,
    ) -> fp_core::error::Result<()> {
        let package = self.program.get_ast_package(package_id);
        let (package_id, items) = {
            let package = package.borrow();
            (
                package.package_id.clone(),
                package.items(),
            )
        };
        let mut resolver = AstResolver::new(
            package_id,
            hir_package,
            self.program.provider().declaration_rules(),
            self.program.provider().resolution_rules(),
            Rc::clone(&self.program),
        );
        resolver.collect_package_items(&items);
        let mut worklist = ResolutionWorklist::default();
        resolver.collect_imports(&items, &mut worklist);
        resolver.resolve_worklist(&mut worklist);
        drop(resolver);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::package::provider::EmptyProvider;
    use std::sync::Arc;

    fn test_program() -> Rc<AstProgram> {
        Rc::new(AstProgram::new(Arc::new(EmptyProvider)))
    }

    #[test]
    fn worklist_resolves_forward_alias_after_target_is_committed() {
        let root = QualifiedPath::new(Vec::new());
        let target = hir::DefId::local(7);
        let mut modules = ModuleTree::new();
        modules.declare(
            &root,
            "Target",
            Binding::Definition {
                target: target.clone(),
                namespace: Namespace::Type,
                span: Span::null(),
            },
            DeclarationRules::rust(),
        );
        let mut resolver = AstResolver::new(
            hir::PackageId::new("test"),
            &mut modules,
            DeclarationRules::rust(),
            ResolutionRules::rust(),
            test_program(),
        );
        let mut worklist = ResolutionWorklist::default();
        worklist.push(ImportDirective {
            module: root.clone(),
            name: Symbol::from("Alias"),
            target: QualifiedPath::new(vec!["Target".into()]),
            namespace: Namespace::Type,
            kind: ImportKind::Single,
            visibility: fp_core::ast::Visibility::Private,
            span: Span::null(),
        });

        resolver.resolve_worklist(&mut worklist);

        assert!(worklist.is_empty());
        assert_eq!(
            resolver
                .modules
                .resolve(&root, "Alias", Namespace::Type, ResolutionRules::rust(),),
            fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Def(target)),
        );
    }

    #[test]
    fn worklist_retains_quiescent_unresolved_directive() {
        let root = QualifiedPath::new(Vec::new());
        let mut modules = ModuleTree::new();
        let mut resolver = AstResolver::new(
            hir::PackageId::new("test"),
            &mut modules,
            DeclarationRules::rust(),
            ResolutionRules::rust(),
            test_program(),
        );
        let mut worklist = ResolutionWorklist::default();
        worklist.push(ImportDirective {
            module: root,
            name: Symbol::from("MissingAlias"),
            target: QualifiedPath::new(vec!["Missing".into()]),
            namespace: Namespace::Type,
            kind: ImportKind::Single,
            visibility: fp_core::ast::Visibility::Private,
            span: Span::null(),
        });

        resolver.resolve_worklist(&mut worklist);

        assert_eq!(worklist.len(), 1);
    }

    #[test]
    fn worklist_resolves_reexport_chain_independent_of_order() {
        let root = QualifiedPath::new(Vec::new());
        let target = hir::DefId::local(9);
        let mut modules = ModuleTree::new();
        modules.declare(
            &root,
            "Target",
            Binding::Definition {
                target: target.clone(),
                namespace: Namespace::Type,
                span: Span::null(),
            },
            DeclarationRules::rust(),
        );
        let mut resolver = AstResolver::new(
            hir::PackageId::new("test"),
            &mut modules,
            DeclarationRules::rust(),
            ResolutionRules::rust(),
            test_program(),
        );
        let mut worklist = ResolutionWorklist::default();
        for (name, source) in [("Second", "First"), ("First", "Target")] {
            worklist.push(ImportDirective {
                module: root.clone(),
                name: Symbol::from(name),
                target: QualifiedPath::new(vec![source.into()]),
                namespace: Namespace::Type,
                kind: ImportKind::Single,
                visibility: fp_core::ast::Visibility::Private,
                span: Span::null(),
            });
        }

        resolver.resolve_worklist(&mut worklist);

        assert!(worklist.is_empty());
        assert!(matches!(
            resolver
                .modules
                .resolve(&root, "Second", Namespace::Type, ResolutionRules::rust()),
            fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Def(_))
        ));
    }

    #[test]
    fn worklist_expands_glob_members_into_destination_module() {
        let root = QualifiedPath::new(Vec::new());
        let source = QualifiedPath::new(vec!["source".into()]);
        let target = hir::DefId::local(11);
        let mut modules = ModuleTree::new();
        modules.ensure_module(&source);
        modules.declare(
            &source,
            "Item",
            Binding::Definition {
                target: target.clone(),
                namespace: Namespace::Type,
                span: Span::null(),
            },
            DeclarationRules::rust(),
        );
        let mut resolver = AstResolver::new(
            hir::PackageId::new("test"),
            &mut modules,
            DeclarationRules::rust(),
            ResolutionRules::rust(),
            test_program(),
        );
        let mut worklist = ResolutionWorklist::default();
        worklist.push(ImportDirective {
            module: root.clone(),
            name: Symbol::from(""),
            target: source,
            namespace: Namespace::Type,
            kind: ImportKind::Glob,
            visibility: fp_core::ast::Visibility::Private,
            span: Span::null(),
        });

        resolver.resolve_worklist(&mut worklist);

        assert!(worklist.is_empty());
        assert_eq!(
            resolver
                .modules
                .resolve(&root, "Item", Namespace::Type, ResolutionRules::rust()),
            fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Def(target)),
        );
    }

    #[test]
    fn worklist_can_import_a_module_binding() {
        let root = QualifiedPath::new(Vec::new());
        let child = QualifiedPath::new(vec!["child".into()]);
        let mut modules = ModuleTree::new();
        modules.ensure_module(&child);
        modules.declare(
            &root,
            "child",
            Binding::Module {
                target: child.clone(),
                def_id: hir::DefId::local(1),
                span: Span::null(),
            },
            DeclarationRules::rust(),
        );
        let mut resolver = AstResolver::new(
            hir::PackageId::new("test"),
            &mut modules,
            DeclarationRules::rust(),
            ResolutionRules::rust(),
            test_program(),
        );
        let mut worklist = ResolutionWorklist::default();
        worklist.push(ImportDirective {
            module: root.clone(),
            name: Symbol::from("alias"),
            target: child,
            namespace: Namespace::Type,
            kind: ImportKind::Single,
            visibility: fp_core::ast::Visibility::Private,
            span: Span::null(),
        });

        resolver.resolve_worklist(&mut worklist);

        assert_eq!(
            resolver
                .modules
                .resolve(&root, "alias", Namespace::Type, ResolutionRules::rust()),
            fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Module(hir::DefId::local(1))),
        );
    }
}
