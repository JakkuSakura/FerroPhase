use super::worklist::ResolutionWorklist;
use super::Resolver;
use fp_core::ast::package::PackageId;
use fp_core::ast::path::{InPackagePath, PathPrefix};
use fp_core::ast::Path;
use fp_core::ast::program::AstProgram;
use fp_core::cfg::CfgFilter;
use fp_core::hir;
use fp_core::hir::resolve::{
    Binding, DeclarationOutcome, DeclarationRules, ModuleData, Namespace, ResolutionResult,
    ResolutionRules,
};
use fp_core::hir::HirProgram;
use fp_core::hir::Symbol;
use fp_core::span::Span;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::VecDeque;
use std::collections::HashMap;
use std::rc::Rc;
pub struct InPackageResolver {
    hir_package: Rc<RefCell<hir::HirPackage>>,
    resolver: Resolver,
    pub declaration_rules: DeclarationRules,
    pub resolution_rules: ResolutionRules,
    /// Authoritative AST package registry used for package-directed lookup.
    /// The resolver retains the program instead of cloning package/module
    /// state into a second registry.
    ast_program: Rc<AstProgram>,
    hir_program: Rc<RefCell<HirProgram>>,
    cfg_filter: CfgFilter,
    impl_def_occurrences: HashMap<(String, Span), usize>,
}

impl InPackageResolver {
    pub fn new(
        hir_package: Rc<RefCell<hir::HirPackage>>,
        hir_program: Rc<RefCell<HirProgram>>,
        declaration_rules: DeclarationRules,
        resolution_rules: ResolutionRules,
        ast_program: Rc<AstProgram>,
    ) -> Self {
        Self {
            hir_package,
            resolver: Resolver::new(Rc::clone(&ast_program), Rc::clone(&hir_program)),
            declaration_rules,
            resolution_rules,
            ast_program,
            hir_program,
            cfg_filter: CfgFilter::host(),
            impl_def_occurrences: HashMap::new(),
        }
    }

    pub fn with_cfg_filter(mut self, cfg_filter: CfgFilter) -> Self {
        self.cfg_filter = cfg_filter;
        self
    }

    pub fn resolve_package(&mut self, package_id: &PackageId) -> fp_core::error::Result<()> {
        self.impl_def_occurrences.clear();
        let module = self
            .ast_program
            .get_ast_package(package_id)
            .borrow()
            .module
            .clone();
        let root = InPackagePath::new(Vec::new());
        let mut worklist = ResolutionWorklist::default();
        self.collect_module(&root, &module.items, &mut worklist);
        self.resolve_worklist(&mut worklist);
        Ok(())
    }

    pub fn package(&self) -> Rc<RefCell<hir::HirPackage>> {
        Rc::clone(&self.hir_package)
    }

    pub fn resolve_declared(
        &self,
        module: &InPackagePath,
        name: &str,
        namespace: Namespace,
    ) -> Option<hir::DefId> {
        let module_id = self.module_id(module)?;
        match self
            .module_data()
            .resolve_child(&module_id, name, namespace)
        {
            ResolutionResult::Found(path) => match path.res {
                hir::Res::Def(id) | hir::Res::Module(id) => Some(id),
                _ => None,
            },
            _ => None,
        }
    }

    fn collect_module(
        &mut self,
        module: &InPackagePath,
        items: &[fp_core::ast::Item],
        worklist: &mut ResolutionWorklist,
    ) {
        self.enqueue_prelude_imports(module, worklist);
        for item in items {
            if !self.cfg_filter.allows(item) {
                continue;
            }
            self.ensure_module_bindings(module, item.span());
            self.collect_item(module, item);
            if !matches!(item.kind(), fp_core::ast::ItemKind::Module(_)) {
                self.collect_import_item(module, item, worklist);
            }
            if let fp_core::ast::ItemKind::Module(child) = item.kind() {
                let child_path = module.with_segment(child.name.name.clone());
                self.collect_module(&child_path, &child.items, worklist);
            }
        }
    }

    fn enqueue_prelude_imports(
        &mut self,
        module: &InPackagePath,
        worklist: &mut ResolutionWorklist,
    ) {
        let package_id = self.hir_package.borrow().id.clone();
        let preludes = self
            .ast_program
            .get_ast_package(&package_id)
            .borrow()
            .prelude_modules
            .clone();
        for prelude in preludes {
            let mut target = InPackagePath::new(vec![prelude.package_id.as_str().to_owned()]);
            target.segments.extend(prelude.path.segments);
            let target = Self::ast_import_path(PathPrefix::Plain, &target);
            for namespace in [Namespace::Type, Namespace::Value, Namespace::Macro] {
                worklist.push(ImportDirective {
                    module: module.clone(),
                    name: Symbol::from(""),
                    target: target.clone(),
                    namespace,
                    kind: ImportKind::Glob,
                    visibility: fp_core::ast::Visibility::Public,
                    span: Span::null(),
                });
            }
        }
    }

    fn module_data(&self) -> Ref<'_, ModuleData> {
        Ref::map(self.hir_package.borrow(), |package| &package.module_data)
    }

    fn module_data_mut(&self) -> RefMut<'_, ModuleData> {
        RefMut::map(self.hir_package.borrow_mut(), |package| {
            &mut package.module_data
        })
    }

    fn module_id(&self, path: &InPackagePath) -> Option<hir::DefId> {
        let root = ModuleData::virtual_root_for(self.hir_package.borrow().id.clone());
        if path.segments.is_empty() {
            return Some(root);
        }
        match self
            .module_data()
            .resolve_module(&root, &path.segments, Namespace::Type)
        {
            fp_core::hir::resolve::ResolutionResult::Found(path) => match path.res {
                hir::Res::Module(id) => Some(id),
                _ => None,
            },
            _ => None,
        }
    }

    fn item_def_id(&mut self) -> hir::DefId {
        self.hir_package.borrow_mut().allocate_anonymous_def_id()
    }

    pub fn declare_module(
        &mut self,
        module: &InPackagePath,
        name: impl Into<Symbol>,
        binding: Binding,
    ) -> DeclarationOutcome {
        let rules = self.declaration_rules;
        let Some(module_id) = self.module_id(module) else {
            return DeclarationOutcome::Conflict;
        };
        self.module_data_mut()
            .declare(&module_id, name, binding, rules)
    }

    pub fn declare_import(
        &mut self,
        module: &InPackagePath,
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

    fn ensure_module_bindings(&mut self, path: &InPackagePath, span: Span) {
        let root = ModuleData::virtual_root_for(self.hir_package.borrow().id.clone());
        for index in 0..path.segments.len() {
            let parent = InPackagePath::new(path.segments[..index].to_vec());
            let child = path.segments[index].clone();
            let child_path = InPackagePath::new(path.segments[..=index].to_vec());
            let existing =
                self.module_data()
                    .resolve_module(&root, &child_path.segments, Namespace::Type);
            let module_id = match existing {
                fp_core::hir::resolve::ResolutionResult::Found(path) => {
                    let hir::Res::Module(id) = path.res else {
                        return;
                    };
                    id
                }
                _ => {
                    let id = self.item_def_id();
                    self.module_data_mut().set_children(id.clone(), Vec::new());
                    id
                }
            };
            let Some(parent_id) = self.module_id(&parent) else {
                continue;
            };
            let already_declared = matches!(
                self.module_data()
                    .resolve_child(&parent_id, &child, Namespace::Type),
                fp_core::hir::resolve::ResolutionResult::Found(path)
                    if matches!(path.res, hir::Res::Module(_))
            );
            if !already_declared {
                self.declare_module(
                    &parent,
                    child,
                    Binding::Module {
                        target: child_path,
                        def_id: module_id,
                        span,
                    },
                );
            }
        }
    }

    fn declare_definition(
        &mut self,
        module: &InPackagePath,
        name: impl Into<Symbol>,
        namespace: Namespace,
        span: Span,
    ) -> hir::DefId {
        let Some(module_id) = self.module_id(module) else {
            return self.item_def_id();
        };
        let (target, _) = self.hir_package.borrow_mut().register_definition(
            &module_id,
            name,
            namespace,
            span,
            self.declaration_rules,
        );
        target
    }

    fn collect_item(&mut self, module: &InPackagePath, item: &fp_core::ast::Item) {
        use fp_core::ast::ItemKind;

        let span = item.span();
        match item.kind() {
            ItemKind::Module(child) => {
                let child_path = module.with_segment(child.name.name.clone());
                let Some(parent_id) = self.module_id(module) else {
                    return;
                };
                self.hir_package.borrow_mut().register_module(
                    &parent_id,
                    &child.name,
                    child_path,
                    span,
                    self.declaration_rules,
                );
            }
            ItemKind::DefStruct(def) => {
                let def_id = self.declare_definition(module, &def.name, Namespace::Type, span);
                self.declare_module(
                    module,
                    def.name.clone(),
                    Binding::Definition {
                        target: def_id,
                        namespace: Namespace::Value,
                        span,
                    },
                );
            }
            ItemKind::DefStructural(def) => {
                let def_id = self.declare_definition(module, &def.name, Namespace::Type, span);
                self.declare_module(
                    module,
                    def.name.clone(),
                    Binding::Definition {
                        target: def_id,
                        namespace: Namespace::Value,
                        span,
                    },
                );
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
                let trait_id = self.declare_definition(module, &def.name, Namespace::Type, span);
                for member in &def.items {
                    let (name, namespace) = match member.kind() {
                        ItemKind::DefFunction(function) => (&function.name, Namespace::Value),
                        ItemKind::DeclFunction(function) => (&function.name, Namespace::Value),
                        ItemKind::DefType(ty) => (&ty.name, Namespace::Type),
                        ItemKind::DeclType(ty) => (&ty.name, Namespace::Type),
                        ItemKind::DefConst(konst) => (&konst.name, Namespace::Value),
                        ItemKind::DeclConst(konst) => (&konst.name, Namespace::Value),
                        _ => continue,
                    };
                    let member_id = self.hir_package.borrow_mut().member_def_id(
                        &trait_id,
                        name.clone(),
                        namespace,
                    );
                    self.module_data_mut().add_child(
                        trait_id.clone(),
                        name.clone(),
                        namespace,
                        hir::Res::Def(member_id),
                    );
                }
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
            ItemKind::DeclType(def) => {
                self.declare_definition(module, &def.name, Namespace::Type, span);
            }
            ItemKind::DeclConst(def) => {
                self.declare_definition(module, &def.name, Namespace::Value, span);
            }
            ItemKind::DeclStatic(def) => {
                self.declare_definition(module, &def.name, Namespace::Value, span);
            }
            ItemKind::DeclFunction(def) => {
                self.declare_definition(module, &def.name, Namespace::Value, span);
            }
            ItemKind::Impl(_) => {
                let module_key = module.to_key();
                let ordinal = self
                    .impl_def_occurrences
                    .entry((module_key.clone(), span))
                    .and_modify(|value| *value += 1)
                    .or_insert(0);
                self.hir_package
                    .borrow_mut()
                    .impl_def_id(&module_key, span, *ordinal);
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

    pub fn resolve_worklist(&mut self, worklist: &mut ResolutionWorklist) {
        // Imports form a dependency graph: a glob can only be expanded after
        // another import has populated its target module. Resolve in explicit
        // fixed-point rounds so a directive deferred in one round is retried
        // after every successful declaration, without requiring callers to
        // invoke the resolver a second time.
        while !worklist.queue.is_empty() {
            let mut deferred = VecDeque::new();
            let mut made_progress = false;
            while let Some(directive) = worklist.queue.pop_front() {
                if directive.kind == ImportKind::Glob {
                    let resolved = if directive.target.segments.is_empty() {
                        ResolutionResult::Found(hir::Path {
                            span: Default::default(),
                            res: hir::Res::Module(ModuleData::virtual_root_for(
                                self.hir_package.borrow().id.clone(),
                            )),
                            segments: Vec::new(),
                        })
                    } else {
                        self.resolver.resolve_parsed_path(
                            &self.hir_package.borrow().id,
                            &directive.module,
                            &directive.target,
                            Namespace::Type,
                        )
                    };
                    let members = match resolved {
                        ResolutionResult::Found(path)
                            if let hir::Res::Module(module) = path.res.clone() =>
                        {
                            self.hir_program
                                .borrow()
                                .package(&module.package_id)
                                .and_then(|package| {
                                    package.module_data.children(&module).map(|children| {
                                        children
                                            .iter()
                                            .filter(|(_, ns, _)| *ns == directive.namespace)
                                            .map(|(name, _, res)| (name.clone(), res.clone()))
                                            .collect::<Vec<_>>()
                                    })
                                })
                        }
                        _ => None,
                    };
                    let Some(members) = members else {
                        deferred.push_back(directive);
                        continue;
                    };
                    if members.is_empty() {
                        deferred.push_back(directive);
                    } else {
                        let mut inserted = false;
                        for (name, target) in members {
                            let outcome = self.declare_import(
                                &directive.module,
                                name,
                                target,
                                directive.namespace,
                                directive.span,
                            );
                            inserted |= outcome == DeclarationOutcome::Inserted;
                        }
                        made_progress |= inserted;
                        // Keep a glob alive for another round. Its target
                        // module may gain additional re-exports after another
                        // directive populates that module.
                        deferred.push_back(directive);
                    }
                    continue;
                }

                // Imports may legally bind a module itself (`use crate::foo as
                // bar`), so terminal type/value checks stay out of this pass.
                let resolved = self.resolver.resolve_parsed_path(
                    &self.hir_package.borrow().id,
                    &directive.module,
                    &directive.target,
                    directive.namespace,
                );
                let Some(target) = (match resolved {
                    ResolutionResult::Found(path) => Some(path.res),
                    ResolutionResult::Ambiguous | ResolutionResult::NotFound(_) => None,
                }) else {
                    deferred.push_back(directive);
                    continue;
                };
                self.declare_import(
                    &directive.module,
                    directive.name,
                    target,
                    directive.namespace,
                    directive.span,
                );
                made_progress = true;
            }
            if !made_progress {
                // No directive can make progress in the current state. Keep
                // unresolved explicit imports for diagnostics, but consume
                // quiescent globs so callers do not need to drain a
                // permanently retryable wildcard.
                worklist.queue = deferred
                    .into_iter()
                    .filter(|directive| directive.kind != ImportKind::Glob)
                    .collect();
                break;
            }
            worklist.queue = deferred;
        }
    }

    fn collect_import_item(
        &self,
        module: &InPackagePath,
        item: &fp_core::ast::Item,
        worklist: &mut ResolutionWorklist,
    ) {
        use fp_core::ast::ItemKind;
        if let ItemKind::Import(import) = item.kind() {
            let mut leaves = Vec::new();
            let (base, prefix) = match &import.style {
                fp_core::ast::ItemImportStyle::Plain => {
                    // Keep a plain import relative to its lexical module.
                    // `Resolver` performs the ancestor walk and recognizes
                    // extern-prelude crate roots; baking `module` into the
                    // target would make aliases such as `alloc_crate` look
                    // like children of every importing submodule.
                    (InPackagePath::new(Vec::new()), PathPrefix::Plain)
                }
                fp_core::ast::ItemImportStyle::From(from) => {
                    let mut base = module.clone();
                    for _ in 0..from.level {
                        let _ = base.pop();
                    }
                    self.import_path(
                        module,
                        &from.module,
                        base,
                        if from.level > 0 {
                            PathPrefix::Crate
                        } else {
                            PathPrefix::Plain
                        },
                    )
                }
            };
            self.collect_tree(module, base, prefix, &import.tree, &mut leaves);
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
        }
    }

    fn import_path(
        &self,
        module: &InPackagePath,
        path: &fp_core::ast::ItemImportPath,
        mut base: InPackagePath,
        mut prefix: PathPrefix,
    ) -> (InPackagePath, PathPrefix) {
        for segment in &path.segments {
            match segment {
                fp_core::ast::ItemImportTree::Root => {
                    base = InPackagePath::new(Vec::new());
                    prefix = PathPrefix::Root;
                }
                fp_core::ast::ItemImportTree::Crate => {
                    base = InPackagePath::new(Vec::new());
                    prefix = PathPrefix::Crate;
                }
                fp_core::ast::ItemImportTree::SelfMod => {
                    base = module.clone();
                    prefix = PathPrefix::Crate;
                }
                fp_core::ast::ItemImportTree::SuperMod => {
                    if base.segments.is_empty() {
                        base = module.clone();
                    }
                    let _ = base.pop();
                    prefix = PathPrefix::Crate;
                }
                fp_core::ast::ItemImportTree::Ident(ident) => base.push(ident.name.clone()),
                fp_core::ast::ItemImportTree::Path(nested) => {
                    (base, prefix) = self.import_path(module, nested, base, prefix)
                }
                _ => {}
            }
        }
        (base, prefix)
    }

    fn collect_tree(
        &self,
        module: &InPackagePath,
        prefix: InPackagePath,
        prefix_kind: PathPrefix,
        tree: &fp_core::ast::ItemImportTree,
        out: &mut Vec<(Path, Symbol, ImportKind)>,
    ) {
        use fp_core::ast::ItemImportTree;
        match tree {
            ItemImportTree::Root | ItemImportTree::Crate => {}
            ItemImportTree::SelfMod => {
                if let Some(name) = prefix.tail().map(Symbol::from) {
                    out.push((
                        Self::ast_import_path(prefix_kind, &prefix),
                        name,
                        ImportKind::Single,
                    ));
                }
            }
            ItemImportTree::SuperMod => {
                let target = prefix
                    .parent_n(1)
                    .unwrap_or_else(|| InPackagePath::new(Vec::new()));
                if let Some(name) = target.tail().map(Symbol::from) {
                    out.push((
                        Self::ast_import_path(PathPrefix::Crate, &target),
                        name,
                        ImportKind::Single,
                    ));
                }
            }
            ItemImportTree::Ident(ident) => {
                let target = prefix.with_segment(ident.name.clone());
                out.push((
                    Self::ast_import_path(prefix_kind, &target),
                    Symbol::from(ident.name.as_str()),
                    ImportKind::Single,
                ));
            }
            ItemImportTree::Rename(rename) => {
                out.push((
                    Self::ast_import_path(
                        prefix_kind,
                        &prefix.with_segment(rename.from.name.clone()),
                    ),
                    Symbol::from(rename.to.name.as_str()),
                    ImportKind::Single,
                ));
            }
            ItemImportTree::Glob => out.push((
                Self::ast_import_path(prefix_kind, &prefix),
                Symbol::from(""),
                ImportKind::Glob,
            )),
            ItemImportTree::Group(group) => {
                for member in &group.items {
                    self.collect_tree(module, prefix.clone(), prefix_kind.clone(), member, out);
                }
            }
            ItemImportTree::Path(path) => {
                let mut current = prefix;
                let mut current_kind = prefix_kind;
                for (index, member) in path.segments.iter().enumerate() {
                    if index + 1 == path.segments.len() {
                        self.collect_tree(
                            module,
                            current.clone(),
                            current_kind.clone(),
                            member,
                            out,
                        );
                    } else {
                        (current, current_kind) = self.import_path(
                            module,
                            &fp_core::ast::ItemImportPath {
                                segments: vec![member.clone()],
                            },
                            current,
                            current_kind,
                        );
                    }
                }
            }
        }
    }

    fn ast_import_path(prefix: PathPrefix, path: &InPackagePath) -> Path {
        Path::new(
            prefix,
            path.segments
                .iter()
                .cloned()
                .map(fp_core::ast::Ident::new)
                .map(Into::into)
                .collect(),
        )
    }
}

#[derive(Debug, Clone)]
pub struct ImportDirective {
    pub module: InPackagePath,
    pub name: Symbol,
    pub target: Path,
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
#[cfg(test)]
mod tests {
    use super::*;
    use crate::worklist;
    use fp_core::ast::package::provider::EmptyProvider;
    use fp_core::span::Span;
    use std::sync::Arc;

    fn test_program() -> Rc<AstProgram> {
        Rc::new(AstProgram::new(Arc::new(EmptyProvider)))
    }

    fn resolver() -> (InPackageResolver, Rc<RefCell<hir::HirPackage>>) {
        let package = Rc::new(RefCell::new(hir::HirPackage::new(hir::PackageId::new(
            "test",
        ))));
        let hir_program = Rc::new(RefCell::new(HirProgram::new()));
        hir_program.borrow_mut().add_package(Rc::clone(&package));
        let resolver = InPackageResolver::new(
            Rc::clone(&package),
            hir_program,
            DeclarationRules::rust(),
            ResolutionRules::rust(),
            test_program(),
        );
        (resolver, package)
    }

    fn root_id(package: &hir::HirPackage) -> hir::DefId {
        ModuleData::virtual_root_for(package.id.clone())
    }

    #[test]
    fn worklist_resolves_forward_alias_after_target_is_committed() {
        let root_path = InPackagePath::new(Vec::new());
        let target = hir::DefId::local(7);
        let (mut resolver, package) = resolver();
        let package_root = root_id(&package.borrow());
        package.borrow_mut().module_data.add_child(
            package_root,
            "Target",
            Namespace::Type,
            hir::Res::Def(target.clone()),
        );
        let mut worklist = worklist::ResolutionWorklist::default();
        worklist.push(ImportDirective {
            module: root_path.clone(),
            name: "Alias".into(),
            target: Path::new(PathPrefix::Plain, vec!["Target".into()]),
            namespace: Namespace::Type,
            kind: ImportKind::Single,
            visibility: fp_core::ast::Visibility::Private,
            span: Span::null(),
        });

        resolver.resolve_worklist(&mut worklist);

        assert!(worklist.is_empty());
        assert_eq!(
            package.borrow().module_data.resolve_child(
                &root_id(&package.borrow()),
                "Alias",
                Namespace::Type
            ),
            fp_core::hir::resolve::ResolutionResult::Found(hir::Path {
                span: Default::default(),
                res: hir::Res::Def(target.clone()),
                segments: vec![hir::PathSegment {
                    ident: "Alias".into(),
                    hir_id: Default::default(),
                    args: None,
                    infer_args: true,
                    delegation_child_segment: false,
                    res: hir::Res::Def(target),
                }]
            }),
        );
    }

    #[test]
    fn worklist_retains_quiescent_unresolved_directive() {
        let root_path = InPackagePath::new(Vec::new());
        let (mut resolver, _package) = resolver();
        let mut worklist = worklist::ResolutionWorklist::default();
        worklist.push(ImportDirective {
            module: root_path,
            name: "MissingAlias".into(),
            target: Path::new(PathPrefix::Plain, vec!["Missing".into()]),
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
        let root_path = InPackagePath::new(Vec::new());
        let target = hir::DefId::local(9);
        let (mut resolver, package) = resolver();
        let package_root = root_id(&package.borrow());
        package.borrow_mut().module_data.add_child(
            package_root,
            "Target",
            Namespace::Type,
            hir::Res::Def(target.clone()),
        );
        let mut worklist = worklist::ResolutionWorklist::default();
        for (name, source) in [("Second", "First"), ("First", "Target")] {
            worklist.push(ImportDirective {
                module: root_path.clone(),
                name: name.into(),
                target: Path::new(PathPrefix::Plain, vec![source.into()]),
                namespace: Namespace::Type,
                kind: ImportKind::Single,
                visibility: fp_core::ast::Visibility::Private,
                span: Span::null(),
            });
        }

        resolver.resolve_worklist(&mut worklist);

        assert!(worklist.is_empty());
        assert!(matches!(
            package.borrow().module_data.resolve_child(
                &root_id(&package.borrow()),
                "Second",
                Namespace::Type
            ),
            fp_core::hir::resolve::ResolutionResult::Found(path) if matches!(path.res, hir::Res::Def(_))
        ));
    }

    #[test]
    fn worklist_expands_glob_members_into_destination_module() {
        let root_path = InPackagePath::new(Vec::new());
        let source = InPackagePath::new(vec!["source".into()]);
        let target = hir::DefId::local(11);
        let (mut resolver, package) = resolver();
        let source_id = hir::DefId::new(package.borrow().id.clone(), 12);
        package.borrow_mut().module_data.set_children(
            source_id.clone(),
            vec![(
                "Item".into(),
                Namespace::Type,
                hir::Res::Def(target.clone()),
            )],
        );
        let package_root = root_id(&package.borrow());
        package.borrow_mut().module_data.add_child(
            package_root,
            "source",
            Namespace::Type,
            hir::Res::Module(source_id),
        );
        let mut worklist = worklist::ResolutionWorklist::default();
        worklist.push(ImportDirective {
            module: root_path.clone(),
            name: "".into(),
            target: InPackageResolver::ast_import_path(PathPrefix::Plain, &source),
            namespace: Namespace::Type,
            kind: ImportKind::Glob,
            visibility: fp_core::ast::Visibility::Private,
            span: Span::null(),
        });

        resolver.resolve_worklist(&mut worklist);

        assert!(worklist.is_empty());
        assert_eq!(
            package.borrow().module_data.resolve_child(
                &root_id(&package.borrow()),
                "Item",
                Namespace::Type
            ),
            fp_core::hir::resolve::ResolutionResult::Found(hir::Path {
                span: Default::default(),
                res: hir::Res::Def(target.clone()),
                segments: vec![hir::PathSegment {
                    ident: "Item".into(),
                    hir_id: Default::default(),
                    args: None,
                    infer_args: true,
                    delegation_child_segment: false,
                    res: hir::Res::Def(target),
                }]
            }),
        );
    }

    #[test]
    fn worklist_can_import_a_module_binding() {
        let root_path = InPackagePath::new(Vec::new());
        let child = InPackagePath::new(vec!["child".into()]);
        let (mut resolver, package) = resolver();
        let child_id = hir::DefId::local(1);
        package
            .borrow_mut()
            .module_data
            .set_children(child_id.clone(), Vec::new());
        let package_root = root_id(&package.borrow());
        package.borrow_mut().module_data.add_child(
            package_root,
            "child",
            Namespace::Type,
            hir::Res::Module(child_id.clone()),
        );
        let mut worklist = worklist::ResolutionWorklist::default();
        worklist.push(ImportDirective {
            module: root_path.clone(),
            name: "alias".into(),
            target: InPackageResolver::ast_import_path(PathPrefix::Plain, &child),
            namespace: Namespace::Type,
            kind: ImportKind::Single,
            visibility: fp_core::ast::Visibility::Private,
            span: Span::null(),
        });

        resolver.resolve_worklist(&mut worklist);

        assert_eq!(
            package.borrow().module_data.resolve_child(
                &root_id(&package.borrow()),
                "alias",
                Namespace::Type
            ),
            fp_core::hir::resolve::ResolutionResult::Found(hir::Path {
                span: Default::default(),
                res: hir::Res::Module(hir::DefId::local(1)),
                segments: vec![hir::PathSegment {
                    ident: "alias".into(),
                    hir_id: Default::default(),
                    args: None,
                    infer_args: true,
                    delegation_child_segment: false,
                    res: hir::Res::Module(hir::DefId::local(1)),
                }]
            }),
        );
    }
}
