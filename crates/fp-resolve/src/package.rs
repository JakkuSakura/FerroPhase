use super::Resolver;
use super::worklist::ResolutionWorklist;
use fp_core::ast::package::PackageId;
use fp_core::ast::path::InPackagePath;
use fp_core::ast::program::AstProgram;
use fp_core::cfg::CfgFilter;
use fp_core::hir;
use fp_core::hir::HirProgram;
use fp_core::hir::Symbol;
use fp_core::hir::resolve::{
    Binding, DeclarationOutcome, DeclarationRules, ModuleData, Namespace, ResolutionResult,
    ResolutionRules,
};
use fp_core::span::Span;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::VecDeque;
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
    cfg_filter: CfgFilter,
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
            resolver: Resolver::new(Rc::clone(&ast_program), hir_program),
            declaration_rules,
            resolution_rules,
            ast_program,
            cfg_filter: CfgFilter::host(),
        }
    }

    pub fn with_cfg_filter(mut self, cfg_filter: CfgFilter) -> Self {
        self.cfg_filter = cfg_filter;
        self
    }

    pub fn resolve_package(&mut self, package_id: &PackageId) -> fp_core::error::Result<()> {
        let module = self
            .ast_program
            .get_ast_package(package_id)
            .borrow()
            .module
            .clone();
        let root = InPackagePath::new(Vec::new());
        let mut worklist = ResolutionWorklist::default();
        self.collect_module(&root, &module.items, &mut worklist);
        self.collect_preludes();
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
            ResolutionResult::Found(hir::Res::Def(id))
            | ResolutionResult::Found(hir::Res::Module(id)) => Some(id),
            _ => None,
        }
    }

    fn collect_module(
        &mut self,
        module: &InPackagePath,
        items: &[fp_core::ast::Item],
        worklist: &mut ResolutionWorklist,
    ) {
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

    fn collect_preludes(&mut self) {
        let preludes = self
            .ast_program
            .get_ast_package(&self.hir_package.borrow().id)
            .borrow()
            .prelude_modules
            .clone();
        for prelude in preludes {
            if let ResolutionResult::Found(hir::Res::Module(def_id)) =
                self.module_data().resolve_module(
                    &ModuleData::virtual_root_for(self.hir_package.borrow().id.clone()),
                    &prelude.path.segments,
                    Namespace::Type,
                )
            {
                if !self.hir_package.borrow().prelude_modules.contains(&def_id) {
                    self.hir_package.borrow_mut().prelude_modules.push(def_id);
                }
            }
        }
        let prelude_ids = self.hir_package.borrow().prelude_modules.clone();
        for def_id in prelude_ids {
            let modules: Vec<_> = self.module_data().module_ids().cloned().collect();
            for module in modules {
                self.hir_package
                    .borrow_mut()
                    .module_data
                    .copy_children(&def_id, &module);
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
            fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Module(id)) => Some(id),
            _ => None,
        }
    }

    fn item_def_id(&mut self) -> hir::DefId {
        self.hir_package.borrow_mut().next_def_id()
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
                fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Module(id)) => id,
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
                fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Module(_))
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
        let target = self.item_def_id();
        let name: Symbol = name.into();
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

    fn collect_item(&mut self, module: &InPackagePath, item: &fp_core::ast::Item) {
        use fp_core::ast::ItemKind;

        let span = item.span();
        match item.kind() {
            ItemKind::Module(child) => {
                let child_path = module.with_segment(child.name.name.clone());
                let module_def_id = self.item_def_id();
                self.module_data_mut()
                    .set_children(module_def_id.clone(), Vec::new());
                self.declare_module(
                    module,
                    &child.name,
                    Binding::Module {
                        target: child_path.clone(),
                        def_id: module_def_id,
                        span,
                    },
                );
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
        let mut deferred = VecDeque::new();
        let mut made_progress = false;
        while let Some(directive) = worklist.queue.pop_front() {
            if directive.kind == ImportKind::Glob {
                let resolved = if directive.target.segments.is_empty() {
                    ResolutionResult::Found(hir::Res::Module(ModuleData::virtual_root_for(
                        self.hir_package.borrow().id.clone(),
                    )))
                } else {
                    self.resolver.resolve_parsed_path(
                        &self.hir_package.borrow().id,
                        &InPackagePath::new(Vec::new()),
                        &directive.target.to_ast_path(),
                        Namespace::Type,
                    )
                };
                let members = match resolved {
                    ResolutionResult::Found(hir::Res::Module(module)) => {
                        self.module_data().children(&module).map(|children| {
                            children
                                .iter()
                                .filter(|(_, ns, _)| *ns == directive.namespace)
                                .map(|(name, _, res)| (name.clone(), res.clone()))
                                .collect::<Vec<_>>()
                        })
                    }
                    _ => None,
                };
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
            let resolved = self.resolver.resolve_parsed_path(
                &self.hir_package.borrow().id,
                &InPackagePath::new(Vec::new()),
                &directive.target.to_ast_path(),
                directive.namespace,
            );
            let target = match resolved {
                fp_core::hir::resolve::ResolutionResult::Found(res) => Some(res),
                fp_core::hir::resolve::ResolutionResult::Ambiguous => None,
                fp_core::hir::resolve::ResolutionResult::NotFound(_) => None,
            };
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
        module: &InPackagePath,
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
        }
    }

    fn import_path(
        &self,
        module: &InPackagePath,
        path: &fp_core::ast::ItemImportPath,
        mut base: InPackagePath,
    ) -> InPackagePath {
        for segment in &path.segments {
            match segment {
                fp_core::ast::ItemImportTree::Root => base = InPackagePath::new(Vec::new()),
                fp_core::ast::ItemImportTree::Crate => {
                    base = InPackagePath::new(Vec::new());
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
        module: &InPackagePath,
        prefix: InPackagePath,
        tree: &fp_core::ast::ItemImportTree,
        out: &mut Vec<(InPackagePath, Symbol, ImportKind)>,
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
                    .unwrap_or_else(|| InPackagePath::new(Vec::new()));
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
    pub module: InPackagePath,
    pub name: Symbol,
    pub target: InPackagePath,
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
            target: InPackagePath::new(vec!["Target".into()]),
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
            fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Def(target)),
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
            target: InPackagePath::new(vec!["Missing".into()]),
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
                target: InPackagePath::new(vec![source.into()]),
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
            fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Def(_))
        ));
    }

    #[test]
    fn worklist_expands_glob_members_into_destination_module() {
        let root_path = InPackagePath::new(Vec::new());
        let source = InPackagePath::new(vec!["source".into()]);
        let target = hir::DefId::local(11);
        let (mut resolver, package) = resolver();
        let source_id = hir::DefId::local(12);
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
            target: source,
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
            fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Def(target)),
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
            target: child,
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
            fp_core::hir::resolve::ResolutionResult::Found(hir::Res::Module(hir::DefId::local(1))),
        );
    }
}
