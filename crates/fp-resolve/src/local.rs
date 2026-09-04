use crate::Resolver;
use fp_core::ast::Path;
use fp_core::ast::package::PackageId;
use fp_core::ast::path::InPackagePath;
use fp_core::ast::path::PathPrefix;
use fp_core::ast::program::AstProgram;
use fp_core::hir::HirPackage;
use fp_core::hir::HirProgram;
use fp_core::hir::Res;
use fp_core::hir::resolve::{
    Binding, DeclarationOutcome, DeclarationRules, LocalScope, Namespace, ResolutionResult,
    ResolutionRules,
};
use std::cell::RefCell;
use std::rc::Rc;

/// Lexical resolver used while lowering a package's item bodies.
///
/// Package/module declarations are established by `InPackageResolver`; this
/// resolver owns only transient lexical scopes and delegates global path
/// lookup to `Resolver`.
pub struct LocalResolver {
    resolver: Resolver,
    scopes: LocalScope,
    declaration_rules: DeclarationRules,
    resolution_rules: ResolutionRules,
    hir_package: Rc<RefCell<HirPackage>>,
}

impl LocalResolver {
    pub fn new(
        ast_program: Rc<AstProgram>,
        hir_program: Rc<RefCell<HirProgram>>,
        hir_package: Rc<RefCell<HirPackage>>,
        declaration_rules: DeclarationRules,
        resolution_rules: ResolutionRules,
    ) -> Self {
        Self {
            resolver: Resolver::new(ast_program, hir_program),
            scopes: LocalScope::new(),
            declaration_rules,
            resolution_rules,
            hir_package,
        }
    }
    pub fn enter_scope(&mut self) {
        self.scopes.enter();
    }

    pub fn leave_scope(&mut self) {
        self.scopes.leave();
    }

    pub fn declare(
        &mut self,
        name: impl Into<fp_core::hir::Symbol>,
        binding: Binding,
    ) -> DeclarationOutcome {
        self.scopes.declare(name, binding, self.declaration_rules)
    }

    pub fn declare_definition(
        &mut self,
        name: impl Into<fp_core::hir::Symbol>,
        namespace: Namespace,
        span: fp_core::span::Span,
    ) -> fp_core::hir::DefId {
        let id = self.hir_package.borrow_mut().allocate_anonymous_def_id();
        let _ = self.declare(
            name,
            Binding::Definition {
                target: id.clone(),
                namespace,
                span,
            },
        );
        id
    }

    pub fn resolve_local(&self, name: &str, namespace: Namespace) -> ResolutionResult {
        self.scopes.resolve(name, namespace, self.resolution_rules)
    }

    pub fn resolve_parsed_path(
        &self,
        current_package: &PackageId,
        location: &InPackagePath,
        path: &Path,
        namespace: Namespace,
    ) -> ResolutionResult {
        if matches!(path.prefix, fp_core::ast::path::PathPrefix::Plain) && !path.segments.is_empty()
        {
            // A lexical binding can be the base of a type-relative path
            // (`T::Assoc`) even when the use site is in value scope. Try the
            // requested namespace first so a value local still shadows a
            // same-named type, then try the type namespace for a projection
            // whose base is a generic/type binding.
            let mut local = self.resolve_local(path.segments[0].as_str(), namespace);
            if path.segments.len() > 1 && namespace != Namespace::Type {
                let type_local = self.resolve_local(path.segments[0].as_str(), Namespace::Type);
                if matches!(
                    type_local,
                    ResolutionResult::Found(ref path)
                        if matches!(path.res, Res::Generic(_) | Res::SelfTy | Res::Builtin(_))
                ) {
                    local = type_local;
                }
            }
            if let ResolutionResult::Found(mut resolved) = local {
                if !matches!(resolved.res, Res::Module(_)) {
                    let base_res = resolved.res.clone();
                    resolved.segments = vec![fp_core::hir::PathSegment {
                        ident: path.segments[0].as_str().into(),
                        hir_id: Default::default(),
                        args: Default::default(),
                        infer_args: true,
                        res: base_res,
                    }];
                    return ResolutionResult::Found(resolved);
                }
            }
        }
        self.resolver
            .resolve_parsed_path(current_package, location, path, namespace)
    }

    pub fn resolve_global_path(
        &self,
        current_package: &PackageId,
        location: &InPackagePath,
        path: &InPackagePath,
        namespace: Namespace,
    ) -> ResolutionResult {
        let parsed = Path::new(
            PathPrefix::Plain,
            path.segments
                .iter()
                .cloned()
                .map(|segment| {
                    fp_core::ast::PathSegment::new(
                        fp_core::ast::Ident::new(segment),
                        None,
                    )
                })
                .collect(),
        );
        self.resolver
            .resolve_parsed_path(current_package, location, &parsed, namespace)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::package::provider::EmptyProvider;
    use fp_core::hir;
    use fp_core::hir::resolve::{Binding, Namespace};
    use fp_core::span::Span;
    use std::sync::Arc;

    #[test]
    fn lexical_binding_returns_resolved_base_path() {
        let package_id = PackageId::new("test");
        let package = Rc::new(RefCell::new(HirPackage::new(package_id.clone())));
        let program = Rc::new(RefCell::new(HirProgram::new()));
        program.borrow_mut().add_package(Rc::clone(&package));
        let mut resolver = LocalResolver::new(
            Rc::new(AstProgram::new(Arc::new(EmptyProvider))),
            program,
            package,
            DeclarationRules::rust(),
            ResolutionRules::rust(),
        );
        resolver.enter_scope();
        let generic = hir::DefId::new(package_id.clone(), 7);
        resolver.declare(
            "T",
            Binding::Generic {
                id: generic.clone(),
                namespace: Namespace::Type,
                span: Span::null(),
            },
        );
        let path = Path::new(
            PathPrefix::Plain,
            vec!["T".into(), "Assoc".into(), "Field".into()],
        );
        assert_eq!(
            resolver.resolve_parsed_path(
                &package_id,
                &InPackagePath::new(Vec::new()),
                &path,
                Namespace::Type,
            ),
            ResolutionResult::Found(hir::Path {
                span: Default::default(),
                res: Res::Generic(generic.clone()),
                segments: vec![fp_core::hir::PathSegment {
                    ident: "T".into(),
                    hir_id: Default::default(),
                    args: Default::default(),
                    infer_args: true,
                    res: Res::Generic(generic.clone()),
                },],
            })
        );
    }
}
