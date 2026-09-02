use crate::Resolver;
use fp_core::ast::Path;
use fp_core::ast::package::PackageId;
use fp_core::ast::path::InPackagePath;
use fp_core::ast::path::PathPrefix;
use fp_core::ast::program::AstProgram;
use fp_core::hir::HirProgram;
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
}

impl LocalResolver {
    pub fn new(
        ast_program: Rc<AstProgram>,
        hir_program: Rc<RefCell<HirProgram>>,
        declaration_rules: DeclarationRules,
        resolution_rules: ResolutionRules,
    ) -> Self {
        Self {
            resolver: Resolver::new(ast_program, hir_program),
            scopes: LocalScope::new(),
            declaration_rules,
            resolution_rules,
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
        if matches!(path.prefix, fp_core::ast::path::PathPrefix::Plain) && path.segments.len() == 1
        {
            if let ResolutionResult::Found(res) =
                self.resolve_local(path.segments[0].as_str(), namespace)
            {
                return ResolutionResult::Found(res);
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
                .map(fp_core::ast::Ident::new)
                .collect(),
        );
        self.resolver
            .resolve_parsed_path(current_package, location, &parsed, namespace)
    }
}
