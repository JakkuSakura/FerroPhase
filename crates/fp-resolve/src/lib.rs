//! AST name-resolution orchestration and temporary worklist state.
//!
//! Stable resolution data lives in `fp-core`; this crate owns the algorithms
//! that populate it before AST-to-HIR lowering.

use fp_core::ast::Path;
use fp_core::ast::package::PackageId;
use fp_core::ast::path::{InPackagePath, PathPrefix};
use fp_core::ast::program::AstProgram;
use fp_core::hir;
use fp_core::hir::HirProgram;
use fp_core::hir::resolve::{Namespace, ResolutionResult, ResolutionRules};
use std::cell::RefCell;
use std::rc::Rc;

pub mod package;
pub mod worklist;

pub struct Resolver {
    program: Rc<AstProgram>,
    hir_program: RefCell<HirProgram>,
}

impl Resolver {
    pub fn new(program: Rc<AstProgram>) -> Self {
        Self {
            program,
            hir_program: RefCell::new(HirProgram::new()),
        }
    }

    /// Read-only lookup of a parsed source path at its lexical location.
    /// `resolve_package` populates the HIR; this method only computes the
    /// absolute in-package path and queries it, returning the terminal `Res`.
    pub fn resolve_parsed_path(
        &self,
        current_package_id: &PackageId,
        location: &InPackagePath,
        parsed: &Path,
        namespace: Namespace,
    ) -> ResolutionResult {
        if parsed.is_empty() {
            return ResolutionResult::NotFound(
                fp_core::hir::resolve::ResolutionNotFound::EmptyPath,
            );
        }
        let hir_program = self.hir_program.borrow();
        let mut external_package = None;
        if let Some(head) = parsed.head() {
            for package_id in hir_program.packages.keys() {
                if hir::HirProgram::external_crate_name(package_id) == head {
                    external_package = Some(package_id.clone());
                    break;
                }
            }
        }
        let target_package_id = match parsed.prefix {
            PathPrefix::Plain | PathPrefix::Root => external_package
                .clone()
                .unwrap_or_else(|| current_package_id.clone()),
            PathPrefix::Crate | PathPrefix::SelfMod | PathPrefix::Super(_) => {
                current_package_id.clone()
            }
        };
        let Some(absolute) = parsed.resolve_from(location) else {
            return ResolutionResult::NotFound(
                fp_core::hir::resolve::ResolutionNotFound::InvalidParent {
                    location: location.clone(),
                    depth: match parsed.prefix {
                        PathPrefix::Super(depth) => depth,
                        _ => 0,
                    },
                },
            );
        };
        let Some(package) = hir_program.package(&target_package_id) else {
            return ResolutionResult::NotFound(fp_core::hir::resolve::ResolutionNotFound::Package(
                target_package_id,
            ));
        };
        let result = package::InPackageResolver::resolve_path(
            &package,
            &absolute,
            namespace,
            ResolutionRules::rust(),
        );
        if !result.is_not_found() {
            return result;
        }
        if external_package.as_ref() != Some(&target_package_id) {
            return ResolutionResult::NotFound(fp_core::hir::resolve::ResolutionNotFound::Package(
                target_package_id,
            ));
        }
        let mut unqualified_segments = Vec::with_capacity(parsed.segments.len().saturating_sub(1));
        for segment in &parsed.segments[1..] {
            unqualified_segments.push(segment.as_str().to_owned());
        }
        package::InPackageResolver::resolve_path(
            &package,
            &InPackagePath::new(unqualified_segments),
            namespace,
            ResolutionRules::rust(),
        )
    }

    pub fn resolve_package(
        &self,
        package_id: &PackageId,
        hir_package: &mut hir::HirPackage,
    ) -> fp_core::error::Result<()> {
        let package = self.program.get_ast_package(package_id);
        let (package_id, items) = {
            let package = package.borrow();
            (package.package_id.clone(), package.items())
        };
        let mut resolver = package::InPackageResolver::new(
            package_id,
            hir_package,
            self.program.provider().declaration_rules(),
            self.program.provider().resolution_rules(),
            Rc::clone(&self.program),
        );
        resolver.collect_package_items(&items);
        let mut worklist = worklist::ResolutionWorklist::default();
        resolver.collect_imports(&items, &mut worklist);
        resolver.resolve_worklist(&mut worklist);
        drop(resolver);
        self.hir_program
            .borrow_mut()
            .add_package(Rc::new(RefCell::new(hir_package.clone())));
        Ok(())
    }
}
