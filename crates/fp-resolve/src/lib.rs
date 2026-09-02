//! AST name-resolution orchestration and temporary worklist state.
//!
//! Stable resolution data lives in `fp-core`; this crate owns the algorithms
//! that populate it before AST-to-HIR lowering.

use fp_core::ast::Path;
use fp_core::ast::package::PackageId;
use fp_core::ast::path::InPackagePath;
use fp_core::ast::program::AstProgram;
use fp_core::hir;
use fp_core::hir::HirProgram;
use fp_core::hir::resolve::{Namespace, ResolutionResult};
use std::cell::RefCell;
use std::rc::Rc;

pub mod package;
pub mod worklist;

pub struct Resolver {
    program: Rc<AstProgram>,
    hir_program: Rc<RefCell<HirProgram>>,
}

impl Resolver {
    pub fn new(program: Rc<AstProgram>, hir_program: Rc<RefCell<HirProgram>>) -> Self {
        Self {
            program,
            hir_program,
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
        let mut hir_package = hir::HirPackage::new(current_package_id.clone());
        let resolver = package::InPackageResolver::new(
            current_package_id.clone(),
            &mut hir_package,
            Rc::clone(&self.hir_program),
            self.program.provider().declaration_rules(),
            self.program.provider().resolution_rules(),
            Rc::clone(&self.program),
        );
        resolver.resolve_parsed_path(current_package_id, location, parsed, namespace)
    }
}
