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
        let root = InPackagePath::new(Vec::new());
        let rules = self.program.provider().resolution_rules();
        let result = package
            .module_tree
            .resolve_path(&root, &absolute, namespace, rules);
        if !result.is_not_found() {
            return result;
        }
        let unqualified = if external_package.as_ref() == Some(&target_package_id) {
            Some(InPackagePath::new(
                parsed.segments[1..]
                    .iter()
                    .map(|segment| segment.as_str().to_owned())
                    .collect(),
            ))
        } else {
            None
        };
        let result = if let Some(path) = &unqualified {
            package
                .module_tree
                .resolve_path(&root, path, namespace, rules)
        } else {
            result
        };
        if !result.is_not_found() {
            return result;
        }
        if parsed.segments.len() > 1 {
            let type_namespace = Namespace::Type;
            let absolute_prefix = InPackagePath::new(
                absolute
                    .segments
                    .iter()
                    .take(absolute.segments.len() - 1)
                    .cloned()
                    .collect(),
            );
            let mut prefix_result =
                package
                    .module_tree
                    .resolve_path(&root, &absolute_prefix, type_namespace, rules);
            if prefix_result.is_not_found() {
                if let Some(unqualified) = &unqualified {
                    let prefix = InPackagePath::new(
                        unqualified
                            .segments
                            .iter()
                            .take(unqualified.segments.len().saturating_sub(1))
                            .cloned()
                            .collect(),
                    );
                    prefix_result =
                        package
                            .module_tree
                            .resolve_path(&root, &prefix, type_namespace, rules);
                }
            }
            if let ResolutionResult::Found(hir::Res::Def(type_def_id)) = prefix_result {
                return ResolutionResult::Found(hir::Res::Def(type_def_id));
            }
        }
        if external_package.as_ref() != Some(&target_package_id) {
            return ResolutionResult::NotFound(fp_core::hir::resolve::ResolutionNotFound::Package(
                target_package_id,
            ));
        }
        result
    }
}
