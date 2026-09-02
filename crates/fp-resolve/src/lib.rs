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
        use fp_core::hir::resolve::ResolutionNotFound;
        if parsed.is_empty() {
            return ResolutionResult::NotFound(ResolutionNotFound::EmptyPath);
        }
        let hir_program = self.hir_program.borrow();
        let external_package = parsed.head().and_then(|head| {
            hir_program
                .packages
                .keys()
                .find(|id| hir::HirProgram::external_crate_name(id) == head)
                .cloned()
        });
        let target_package_id = external_package
            .clone()
            .unwrap_or_else(|| current_package_id.clone());
        let root = fp_core::hir::resolve::ModuleData::virtual_root_for(target_package_id.clone());
        let mut module = root;
        let skip_external = external_package.is_some()
            && matches!(parsed.prefix, PathPrefix::Plain | PathPrefix::Root);
        if !skip_external && !matches!(parsed.prefix, PathPrefix::Root | PathPrefix::Crate) {
            let start_segments = match parsed.prefix {
                PathPrefix::Super(depth) => {
                    let Some(parent) = location.segments.len().checked_sub(depth) else {
                        return ResolutionResult::NotFound(ResolutionNotFound::InvalidParent {
                            location: location.clone(),
                            depth,
                        });
                    };
                    &location.segments[..parent]
                }
                _ => &location.segments,
            };
            match hir_program.resolve_module_location_segments(&target_package_id, start_segments) {
                ResolutionResult::Found(hir::Res::Module(start)) => module = start,
                result => return result,
            }
        }
        let first = usize::from(skip_external);
        let count = parsed.segments.len().saturating_sub(first);
        if count == 0 {
            return ResolutionResult::Found(hir::Res::Module(module));
        }
        for (offset, segment) in parsed.segments.iter().skip(first).enumerate() {
            let segment_namespace = if offset + 1 == count {
                namespace
            } else {
                Namespace::Type
            };
            let result = hir_program.resolve_module_child(&module, segment, segment_namespace);
            if offset + 1 == count {
                return result;
            }
            match result {
                ResolutionResult::Found(hir::Res::Module(next)) => module = next,
                ResolutionResult::Found(found) => {
                    return ResolutionResult::NotFound(ResolutionNotFound::ExpectedModule {
                        path: location.clone(),
                        found,
                    });
                }
                _ => break,
            }
        }
        result
    }
}
