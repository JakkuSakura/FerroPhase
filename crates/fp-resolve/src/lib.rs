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

pub mod local;
pub mod package;
pub mod worklist;

pub struct Resolver {
    hir_program: Rc<RefCell<HirProgram>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::ast::package::provider::EmptyProvider;
    use fp_core::hir::resolve::{ModuleData, ResolutionNotFound};
    use std::sync::Arc;

    fn setup() -> (Resolver, Rc<RefCell<HirProgram>>, PackageId, hir::DefId) {
        let program = Rc::new(RefCell::new(HirProgram::new()));
        let package_id = PackageId::new("app");
        let package = Rc::new(RefCell::new(hir::HirPackage::new(package_id.clone())));
        let root = ModuleData::virtual_root_for(package_id.clone());
        package
            .borrow_mut()
            .module_data
            .set_children(root.clone(), Vec::new());
        program.borrow_mut().add_package(package);
        let resolver = Resolver::new(
            Rc::new(AstProgram::new(Arc::new(EmptyProvider))),
            Rc::clone(&program),
        );
        (resolver, program, package_id, root)
    }

    fn add_package(program: &Rc<RefCell<HirProgram>>, id: &str) -> hir::DefId {
        let package_id = PackageId::new(id);
        let root = ModuleData::virtual_root_for(package_id.clone());
        let package = Rc::new(RefCell::new(hir::HirPackage::new(package_id)));
        package
            .borrow_mut()
            .module_data
            .set_children(root.clone(), Vec::new());
        program.borrow_mut().add_package(package);
        root
    }

    #[test]
    fn resolves_paths_across_packages_and_absolute_roots() {
        let (resolver, program, app, app_root) = setup();
        let std_id = PackageId::new("std");
        let std_root = add_package(&program, "std");
        let alloc = hir::DefId::local(10);
        program
            .borrow()
            .package_rc(&std_id)
            .unwrap()
            .borrow_mut()
            .module_data
            .add_child(
                std_root.clone(),
                "alloc",
                Namespace::Type,
                hir::Res::Def(alloc.clone()),
            );
        let path = Path::new(PathPrefix::Root, vec!["std".into(), "alloc".into()]);
        assert_eq!(
            resolver.resolve_parsed_path(
                &app,
                &InPackagePath::new(Vec::new()),
                &path,
                Namespace::Type
            ),
            ResolutionResult::Found(hir::Path {
                res: hir::Res::Def(alloc),
                segments: Vec::new()
            })
        );
        let plain = Path::new(PathPrefix::Plain, vec!["std".into(), "alloc".into()]);
        assert!(matches!(
            resolver.resolve_parsed_path(
                &app,
                &InPackagePath::new(Vec::new()),
                &plain,
                Namespace::Type
            ),
            ResolutionResult::Found(_)
        ));
        let _ = app_root;
    }

    #[test]
    fn resolves_crate_self_and_super_prefixes() {
        let (resolver, program, app, root) = setup();
        let module = hir::DefId::new(app.clone(), 2);
        let item = hir::DefId::new(app.clone(), 3);
        let package = program.borrow().package_rc(&app).unwrap();
        package.borrow_mut().module_data.set_children(
            module.clone(),
            vec![("Item".into(), Namespace::Value, hir::Res::Def(item.clone()))],
        );
        package.borrow_mut().module_data.add_child(
            root.clone(),
            "m",
            Namespace::Type,
            hir::Res::Module(module),
        );
        let location = InPackagePath::new(vec!["m".into()]);
        let crate_path = Path::new(PathPrefix::Crate, vec!["m".into(), "Item".into()]);
        assert!(matches!(
            resolver.resolve_parsed_path(&app, &location, &crate_path, Namespace::Value),
            ResolutionResult::Found(_)
        ));
        let self_path = Path::new(PathPrefix::SelfMod, vec!["Item".into()]);
        assert!(matches!(
            resolver.resolve_parsed_path(&app, &location, &self_path, Namespace::Value),
            ResolutionResult::Found(_)
        ));
        let super_path = Path::new(PathPrefix::Super(1), vec!["m".into(), "Item".into()]);
        assert!(matches!(
            resolver.resolve_parsed_path(&app, &location, &super_path, Namespace::Value),
            ResolutionResult::Found(_)
        ));
    }

    #[test]
    fn returns_projection_tail_for_non_module_base() {
        let (resolver, program, app, root) = setup();
        let ty = hir::DefId::local(4);
        let package = program.borrow().package_rc(&app).unwrap();
        package.borrow_mut().module_data.add_child(
            root,
            "T",
            Namespace::Type,
            hir::Res::Generic(ty.clone()),
        );
        let path = Path::new(PathPrefix::Plain, vec!["T".into(), "Assoc".into()]);
        assert_eq!(
            resolver.resolve_parsed_path(
                &app,
                &InPackagePath::new(Vec::new()),
                &path,
                Namespace::Type
            ),
            ResolutionResult::Found(hir::Path {
                res: hir::Res::Generic(ty),
                segments: vec![hir::PathSegment {
                    name: "Assoc".into(),
                    args: None
                }]
            })
        );
    }

    #[test]
    fn reports_invalid_parent_and_missing_package() {
        let (resolver, _program, app, _root) = setup();
        let path = Path::new(PathPrefix::Super(2), vec!["x".into()]);
        assert!(matches!(
            resolver.resolve_parsed_path(
                &app,
                &InPackagePath::new(vec!["m".into()]),
                &path,
                Namespace::Type
            ),
            ResolutionResult::NotFound(ResolutionNotFound::InvalidParent { .. })
        ));
        let missing = Path::new(PathPrefix::Root, vec!["missing".into(), "Thing".into()]);
        assert!(matches!(
            resolver.resolve_parsed_path(
                &app,
                &InPackagePath::new(Vec::new()),
                &missing,
                Namespace::Type
            ),
            ResolutionResult::NotFound(_)
        ));
    }
}

impl Resolver {
    pub fn new(_program: Rc<AstProgram>, hir_program: Rc<RefCell<HirProgram>>) -> Self {
        Self { hir_program }
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
                ResolutionResult::Found(path) if let hir::Res::Module(start) = path.res.clone() => {
                    module = start
                }
                result => return result,
            }
        }
        let first = usize::from(skip_external);
        let count = parsed.segments.len().saturating_sub(first);
        if count == 0 {
            return ResolutionResult::Found(hir::Path {
                res: hir::Res::Module(module),
                segments: Vec::new(),
            });
        }
        let mut last_result = ResolutionResult::NotFound(ResolutionNotFound::EmptyPath);
        for (offset, segment) in parsed.segments.iter().skip(first).enumerate() {
            let segment_namespace = if offset + 1 == count {
                namespace
            } else {
                Namespace::Type
            };
            let result =
                hir_program.resolve_module_child(&module, segment.name.as_str(), segment_namespace);
            last_result = result.clone();
            if offset + 1 == count {
                return result;
            }
            match result {
                ResolutionResult::Found(path) if let hir::Res::Module(next) = path.res.clone() => {
                    module = next
                }
                ResolutionResult::Found(found) => {
                    // A non-module segment is the resolved base (for
                    // example `Vec` in `Vec::new` or `Trait::Assoc`).  Keep
                    // the remaining path segments for type checking to
                    // resolve as associated items instead of treating them
                    // as an invalid module traversal.
                    return ResolutionResult::Found(hir::Path {
                        res: found.res,
                        segments: parsed
                            .segments
                            .iter()
                            .skip(first + offset + 1)
                            .map(|segment| hir::PathSegment {
                                name: segment.name.clone().into(),
                                args: None,
                            })
                            .collect(),
                    });
                }
                _ => break,
            }
        }
        last_result
    }
}
