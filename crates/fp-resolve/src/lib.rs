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
        program
            .borrow()
            .package_rc(&std_id)
            .unwrap()
            .borrow_mut()
            .module_data
            .add_child(
                std_root.clone(),
                "alloc_alias",
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
                span: Default::default(),
                res: hir::Res::Def(alloc.clone()),
                segments: vec![
                    hir::PathSegment {
                        ident: "std".into(),
                        hir_id: Default::default(),
                        args: None,
                        infer_args: true,
                        res: hir::Res::Module(std_root),
                    },
                    hir::PathSegment {
                        ident: "alloc".into(),
                        hir_id: Default::default(),
                        args: None,
                        infer_args: true,
                        res: hir::Res::Def(alloc),
                    },
                ]
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
        let alias = Path::new(PathPrefix::Root, vec!["std".into(), "alloc_alias".into()]);
        assert!(matches!(
            resolver.resolve_parsed_path(
                &app,
                &InPackagePath::new(Vec::new()),
                &alias,
                Namespace::Type
            ),
            ResolutionResult::Found(hir::Path {
                res: hir::Res::Def(_),
                segments: _,
                ..
            })
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
            hir::Res::Module(module.clone()),
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
        let nested_location = InPackagePath::new(vec!["m".into(), "n".into()]);
        let super_two = Path::new(PathPrefix::Super(2), vec!["m".into(), "Item".into()]);
        assert!(matches!(
            resolver.resolve_parsed_path(&app, &nested_location, &super_two, Namespace::Value),
            ResolutionResult::Found(_)
        ));
    }

    #[test]
    fn returns_resolved_base_for_non_module_path() {
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
                span: Default::default(),
                res: hir::Res::Generic(ty.clone()),
                segments: vec![hir::PathSegment {
                    ident: "T".into(),
                    hir_id: Default::default(),
                    args: None,
                    infer_args: true,
                    res: hir::Res::Generic(ty),
                },]
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

    #[test]
    fn continues_through_modules_and_keeps_namespaces_distinct() {
        let (resolver, program, app, root) = setup();
        let module = hir::DefId::new(app.clone(), 20);
        let value = hir::DefId::new(app.clone(), 21);
        let ty = hir::DefId::new(app.clone(), 22);
        let mac = hir::DefId::new(app.clone(), 23);
        let package = program.borrow().package_rc(&app).unwrap();
        package.borrow_mut().module_data.set_children(
            module.clone(),
            vec![
                (
                    "Thing".into(),
                    Namespace::Value,
                    hir::Res::Def(value.clone()),
                ),
                ("Thing".into(), Namespace::Type, hir::Res::Def(ty.clone())),
                ("Thing".into(), Namespace::Macro, hir::Res::Def(mac.clone())),
            ],
        );
        package.borrow_mut().module_data.add_child(
            root,
            "nested",
            Namespace::Type,
            hir::Res::Module(module.clone()),
        );
        let location = InPackagePath::new(Vec::new());
        let path = Path::new(PathPrefix::Plain, vec!["nested".into(), "Thing".into()]);
        assert_eq!(
            resolver.resolve_parsed_path(&app, &location, &path, Namespace::Value),
            ResolutionResult::Found(hir::Path {
                span: Default::default(),
                res: hir::Res::Def(value.clone()),
                segments: vec![
                    hir::PathSegment {
                        ident: "nested".into(),
                        hir_id: Default::default(),
                        args: None,
                        infer_args: true,
                        res: hir::Res::Module(module.clone()),
                    },
                    hir::PathSegment {
                        ident: "Thing".into(),
                        hir_id: Default::default(),
                        args: None,
                        infer_args: true,
                        res: hir::Res::Def(value),
                    },
                ]
            })
        );
        assert_eq!(
            resolver.resolve_parsed_path(&app, &location, &path, Namespace::Type),
            ResolutionResult::Found(hir::Path {
                span: Default::default(),
                res: hir::Res::Def(ty.clone()),
                segments: vec![
                    hir::PathSegment {
                        ident: "nested".into(),
                        hir_id: Default::default(),
                        args: None,
                        infer_args: true,
                        res: hir::Res::Module(module.clone()),
                    },
                    hir::PathSegment {
                        ident: "Thing".into(),
                        hir_id: Default::default(),
                        args: None,
                        infer_args: true,
                        res: hir::Res::Def(ty),
                    },
                ]
            })
        );
        assert_eq!(
            resolver.resolve_parsed_path(&app, &location, &path, Namespace::Macro),
            ResolutionResult::Found(hir::Path {
                span: Default::default(),
                res: hir::Res::Def(mac.clone()),
                segments: vec![
                    hir::PathSegment {
                        ident: "nested".into(),
                        hir_id: Default::default(),
                        args: None,
                        infer_args: true,
                        res: hir::Res::Module(module),
                    },
                    hir::PathSegment {
                        ident: "Thing".into(),
                        hir_id: Default::default(),
                        args: None,
                        infer_args: true,
                        res: hir::Res::Def(mac),
                    },
                ]
            })
        );
    }

    #[test]
    fn reports_ambiguity_and_read_only_lookup_does_not_mutate_modules() {
        let (resolver, program, app, root) = setup();
        let package = program.borrow().package_rc(&app).unwrap();
        package.borrow_mut().module_data.add_child(
            root.clone(),
            "dup",
            Namespace::Value,
            hir::Res::Def(hir::DefId::new(app.clone(), 30)),
        );
        package.borrow_mut().module_data.add_child(
            root.clone(),
            "dup",
            Namespace::Value,
            hir::Res::Def(hir::DefId::new(app.clone(), 31)),
        );
        let path = Path::new(PathPrefix::Plain, vec!["dup".into()]);
        assert_eq!(
            resolver.resolve_parsed_path(
                &app,
                &InPackagePath::new(Vec::new()),
                &path,
                Namespace::Value
            ),
            ResolutionResult::Ambiguous
        );
        assert_eq!(
            package
                .borrow()
                .module_data
                .resolve_child(&root, "new_name", Namespace::Value),
            ResolutionResult::NotFound(ResolutionNotFound::Symbol {
                module: InPackagePath::new(Vec::new()),
                symbol: "new_name".into(),
                namespace: Namespace::Value
            })
        );
    }
}

impl Resolver {
    pub fn new(_program: Rc<AstProgram>, hir_program: Rc<RefCell<HirProgram>>) -> Self {
        Self { hir_program }
    }

    /// Read-only lookup of a parsed source path at its lexical location.
    /// `resolve_package` populates the HIR; this method only computes the
    /// absolute in-package path and queries it. For a path whose suffix is
    /// type-relative, the returned HIR path contains only the resolved base;
    /// lowering represents the suffix with nested `QPath::TypeRelative`
    /// nodes.
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
        let skip_external = external_package.is_some()
            && matches!(parsed.prefix, PathPrefix::Plain | PathPrefix::Root);
        let start_segments = match parsed.prefix {
            PathPrefix::Super(depth) => {
                let Some(parent) = location.segments.len().checked_sub(depth) else {
                    return ResolutionResult::NotFound(ResolutionNotFound::InvalidParent {
                        location: location.clone(),
                        depth,
                    });
                };
                location.segments[..parent].to_vec()
            }
            _ => location.segments.clone(),
        };
        // A plain path in a nested module can refer to a private item imported
        // by an ancestor module. This is ordinary Rust lexical lookup, not a
        // suffix search: once a candidate ancestor provides the first segment
        // but a later segment fails, lookup stops at that shadowing binding.
        let search_ancestors = matches!(parsed.prefix, PathPrefix::Plain) && !skip_external;
        let mut start_len = start_segments.len();
        let first = usize::from(skip_external);
        let count = parsed.segments.len().saturating_sub(first);
        if count == 0 {
            return ResolutionResult::Found(hir::Path {
                span: Default::default(),
                res: hir::Res::Module(root.clone()),
                segments: parsed
                    .segments
                    .iter()
                    .take(first)
                    .map(|segment| hir::PathSegment {
                        ident: segment.ident.name.clone().into(),
                        hir_id: Default::default(),
                        args: None,
                        infer_args: true,
                        res: hir::Res::Module(root.clone()),
                    })
                    .collect(),
            });
        }

        'ancestor: loop {
            let mut module = root.clone();
            if !skip_external && !matches!(parsed.prefix, PathPrefix::Root | PathPrefix::Crate) {
                match hir_program.resolve_module_location_segments(
                    &target_package_id,
                    &start_segments[..start_len],
                ) {
                    ResolutionResult::Found(path)
                        if let hir::Res::Module(start) = path.res.clone() =>
                    {
                        module = start;
                    }
                    result => {
                        if search_ancestors && start_len > 0 {
                            start_len -= 1;
                            continue 'ancestor;
                        }
                        return result;
                    }
                }
            }

            let mut resolved_segments: Vec<hir::PathSegment> = parsed
                .segments
                .iter()
                .take(first)
                .map(|segment| hir::PathSegment {
                    ident: segment.ident.name.clone().into(),
                    hir_id: Default::default(),
                    args: None,
                    infer_args: true,
                    res: hir::Res::Module(root.clone()),
                })
                .collect();
            for (offset, segment) in parsed.segments.iter().skip(first).enumerate() {
                let segment_namespace = if offset + 1 == count {
                    namespace
                } else {
                    Namespace::Type
                };
                let result = hir_program.resolve_module_child(
                    &module,
                    segment.ident.as_str(),
                    segment_namespace,
                );
                match result {
                    ResolutionResult::Found(path) => {
                        let resolved = path.res.clone();
                        resolved_segments.push(hir::PathSegment {
                            ident: segment.ident.name.clone().into(),
                            hir_id: Default::default(),
                            args: None,
                            infer_args: true,
                            res: resolved.clone(),
                        });
                        if offset + 1 == count {
                            return ResolutionResult::Found(hir::Path {
                                span: Default::default(),
                                res: resolved,
                                segments: resolved_segments,
                            });
                        }
                        if let hir::Res::Module(next) = path.res {
                            module = next;
                        } else {
                            // Keep only the resolved base in the ordinary
                            // path. Rustc represents every remaining
                            // associated-item segment as a nested
                            // `QPath::TypeRelative` node during lowering.
                            return ResolutionResult::Found(hir::Path {
                                span: Default::default(),
                                res: resolved,
                                segments: resolved_segments,
                            });
                        }
                    }
                    result => {
                        if search_ancestors && offset == 0 && start_len > 0 {
                            start_len -= 1;
                            continue 'ancestor;
                        }
                        return result;
                    }
                }
            }
        }
    }
}
