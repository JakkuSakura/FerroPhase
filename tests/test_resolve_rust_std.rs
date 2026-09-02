use std::rc::Rc;
use std::sync::Arc;

use fp_core::ast::Path;
use fp_core::ast::package::PackageId;
use fp_core::ast::package::provider::PackageProvider;
use fp_core::ast::package::{AstPackage, PackageDescriptor};
use fp_core::ast::path::{InPackagePath, PathPrefix};
use fp_core::ast::program::AstProgram;
use fp_core::ast::{Item, ItemKind};
use fp_core::hir::HirPackage;
use fp_core::hir::HirProgram;
use fp_core::hir::resolve::{Namespace, ResolutionResult};
use fp_core::lir::LirDataLayout;
use fp_resolve::Resolver;
use fp_rust::RustStdProvider;

#[test]
fn resolves_items_from_external_package() {
    let prepared = prepare_std();
    let current_package = register_external_package(&prepared);
    let mut failures = Vec::new();

    for package_id in ["core", "alloc", "libc", "std"] {
        let items = known_items(&prepared, &PackageId::new(package_id));
        assert_resolved_items(
            &prepared,
            &current_package,
            package_id,
            items,
            true,
            &mut failures,
        );
    }

    assert_failures("external package", failures);
}

#[test]
fn resolves_items_within_std_package() {
    let prepared = prepare_std();
    let std_package = PackageId::new("std");
    let mut failures = Vec::new();
    assert_resolved_items(
        &prepared,
        &std_package,
        "std",
        known_items(&prepared, &std_package),
        false,
        &mut failures,
    );
    assert_failures("std package", failures);
}

struct PreparedStd {
    program: Rc<AstProgram>,
    hir_program: Rc<std::cell::RefCell<HirProgram>>,
    resolver: Resolver,
}

fn prepare_std() -> PreparedStd {
    let provider = Arc::new(RustStdProvider);
    let package_ids = ["core", "alloc", "libc", "std"]
        .into_iter()
        .map(PackageId::new)
        .collect::<Vec<_>>();
    let program = Rc::new(AstProgram::new(provider.clone()));
    for package_id in &package_ids {
        let source = provider
            .load_package_source(package_id)
            .unwrap_or_else(|error| panic!("failed to load `{package_id}`: {error}"));
        program.begin_package(package_id.clone(), source, LirDataLayout::x86_64());
    }
    let hir_program = Rc::new(std::cell::RefCell::new(HirProgram::new()));
    let resolver = Resolver::new(Rc::clone(&program), Rc::clone(&hir_program));
    for package_id in &package_ids {
        let mut hir_package = HirPackage::new(package_id.clone());
        resolver
            .resolve_package(package_id, &mut hir_package)
            .unwrap_or_else(|error| panic!("failed to resolve `{package_id}`: {error}"));
        hir_program.borrow_mut().publish_package(hir_package);
    }
    PreparedStd {
        program,
        hir_program,
        resolver,
    }
}

fn register_external_package(prepared: &PreparedStd) -> PackageId {
    let package_id = PackageId::new("external_consumer");
    let source = AstPackage::new(
        package_id.clone(),
        package_id.as_str(),
        PackageDescriptor::empty(package_id.clone(), package_id.as_str()),
        fp_core::ast::Module {
            attrs: Vec::new(),
            name: fp_core::ast::Ident::new(""),
            items: Vec::new(),
            visibility: fp_core::ast::Visibility::Public,
            is_external: false,
        },
    );
    prepared
        .program
        .begin_package(package_id.clone(), source, LirDataLayout::x86_64());
    let mut hir_package = HirPackage::new(package_id.clone());
    prepared
        .resolver
        .resolve_package(&package_id, &mut hir_package)
        .expect("failed to resolve external package");
    prepared
        .hir_program
        .borrow_mut()
        .publish_package(hir_package);
    package_id
}

fn known_items(prepared: &PreparedStd, package_id: &PackageId) -> Vec<(InPackagePath, Item)> {
    let package = prepared.program.get_ast_package(package_id);
    let package = package.borrow();
    let mut items = Vec::new();
    collect_known_items(
        &package.module.items,
        &InPackagePath::new(Vec::new()),
        &mut items,
    );
    items
}

fn assert_resolved_items(
    prepared: &PreparedStd,
    current_package: &PackageId,
    owner_package: &str,
    items: Vec<(InPackagePath, Item)>,
    external: bool,
    failures: &mut Vec<String>,
) {
    for (path, item) in items {
        let mut segments = if external {
            vec![owner_package.to_owned()]
        } else {
            Vec::new()
        };
        segments.extend(path.segments.iter().cloned());
        let mut resolved = false;
        let mut ambiguous = false;
        for namespace in [Namespace::Type, Namespace::Value, Namespace::Macro] {
            match prepared.resolver.resolve_parsed_path(
                current_package,
                &InPackagePath::new(Vec::new()),
                &Path::new(
                    PathPrefix::Root,
                    segments.iter().cloned().map(Into::into).collect(),
                ),
                namespace,
            ) {
                ResolutionResult::Found(_) => resolved = true,
                ResolutionResult::Ambiguous => ambiguous = true,
                ResolutionResult::NotFound(_) => {}
            }
        }
        let name = format!("{}::{}", owner_package, path.segments.join("::"));
        if !resolved && ambiguous {
            eprintln!("{name} is ambiguous during resolution");
        } else if !resolved {
            let message = format!("`{name}` was parsed but not registered by resolution");
            eprintln!("{message}");
            failures.push(message);
        }
        let _ = item;
    }
}

fn assert_failures(scope: &str, failures: Vec<String>) {
    assert!(
        failures.is_empty(),
        "{} {scope} resolution checks failed:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

fn collect_known_items(
    items: &[Item],
    module_path: &InPackagePath,
    output: &mut Vec<(InPackagePath, Item)>,
) {
    for item in items {
        match item.kind() {
            ItemKind::Module(module) => {
                collect_known_items(
                    &module.items,
                    &module_path.with_segment(module.name.as_str().to_owned()),
                    output,
                );
            }
            ItemKind::Impl(_) => {}
            _ if item.get_ident().is_some() => {
                let mut path = module_path.clone();
                path.push(item.get_ident().unwrap().as_str().to_owned());
                output.push((path, item.clone()));
            }
            _ => {}
        }
    }
}
