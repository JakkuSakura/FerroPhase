use std::rc::Rc;
use std::sync::Arc;

use fp_core::ast::Path;
use fp_core::ast::package::PackageId;
use fp_core::ast::package::provider::PackageProvider;
use fp_core::ast::path::PathPrefix;
use fp_core::ast::program::AstProgram;
use fp_core::hir::HirPackage;
use fp_core::hir::HirProgram;
use fp_core::hir::resolve::{Namespace, ResolutionResult};
use fp_core::lir::LirDataLayout;
use fp_resolve::Resolver;
use fp_rust::RustStdProvider;

#[test]
fn resolves_every_named_rust_std_declaration() {
    let provider = Arc::new(RustStdProvider);
    // Load and resolve crates in dependency order so every dependency is
    // available before a package that references it is resolved.
    let package_ids = ["core", "alloc", "libc", "std", "test"]
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
    let resolver = Resolver::new(Rc::clone(&program), hir_program);
    let mut failures = Vec::new();
    for package_id in &package_ids {
        let mut hir_package = HirPackage::new(package_id.clone());
        resolver
            .resolve_package(package_id, &mut hir_package)
            .unwrap_or_else(|error| panic!("failed to resolve `{package_id}`: {error}"));
    }

    for package_id in &package_ids {
        let package = program.get_ast_package(package_id);
        let package_items = package.borrow().items();

        let mut checked = 0usize;
        for package_item in package_items {
            let Some(name) = package_item.item.get_ident() else {
                continue;
            };
            let resolved = [Namespace::Type, Namespace::Value, Namespace::Macro]
                .into_iter()
                .any(|namespace| {
                    matches!(
                        resolver.resolve_parsed_path(
                            package_id,
                            &package_item.module_path,
                            &Path::new(PathPrefix::Plain, vec![name.as_str().into()],),
                            namespace,
                        ),
                        ResolutionResult::Found(_)
                    )
                });
            if !resolved {
                let message = format!(
                    "`{package_id}::{}` was parsed but not registered by resolution",
                    name.as_str()
                );
                eprintln!("{message}");
                failures.push(message);
            }
            checked += 1;
        }

        if checked == 0 {
            failures.push(format!("`{package_id}` produced no named declarations"));
        }
    }

    assert!(
        failures.is_empty(),
        "{} resolution checks failed:\n{}",
        failures.len(),
        failures.join("\n")
    );
}
