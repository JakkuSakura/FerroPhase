use std::rc::Rc;
use std::sync::Arc;

use fp_core::ast::package::provider::PackageProvider;
use fp_core::ast::program::AstProgram;
use fp_core::hir::HirPackage;
use fp_core::hir::resolve::{Namespace, ResolutionResult, ResolutionRules};
use fp_core::lir::LirDataLayout;
use fp_resolve::Resolver;
use fp_rust::RustStdProvider;

#[test]
fn resolves_every_named_rust_std_declaration() {
    let provider = Arc::new(RustStdProvider);
    let package_ids = provider
        .list_packages()
        .expect("Rust std provider should enumerate its packages");
    let program = Rc::new(AstProgram::new(provider.clone()));

    for package_id in package_ids {
        let source = provider
            .load_package_source(&package_id)
            .unwrap_or_else(|error| panic!("failed to load `{package_id}`: {error}"));
        program.begin_package(package_id.clone(), source, LirDataLayout::x86_64());
    }

    let resolver = Resolver::new(Rc::clone(&program));
    for package_id in provider
        .list_packages()
        .expect("Rust std provider should enumerate its packages")
    {
        let package = program.get_ast_package(&package_id);
        let package_items = package.borrow().items();
        let mut hir_package = HirPackage::new(package_id.clone());
        resolver
            .resolve_package(&package_id, &mut hir_package)
            .unwrap_or_else(|error| panic!("failed to resolve `{package_id}`: {error}"));

        let mut checked = 0usize;
        for package_item in package_items {
            let Some(name) = package_item.item.get_ident() else {
                continue;
            };
            let resolved = [Namespace::Type, Namespace::Value, Namespace::Macro]
                .into_iter()
                .any(|namespace| {
                    matches!(
                        hir_package.module_tree.resolve(
                            &package_item.module_path,
                            name.as_str(),
                            namespace,
                            ResolutionRules::rust(),
                        ),
                        ResolutionResult::Found(_)
                    )
                });
            assert!(
                resolved,
                "`{package_id}::{}` was parsed but not registered by resolution",
                name.as_str()
            );
            checked += 1;
        }

        assert!(checked > 0, "`{package_id}` produced no named declarations");
        for prelude in &hir_package.prelude_modules {
            assert!(
                hir_package.module_tree.path_for_module(prelude).is_some(),
                "`{package_id}` recorded an unknown prelude module {prelude:?}"
            );
        }
    }
}
