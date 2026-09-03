use std::cell::RefCell;
use std::panic::AssertUnwindSafe;
use std::rc::Rc;
use std::sync::Arc;

use fp_compiler::CompilerExecutor;
use fp_core::ast::package::PackageId;
use fp_core::ast::package::provider::PackageProvider;
use fp_core::ast::program::AstProgram;
use fp_core::hir::{HirPackage, HirProgram};
use fp_core::lir::LirDataLayout;
use fp_rust::RustStdProvider;
use fp_typing::HirTypeChecker;
use fp_typing::ComptimeResolver;

#[test]
fn type_checks_rust_std_packages_without_stopping_at_first_error() {
    let provider = Arc::new(RustStdProvider);
    let package_ids = ["core", "alloc", "libc", "std"]
        .into_iter()
        .map(PackageId::new)
        .collect::<Vec<_>>();
    let ast_program = Rc::new(AstProgram::new(provider.clone()));
    for package_id in &package_ids {
        let source = provider
            .load_package_source(package_id)
            .unwrap_or_else(|error| panic!("failed to load `{package_id}`: {error}"));
        ast_program.begin_package(package_id.clone(), source, LirDataLayout::x86_64());
    }

    let hir_program = Rc::new(RefCell::new(HirProgram::new()));
    let shared_hir_program = fp_core::hir::SharedHirProgram::new(HirProgram::new());
    let mut lowering_failures = Vec::new();
    for package_id in &package_ids {
        let package = Rc::new(RefCell::new(HirPackage::new(package_id.clone())));
        hir_program.borrow_mut().add_package(Rc::clone(&package));
        shared_hir_program.add_package(Rc::clone(&package));
        let source = ast_program.get_ast_package(package_id).borrow().clone();
        let mut lowerer = fp_backend::transformations::AstToHirLowerer::new(
            Rc::clone(&ast_program),
            shared_hir_program.clone(),
            package_id.clone(),
        )
        .with_lowering_config(fp_backend::transformations::HirLoweringConfig {
            resolution_only: true,
            ..Default::default()
        });
        let lowered =
            match std::panic::catch_unwind(AssertUnwindSafe(|| lowerer.transform_package(&source)))
            {
                Ok(Ok(package)) => package,
                Ok(Err(error)) => {
                    lowering_failures.push(format!("`{package_id}`: {error}"));
                    continue;
                }
                Err(_) => {
                    lowering_failures.push(format!("`{package_id}`: lowering panicked"));
                    continue;
                }
            };
        *package.borrow_mut() = lowered;
    }

    for package_id in package_ids {
        let package = hir_program
            .borrow()
            .package_rc(&package_id)
            .expect("lowered package was not published");
        let dependency_program = Rc::new(hir_program.borrow().clone());
        let executor = CompilerExecutor::new();
        let comptime_resolver: ComptimeResolver =
            Rc::new(|_request| Box::pin(async { Ok(fp_core::ast::Value::unit()) }));
        let checker = HirTypeChecker::new(
            package.borrow().clone(),
            Some(dependency_program),
            Some(comptime_resolver),
            executor.handle(),
        );
        let item_ids = checker
            .borrow()
            .package()
            .items
            .iter()
            .map(|item| item.def_id.clone())
            .collect::<Vec<_>>();
        let handles = item_ids
            .into_iter()
            .map(|def_id| HirTypeChecker::spawn_item_task(&checker, def_id))
            .collect::<Vec<_>>();
        executor.run(async {
            for handle in handles {
                handle.await;
            }
        });

        let diagnostics = checker
            .borrow()
            .finish()
            .borrow()
            .diagnostics
            .get_diagnostics();
        eprintln!(
            "typecheck `{package_id}`: {} diagnostic(s)",
            diagnostics.len()
        );
        for diagnostic in diagnostics.iter().take(20) {
            eprintln!("  {:?}: {}", diagnostic.level, diagnostic.message);
        }
    }

    if !lowering_failures.is_empty() {
        eprintln!(
            "Rust std lowering/type-check preparation failures:\n{}",
            lowering_failures.join("\n")
        );
    }
}
