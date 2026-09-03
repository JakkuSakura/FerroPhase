use std::cell::RefCell;
use std::panic::AssertUnwindSafe;
use std::rc::Rc;
use std::sync::Arc;

use fp_compiler::CompilerExecutor;
use fp_core::ast::package::PackageId;
use fp_core::ast::package::provider::PackageProvider;
use fp_core::ast::program::AstProgram;
use fp_core::diagnostics::DiagnosticLevel;
use fp_core::hir::HirProgram;
use fp_core::lir::LirDataLayout;
use fp_rust::RustStdProvider;
use fp_typing::ComptimeResolver;
use fp_typing::HirTypeChecker;

#[test]
fn type_checks_rust_std_packages_without_stopping_at_first_error() {
    let provider = Arc::new(RustStdProvider);
    let package_ids = dependency_closure(&provider, &[PackageId::new("std")]);
    let ast_program = Rc::new(AstProgram::new(provider.clone()));
    for package_id in &package_ids {
        let source = provider
            .load_package_source(package_id)
            .unwrap_or_else(|error| panic!("failed to load `{package_id}`: {error}"));
        ast_program.begin_package(package_id.clone(), source, LirDataLayout::x86_64());
    }

    let hir_program = Rc::new(RefCell::new(HirProgram::new()));
    let shared_hir_program = fp_core::hir::SharedHirProgram::from(Rc::clone(&hir_program));
    let mut lowering_failures = Vec::new();
    for package_id in &package_ids {
        let source = ast_program.get_ast_package(package_id).borrow().clone();
        let mut lowerer = fp_backend::transformations::AstToHirLowerer::new(
            Rc::clone(&ast_program),
            shared_hir_program.clone(),
            package_id.clone(),
        );
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
        let lowering_diagnostics = lowerer.take_diagnostics().get_diagnostics();
        eprintln!(
            "lowering `{package_id}` produced {} diagnostic(s)",
            lowering_diagnostics.len()
        );
        let lowering_diagnostic_limit = if std::env::var_os("FP_STD_ALL_DIAGNOSTICS").is_some() {
            usize::MAX
        } else {
            20
        };
        for diagnostic in lowering_diagnostics.iter().take(lowering_diagnostic_limit) {
            let location = diagnostic
                .span
                .and_then(|span| {
                    fp_core::source_map::source_map()
                        .file(span.file)
                        .map(|file| {
                            let (line, column) = file.line_col(span.lo);
                            format!("{}:{line}:{column}", file.path.display())
                        })
                })
                .unwrap_or_else(|| "<unknown>".to_owned());
            eprintln!(
                "  {:?}: {} at {location}",
                diagnostic.level, diagnostic.message
            );
        }
        let package = Rc::new(RefCell::new(lowered));
        hir_program.borrow_mut().add_package(Rc::clone(&package));
        shared_hir_program.add_package(package);
    }

    let mut total_diagnostics = 0usize;
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
        let error_count = diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.level == DiagnosticLevel::Error)
            .count();
        total_diagnostics += error_count;
        let successfully_typed_items = checker.borrow().successfully_typed_items();
        let failed_typed_items = checker.borrow().failed_typed_items();
        eprintln!(
            "typecheck `{package_id}`: {successfully_typed_items} successful + {failed_typed_items} failed = {} item(s) checked, {} diagnostic(s)",
            successfully_typed_items + failed_typed_items,
            error_count,
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

    assert!(
        total_diagnostics < 10_000,
        "Rust std type checking emitted {total_diagnostics} errors (threshold: 10000)"
    );
}

fn dependency_closure(provider: &RustStdProvider, roots: &[PackageId]) -> Vec<PackageId> {
    let mut ordered = Vec::new();
    let mut pending = roots.to_vec();
    while let Some(package_id) = pending.pop() {
        if ordered.contains(&package_id) {
            continue;
        }
        let metadata = provider
            .load_package_metadata(&package_id)
            .unwrap_or_else(|error| panic!("failed to load metadata for `{package_id}`: {error}"));
        let dependencies = metadata
            .metadata
            .dependencies
            .iter()
            .filter_map(|dependency| dependency.resolved_package_id.clone())
            .collect::<Vec<_>>();
        pending.extend(dependencies);
        ordered.push(package_id);
    }
    ordered.reverse();
    ordered
}
