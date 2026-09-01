use std::fs;

use fp_core::ast::package::provider::PackageProvider;
use tempfile::TempDir;

#[test]
fn directory_provider_loads_prql_documents() {
    let directory = TempDir::new().unwrap();
    fs::write(
        directory.path().join("query.prql"),
        "from employees | select {name}",
    )
    .unwrap();
    let provider = fp_prql::package::PrqlPackageProvider::discover(directory.path()).unwrap();
    let packages = provider.list_packages().unwrap();
    let package = provider.load_package_source(&packages[0]).unwrap();
    assert!(package.items.is_empty());
    assert_eq!(package.graph.modules().count(), 1);
    assert_eq!(
        provider
            .load_package_metadata(&packages[0])
            .unwrap()
            .modules
            .len(),
        1
    );
}

#[test]
fn directory_provider_reports_empty_prql_projects() {
    let directory = TempDir::new().unwrap();
    let provider = fp_prql::package::PrqlPackageProvider::discover(directory.path()).unwrap();
    let error = provider
        .load_package_source(&fp_core::ast::package::PackageId::new("empty"))
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("PRQL project contains no `.prql` files")
    );
}
