use std::fs;

use fp_core::ast::package::provider::PackageProvider;
use tempfile::TempDir;

#[test]
fn directory_provider_loads_sql_documents() {
    let directory = TempDir::new().unwrap();
    fs::write(directory.path().join("query.sql"), "SELECT 42;").unwrap();
    let provider = fp_sql::package::SqlPackageProvider::discover(directory.path()).unwrap();
    let packages = provider.list_packages().unwrap();
    let package = provider.load_package_source(&packages[0]).unwrap();
    assert_eq!(package.items.len(), 0);
    assert_eq!(package.module_paths.len(), 1);
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
fn directory_provider_reports_empty_sql_projects() {
    let directory = TempDir::new().unwrap();
    let provider = fp_sql::package::SqlPackageProvider::discover(directory.path()).unwrap();
    let error = provider
        .load_package_source(&fp_core::ast::package::PackageId::new("empty"))
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("SQL project contains no `.sql` files")
    );
}
