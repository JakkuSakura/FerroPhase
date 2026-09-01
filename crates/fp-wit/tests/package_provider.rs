use std::fs;

use fp_core::ast::package::provider::PackageProvider;
use tempfile::TempDir;

#[test]
fn directory_provider_loads_wit_interfaces() {
    let directory = TempDir::new().unwrap();
    fs::create_dir_all(directory.path().join("api")).unwrap();
    fs::write(
        directory.path().join("api/service.wit"),
        "package demo:service;\ninterface api {\n  ping: func();\n}\n",
    )
    .unwrap();
    let provider = fp_wit::package::WitPackageProvider::discover(directory.path()).unwrap();
    let packages = provider.list_packages().unwrap();
    let package = provider.load_package_source(&packages[0]).unwrap();
    assert_eq!(package.items.len(), 1);
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
fn directory_provider_reports_empty_wit_projects() {
    let directory = TempDir::new().unwrap();
    let provider = fp_wit::package::WitPackageProvider::discover(directory.path()).unwrap();
    let error = provider
        .load_package_source(&fp_core::ast::package::PackageId::new("empty"))
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("WIT project contains no `.wit` files")
    );
}
