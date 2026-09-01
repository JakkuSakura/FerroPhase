use std::fs;

use fp_core::ast::package::provider::PackageProvider;
use tempfile::TempDir;

#[test]
fn directory_provider_discovers_go_modules_and_manifest_metadata() {
    let directory = TempDir::new().unwrap();
    fs::create_dir_all(directory.path().join("internal/service")).unwrap();
    fs::write(
        directory.path().join("go.mod"),
        "module example.com/demo\n\ngo 1.22\n\nrequire github.com/foo/bar v1.2.3\n",
    )
    .unwrap();
    fs::write(
        directory.path().join("main.go"),
        "package main\n\nconst Answer = 42\nfunc Main() {}\n",
    )
    .unwrap();
    fs::write(
        directory.path().join("internal/service/service.go"),
        "package service\n\nfunc Run() {}\n",
    )
    .unwrap();

    let provider = fp_golang::package::GoLangPackageProvider::discover(directory.path()).unwrap();
    let packages = provider.list_packages().unwrap();
    assert_eq!(packages.len(), 1);
    assert_eq!(packages[0].as_str(), "example.com/demo");

    let package = provider.load_package_source(&packages[0]).unwrap();
    assert_eq!(package.items.len(), 3);
    assert!(
        package
            .items.iter().any(|item| item.module_path.to_key() == "root")
    );
    assert!(
        package
            .items.iter().any(|item| item.module_path.to_key() == "internal::service")
    );
    assert!(
        package
            .items
            .iter()
            .any(|item| { matches!(item.item.kind(), fp_core::ast::ItemKind::DefFunction(_)) })
    );

    let metadata = provider.load_package_metadata(&packages[0]).unwrap();
    assert_eq!(metadata.name, "example.com/demo");
    assert_eq!(metadata.metadata.edition.as_deref(), Some("1.22"));
    assert_eq!(metadata.metadata.dependencies.len(), 1);
    assert_eq!(metadata.modules.len(), 2);
}

#[test]
fn directory_provider_reports_empty_go_projects() {
    let directory = TempDir::new().unwrap();
    fs::write(
        directory.path().join("go.mod"),
        "module example.com/empty\n",
    )
    .unwrap();
    let provider = fp_golang::package::GoLangPackageProvider::discover(directory.path()).unwrap();
    let error = provider
        .load_package_source(&fp_core::ast::package::PackageId::new("example.com/empty"))
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("Go project contains no `.go` files")
    );
}
