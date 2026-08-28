use std::fs;

use fp_core::ast::package::provider::PackageProvider;
use tempfile::TempDir;

#[test]
fn directory_provider_loads_toml_documents() {
    let directory = TempDir::new().unwrap();
    fs::create_dir_all(directory.path().join("nested")).unwrap();
    fs::write(directory.path().join("config.toml"), "answer = 42\n").unwrap();
    fs::write(
        directory.path().join("nested/other.toml"),
        "enabled = true\n",
    )
    .unwrap();
    let provider = fp_toml::package::TomlPackageProvider::discover(directory.path()).unwrap();
    let packages = provider.list_packages().unwrap();
    let package = provider.load_package_source(&packages[0]).unwrap();
    assert_eq!(package.items.len(), 2);
    assert_eq!(
        provider
            .load_package_metadata(&packages[0])
            .unwrap()
            .modules
            .len(),
        2
    );
}

#[test]
fn directory_provider_reports_empty_toml_projects() {
    let directory = TempDir::new().unwrap();
    let provider = fp_toml::package::TomlPackageProvider::discover(directory.path()).unwrap();
    let error = provider
        .load_package_source(&fp_core::ast::package::PackageId::new("empty"))
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("TOML project contains no `.toml` files")
    );
}
