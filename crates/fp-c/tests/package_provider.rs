use std::fs;

use fp_core::ast::ItemKind;
use fp_core::ast::package::provider::PackageProvider;
use tempfile::TempDir;

#[test]
fn directory_provider_discovers_and_loads_c_sources() {
    let directory = TempDir::new().unwrap();
    fs::create_dir(directory.path().join("include")).unwrap();
    fs::write(
        directory.path().join("main.c"),
        "typedef int count_t;\nint add(int left, int right);\n",
    )
    .unwrap();
    fs::write(
        directory.path().join("include/api.h"),
        "int api_value(void);\n",
    )
    .unwrap();
    fs::write(directory.path().join("README.txt"), "ignored").unwrap();

    let provider = fp_c::package::CPackageProvider::discover(directory.path()).unwrap();
    let packages = provider.list_packages().unwrap();
    assert_eq!(packages.len(), 1);

    let package = provider.load_package_source(&packages[0]).unwrap();
    assert_eq!(package.graph.modules().count(), 2);
    assert_eq!(package.items.len(), 7);
    assert!(
        package
            .graph
            .modules()
            .any(|module| module.module_path.to_key() == "main")
    );
    assert!(
        package
            .graph
            .modules()
            .any(|module| module.module_path.to_key() == "include::api")
    );
    assert!(
        package
            .items
            .iter()
            .any(|item| matches!(item.item.kind(), ItemKind::DeclFunction(_)))
    );

    let metadata = provider.load_package_metadata(&packages[0]).unwrap();
    assert_eq!(metadata.modules.len(), 2);
    assert!(
        metadata
            .modules
            .iter()
            .any(|module| module.as_str() == "include::api")
    );
}

#[test]
fn directory_provider_reports_empty_c_projects() {
    let directory = TempDir::new().unwrap();
    let provider = fp_c::package::CPackageProvider::discover(directory.path()).unwrap();
    let error = provider
        .load_package_source(&fp_core::ast::package::PackageId::new("empty"))
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("C project contains no `.c` or `.h` files")
    );
}
