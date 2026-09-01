use std::fs;

use fp_core::ast::package::provider::PackageProvider;
use tempfile::TempDir;

#[test]
fn directory_provider_discovers_nested_zig_modules() {
    let directory = TempDir::new().unwrap();
    fs::create_dir_all(directory.path().join("src/math")).unwrap();
    fs::write(
        directory.path().join("src/main.zig"),
        "pub const Point = struct { x: i32 };\n",
    )
    .unwrap();
    fs::write(
        directory.path().join("src/math/add.zig"),
        "pub fn add() void {}\n",
    )
    .unwrap();

    let provider = fp_zig::package::ZigPackageProvider::discover(directory.path()).unwrap();
    let packages = provider.list_packages().unwrap();
    let package = provider.load_package_source(&packages[0]).unwrap();
    assert_eq!(package.items.len(), 2);
    assert!(
        package
            .graph
            .modules()
            .any(|module| module.module_path.to_key() == "src::main")
    );
    assert!(
        package
            .graph
            .modules()
            .any(|module| module.module_path.to_key() == "src::math::add")
    );
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
fn directory_provider_reports_empty_zig_projects() {
    let directory = TempDir::new().unwrap();
    let provider = fp_zig::package::ZigPackageProvider::discover(directory.path()).unwrap();
    let error = provider
        .load_package_source(&fp_core::ast::package::PackageId::new("empty"))
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("Zig project contains no `.zig` files")
    );
}
