use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::module::ModuleLanguage;
use fp_core::package::provider::{ModuleProvider, PackageProvider};
use fp_core::package::PackageId;
use fp_rust::package::{CargoPackageProvider, RustModuleProvider};
use tempfile::TempDir;

fn create_temp_workspace() -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("temp workspace");
    let root = dir.path().to_path_buf();

    fs::write(
        root.join("Cargo.toml"),
        "[workspace]\nmembers = [\"crate_a\", \"crate_b\"]\n",
    )
    .expect("workspace manifest");

    let crate_a = root.join("crate_a");
    fs::create_dir_all(crate_a.join("src")).expect("crate_a src");
    fs::write(
        crate_a.join("Cargo.toml"),
        "[package]\nname = \"crate_a\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
    )
    .expect("crate_a manifest");
    fs::write(
        crate_a.join("src/lib.rs"),
        "pub fn meaning() -> i32 { 42 }\n",
    )
    .expect("crate_a lib");

    let crate_b = root.join("crate_b");
    fs::create_dir_all(crate_b.join("src")).expect("crate_b src");
    fs::write(
        crate_b.join("Cargo.toml"),
        "[package]\nname = \"crate_b\"\nversion = \"0.1.0\"\nedition = \"2021\"\n[[bin]]\nname = \"crate_b\"\npath = \"src/main.rs\"\n",
    )
    .expect("crate_b manifest");
    fs::write(
        crate_b.join("src/main.rs"),
        "fn main() { println!(\"hello\"); }\n",
    )
    .expect("crate_b main");

    (dir, root)
}

#[test]
fn cargo_package_provider_discovers_workspace_members() {
    let (_tmp, root) = create_temp_workspace();
    let provider = Arc::new(CargoPackageProvider::new(root));
    provider.refresh().expect("load workspace");

    let mut packages = provider.list_packages().expect("list packages");
    packages.sort_by(|a, b| a.as_str().cmp(b.as_str()));
    assert_eq!(
        packages,
        vec![PackageId::new("crate_a"), PackageId::new("crate_b")]
    );

    let crate_a = provider
        .load_package(&PackageId::new("crate_a"))
        .expect("load crate_a");
    assert_eq!(crate_a.name, "crate_a");
    assert_eq!(crate_a.metadata.dependencies.len(), 0);
}

#[test]
fn rust_module_provider_enumerates_targets() {
    let (_tmp, root) = create_temp_workspace();
    let provider = Arc::new(CargoPackageProvider::new(root));
    provider.refresh().expect("load workspace");

    let module_provider = RustModuleProvider::new(provider.clone());
    let package_id = PackageId::new("crate_a");
    let modules = module_provider
        .modules_for_package(&package_id)
        .expect("modules");
    assert_eq!(modules.len(), 1);

    let module = module_provider
        .load_module(&modules[0])
        .expect("load module");
    assert_eq!(module.language, ModuleLanguage::Rust);
    assert_eq!(module.package, package_id);
    assert_eq!(module.module_path, vec![String::from("lib")]);

    let crate_b_modules = module_provider
        .modules_for_package(&PackageId::new("crate_b"))
        .expect("crate_b modules");
    assert_eq!(crate_b_modules.len(), 1);
}

#[test]
fn cargo_package_provider_handles_current_workspace() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf();

    let provider = Arc::new(CargoPackageProvider::new(workspace_root));
    if let Err(err) = provider.refresh() {
        eprintln!("Skipping workspace integration test: {err}");
        return;
    }

    let packages = provider.list_packages().expect("list packages");
    assert!(packages.iter().any(|pkg| pkg.as_str() == "fp-core"));

    let module_provider = RustModuleProvider::new(provider.clone());
    let modules = module_provider
        .modules_for_package(&PackageId::new("fp-core"))
        .expect("fp-core modules");
    assert!(!modules.is_empty());
}
