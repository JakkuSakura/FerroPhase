use std::sync::Arc;

use fp_core::module::ModuleLanguage;
use fp_core::package::provider::{
    CargoPackageProvider, ModuleProvider, PackageProvider, RustModuleProvider,
};
use fp_core::package::PackageId;
use fp_core::vfs::{InMemoryFileSystem, VirtualFileSystem, VirtualPath};

fn setup_workspace(fs: &InMemoryFileSystem, workspace: &VirtualPath) {
    fs.mkdirp(workspace).expect("workspace dir");

    let cargo_toml = "\n[workspace]\nmembers = [\"crates/*\"]\n";
    fs.write(&workspace.join("Cargo.toml"), cargo_toml.as_bytes())
        .expect("workspace manifest");

    let package_root = workspace.join("crates").join("example");
    fs.mkdirp(&package_root.join("src")).expect("src dir");

    let package_manifest = "\n[package]\nname = \"example\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[dependencies]\nserde = \"1\"\n";
    fs.write(
        &package_root.join("Cargo.toml"),
        package_manifest.as_bytes(),
    )
    .expect("package manifest");

    let lib_rs = "pub fn meaning() -> i32 { 42 }\n";
    fs.write(&package_root.join("src").join("lib.rs"), lib_rs.as_bytes())
        .expect("lib.rs");
}

#[test]
fn cargo_package_provider_discovers_workspace_members() {
    let fs = Arc::new(InMemoryFileSystem::new());
    let workspace_root = VirtualPath::new_absolute(["workspace"]);
    setup_workspace(&fs, &workspace_root);

    let provider = CargoPackageProvider::new(fs.clone(), workspace_root.clone());
    provider.refresh().expect("load workspace");

    let packages = provider.list_packages().expect("list packages");
    assert_eq!(packages, vec![PackageId::new("example")]);

    let package = provider
        .load_package(&PackageId::new("example"))
        .expect("load package");
    assert_eq!(package.name, "example");
    assert_eq!(package.metadata.dependencies.len(), 1);
    assert_eq!(package.metadata.dependencies[0].package, "serde");
    assert_eq!(
        package.manifest_path,
        workspace_root
            .join("crates")
            .join("example")
            .join("Cargo.toml"),
    );
}

#[test]
fn rust_module_provider_scans_rust_sources() {
    let fs = Arc::new(InMemoryFileSystem::new());
    let workspace_root = VirtualPath::new_absolute(["workspace"]);
    setup_workspace(&fs, &workspace_root);

    let package_provider = Arc::new(CargoPackageProvider::new(
        fs.clone(),
        workspace_root.clone(),
    ));
    package_provider.refresh().expect("load workspace");

    let module_provider = RustModuleProvider::new(fs.clone(), package_provider.clone());
    let package_id = PackageId::new("example");
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
}
