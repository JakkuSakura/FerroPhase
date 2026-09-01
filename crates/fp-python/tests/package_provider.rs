use std::fs;

use fp_core::ast::package::provider::PackageProvider;
use tempfile::TempDir;

#[test]
fn directory_provider_discovers_python_sources_and_metadata() {
    let directory = TempDir::new().unwrap();
    fs::create_dir_all(directory.path().join("src/demo/nested")).unwrap();
    fs::write(
        directory.path().join("pyproject.toml"),
        "[project]\nname = \"demo\"\nversion = \"1.2.3\"\ndependencies = [\"requests>=2.0\"]\n\n[project.optional-dependencies]\ndev = [\"pytest>=7.0\"]\n",
    )
    .unwrap();
    fs::write(directory.path().join("src/demo/__init__.py"), "VALUE = 7\n").unwrap();
    fs::write(
        directory.path().join("src/demo/nested/helpers.py"),
        "def answer():\n    return 42\n",
    )
    .unwrap();

    let provider = fp_python::package::PythonPackageProvider::discover(directory.path()).unwrap();
    let packages = provider.list_packages().unwrap();
    assert_eq!(packages.len(), 1);
    assert_eq!(packages[0].as_str(), "demo");

    let package = provider.load_package_source(&packages[0]).unwrap();
    assert_eq!(package.items.len(), 2);
    assert!(
        package
            .graph
            .modules()
            .any(|module| module.module_path.to_key() == "demo")
    );
    assert!(
        package
            .graph
            .modules()
            .any(|module| module.module_path.to_key() == "demo::nested::helpers")
    );

    let metadata = provider.load_package_metadata(&packages[0]).unwrap();
    assert_eq!(metadata.name, "demo");
    assert_eq!(metadata.version.as_ref().unwrap().to_string(), "1.2.3");
    assert_eq!(metadata.modules.len(), 2);
    assert_eq!(metadata.metadata.dependencies.len(), 2);
}

#[test]
fn directory_provider_reports_empty_python_projects() {
    let directory = TempDir::new().unwrap();
    fs::write(
        directory.path().join("pyproject.toml"),
        "[project]\nname = \"empty\"\n",
    )
    .unwrap();
    let provider = fp_python::package::PythonPackageProvider::discover(directory.path()).unwrap();
    let error = provider
        .load_package_source(&fp_core::ast::package::PackageId::new("empty"))
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("Python project contains no `.py` files")
    );
}
