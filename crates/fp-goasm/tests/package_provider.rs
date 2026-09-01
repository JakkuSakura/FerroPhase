use std::fs;

use fp_core::ast::package::provider::PackageProvider;
use tempfile::TempDir;

fn source(function: &str) -> String {
    format!(
        "#include \"textflag.h\"\n// fp-goasm (amd64)\n\nTEXT ·{function}(SB), NOSPLIT, $0-0\n{function}_bb0:\n    MOVQ $7, R10\n    MOVQ R10, AX\n    RET\n"
    )
}

#[test]
fn directory_provider_discovers_nested_goasm_modules() {
    let directory = TempDir::new().unwrap();
    fs::create_dir(directory.path().join("nested")).unwrap();
    fs::write(directory.path().join("main.goasm"), source("main")).unwrap();
    fs::write(
        directory.path().join("nested/helper.goasm"),
        source("helper"),
    )
    .unwrap();
    fs::write(directory.path().join("README.txt"), "ignored").unwrap();

    let provider = fp_goasm::package::GoPackageProvider::discover(directory.path()).unwrap();
    let packages = provider.list_packages().unwrap();
    assert_eq!(packages.len(), 1);

    let package = provider.load_package_source(&packages[0]).unwrap();
    assert_eq!(package.items.len(), 2);
    assert_eq!(package.graph.modules().count(), 2);
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
            .any(|module| module.module_path.to_key() == "nested::helper")
    );

    let metadata = provider.load_package_metadata(&packages[0]).unwrap();
    assert_eq!(metadata.modules.len(), 2);
    assert!(
        metadata
            .modules
            .iter()
            .any(|module| module.as_str() == "nested::helper")
    );
}

#[test]
fn directory_provider_rejects_empty_projects() {
    let directory = TempDir::new().unwrap();
    let provider = fp_goasm::package::GoPackageProvider::discover(directory.path()).unwrap();
    let error = provider
        .load_package_source(&fp_core::ast::package::PackageId::new("unused"))
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("GoASM project contains no `.goasm` files")
    );
}
