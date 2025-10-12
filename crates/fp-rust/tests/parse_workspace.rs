use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fp_core::package::provider::{ModuleProvider, PackageProvider};
use fp_rust::package::{CargoPackageProvider, RustModuleProvider};
use fp_rust::parser::RustParser;

fn workspace_root() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf()
}

#[test]
fn parses_ferrophase_workspace_sources() {
    let root = workspace_root();
    let provider = Arc::new(CargoPackageProvider::new(root.clone()));

    if let Err(err) = provider.refresh() {
        eprintln!("skipping workspace parse test: {err}");
        return;
    }

    let mut failures = Vec::new();
    let module_provider = RustModuleProvider::new(provider.clone());
    let parser = RustParser::new();

    let packages = match provider.list_packages() {
        Ok(packages) => packages,
        Err(err) => {
            panic!("failed to list packages: {err}");
        }
    };

    for package_id in packages {
        let modules = match module_provider.modules_for_package(&package_id) {
            Ok(modules) => modules,
            Err(err) => {
                failures.push(format!("{package_id}: failed to enumerate modules: {err}"));
                continue;
            }
        };

        for module_id in modules {
            match module_provider.load_module(&module_id) {
                Ok(descriptor) => {
                    let path = descriptor.source.to_path_buf();
                    if !path.exists() {
                        failures.push(format!(
                            "{module_id}: source path does not exist: {}",
                            path.display()
                        ));
                        continue;
                    }

                    let content = match fs::read_to_string(&path) {
                        Ok(src) => src,
                        Err(err) => {
                            failures.push(format!(
                                "{module_id}: failed to read {}: {err}",
                                path.display()
                            ));
                            continue;
                        }
                    };

                    let syn_file = match syn::parse_file(&content) {
                        Ok(file) => file,
                        Err(err) => {
                            failures.push(format!(
                                "{module_id}: syn parse error in {}: {err}",
                                path.display()
                            ));
                            continue;
                        }
                    };

                    if let Err(err) = parser.parse_file_content(path.clone(), syn_file) {
                        failures.push(format!(
                            "{module_id}: lowering error in {}: {err}",
                            path.display()
                        ));
                    }
                }
                Err(err) => {
                    failures.push(format!(
                        "{package_id}: failed to load module {module_id}: {err}"
                    ));
                }
            }
        }
    }

    if !failures.is_empty() {
        panic!(
            "workspace parse reported {} failures:\n{}",
            failures.len(),
            failures.join("\n")
        );
    }
}
