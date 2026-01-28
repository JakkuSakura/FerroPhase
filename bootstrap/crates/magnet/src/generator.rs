use fp_core::ast::{Value, ValueMap};

use crate::configs::{ManifestConfig, value_from_dependencies};
use crate::utils::{to_string_list, write_toml_file};
use std::path::{Path, PathBuf};

pub fn generate_workspace_cargo_toml(config: &ManifestConfig, output_path: &Path) -> crate::Result<()> {
    let mut entries = Vec::new();

    if let Some(workspace) = &config.workspace {
        let workspace_map = ValueMap::from_pairs([
            (
                Value::string("members".to_string()),
                to_string_list(&workspace.members),
            ),
            (
                Value::string("exclude".to_string()),
                to_string_list(&workspace.exclude),
            ),
        ]);
        entries.push((Value::string("workspace".to_string()), Value::Map(workspace_map)));
    }

    if let Some(package) = &config.package {
        let mut package_entries = Vec::new();
        if let Some(name) = &package.name {
            package_entries.push((Value::string("name".to_string()), Value::string(name.clone())));
        }
        if let Some(version) = &package.version {
            package_entries.push((
                Value::string("version".to_string()),
                Value::string(version.clone()),
            ));
        }
        if let Some(edition) = &package.edition {
            package_entries.push((
                Value::string("edition".to_string()),
                Value::string(edition.clone()),
            ));
        }
        if !package.authors.is_empty() {
            package_entries.push((
                Value::string("authors".to_string()),
                to_string_list(&package.authors),
            ));
        }
        if let Some(description) = &package.description {
            package_entries.push((
                Value::string("description".to_string()),
                Value::string(description.clone()),
            ));
        }
        if let Some(license) = &package.license {
            package_entries.push((
                Value::string("license".to_string()),
                Value::string(license.clone()),
            ));
        }
        if !package.keywords.is_empty() {
            package_entries.push((
                Value::string("keywords".to_string()),
                to_string_list(&package.keywords),
            ));
        }
        entries.push((Value::string("package".to_string()), Value::Map(ValueMap::from_pairs(package_entries))));
    }

    if !config.dependencies.is_empty() {
        entries.push((
            Value::string("dependencies".to_string()),
            value_from_dependencies(&config.dependencies),
        ));
    }
    if !config.dev_dependencies.is_empty() {
        entries.push((
            Value::string("dev-dependencies".to_string()),
            value_from_dependencies(&config.dev_dependencies),
        ));
    }
    if !config.build_dependencies.is_empty() {
        entries.push((
            Value::string("build-dependencies".to_string()),
            value_from_dependencies(&config.build_dependencies),
        ));
    }

    let cargo = Value::Map(ValueMap::from_pairs(entries));
    write_toml_file(output_path, &cargo)
}

pub fn workspace_output_path(root: &Path) -> PathBuf {
    root.join("Cargo.toml")
}
