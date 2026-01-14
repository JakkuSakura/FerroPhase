use crate::models::{DependencyModel, DependencyModelMap, PatchMap};
use eyre::Result;
use std::collections::HashMap;
use std::path::Path;

pub struct CargoDependencies {
    pub dependencies: DependencyModelMap,
    pub dev_dependencies: DependencyModelMap,
    pub build_dependencies: DependencyModelMap,
}

pub fn parse_cargo_dependencies(manifest_path: &Path) -> Result<CargoDependencies> {
    let content = std::fs::read_to_string(manifest_path)?;
    let value: toml::Value = toml::from_str(&content)?;
    Ok(CargoDependencies {
        dependencies: parse_cargo_deps(value.get("dependencies")),
        dev_dependencies: parse_cargo_deps(value.get("dev-dependencies")),
        build_dependencies: parse_cargo_deps(value.get("build-dependencies")),
    })
}

pub fn parse_cargo_deps(section: Option<&toml::Value>) -> DependencyModelMap {
    let mut out = HashMap::new();
    let Some(table) = section.and_then(|v| v.as_table()) else {
        return out;
    };
    parse_cargo_deps_from_table(table, &mut out);
    out
}

pub fn parse_cargo_deps_from_table(
    table: &toml::value::Table,
    out: &mut DependencyModelMap,
) {
    for (name, value) in table {
        let mut model = DependencyModel::default();
        match value {
            toml::Value::String(version) => {
                model.version = Some(version.to_string());
            }
            toml::Value::Table(table) => {
                if let Some(version) = table.get("version").and_then(|v| v.as_str()) {
                    model.version = Some(version.to_string());
                }
                if let Some(path) = table.get("path").and_then(|v| v.as_str()) {
                    model.path = Some(Path::new(path).to_path_buf());
                }
                if let Some(git) = table.get("git").and_then(|v| v.as_str()) {
                    model.git = Some(git.to_string());
                }
                if let Some(branch) = table.get("branch").and_then(|v| v.as_str()) {
                    model.branch = Some(branch.to_string());
                }
                if let Some(tag) = table.get("tag").and_then(|v| v.as_str()) {
                    model.tag = Some(tag.to_string());
                }
                if let Some(rev) = table.get("rev").and_then(|v| v.as_str()) {
                    model.rev = Some(rev.to_string());
                }
                if let Some(features) = table.get("features").and_then(|v| v.as_array()) {
                    let list = features
                        .iter()
                        .filter_map(|v| v.as_str())
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>();
                    if !list.is_empty() {
                        model.features = Some(list);
                    }
                }
                if let Some(default_features) = table
                    .get("default-features")
                    .and_then(|v| v.as_bool())
                {
                    model.default_features = Some(default_features);
                }
                if let Some(optional) = table.get("optional").and_then(|v| v.as_bool()) {
                    model.optional = Some(optional);
                }
                if let Some(package) = table.get("package").and_then(|v| v.as_str()) {
                    model.package = Some(package.to_string());
                }
                if let Some(registry) = table.get("registry").and_then(|v| v.as_str()) {
                    model.registry = Some(registry.to_string());
                }
                if let Some(workspace) = table.get("workspace").and_then(|v| v.as_bool()) {
                    model.workspace = Some(workspace);
                }
            }
            _ => {}
        }
        out.insert(name.to_string(), model);
    }
}

pub fn parse_patch_table(patch: Option<toml::value::Table>) -> PatchMap {
    let mut out = PatchMap::new();
    let Some(table) = patch else {
        return out;
    };
    for (name, value) in table {
        let mut deps = DependencyModelMap::new();
        if let Some(dep_table) = value.as_table() {
            parse_cargo_deps_from_table(dep_table, &mut deps);
        }
        out.insert(name, deps);
    }
    out
}
