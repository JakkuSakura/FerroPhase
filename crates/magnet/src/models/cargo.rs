use crate::models::{DependencyModel, DependencyModelMap, PatchMap};
use eyre::Result;
use std::collections::HashMap;
use std::path::Path;

pub struct CargoDependencies {
    pub dependencies: DependencyModelMap,
    pub dev_dependencies: DependencyModelMap,
    pub build_dependencies: DependencyModelMap,
    pub target_dependencies: Vec<TargetedDependencies>,
}

#[derive(Debug, Clone)]
pub struct TargetedDependencies {
    pub target: String,
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
        target_dependencies: parse_target_dependencies(value.get("target")),
    })
}

pub fn parse_cargo_features(manifest_path: &Path) -> Result<HashMap<String, Vec<String>>> {
    let content = std::fs::read_to_string(manifest_path)?;
    let value: toml::Value = toml::from_str(&content)?;
    let mut out = HashMap::new();
    let Some(table) = value.get("features").and_then(|v| v.as_table()) else {
        return Ok(out);
    };
    for (name, value) in table {
        let Some(items) = value.as_array() else {
            continue;
        };
        let list = items
            .iter()
            .filter_map(|v| v.as_str())
            .map(|s| s.to_string())
            .collect::<Vec<_>>();
        out.insert(name.to_string(), list);
    }
    Ok(out)
}

pub fn parse_cargo_deps(section: Option<&toml::Value>) -> DependencyModelMap {
    let mut out = HashMap::new();
    let Some(table) = section.and_then(|v| v.as_table()) else {
        return out;
    };
    parse_cargo_deps_from_table(table, &mut out);
    out
}

pub fn parse_cargo_deps_from_table(table: &toml::value::Table, out: &mut DependencyModelMap) {
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
                if let Some(default_features) =
                    table.get("default-features").and_then(|v| v.as_bool())
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

fn parse_target_dependencies(section: Option<&toml::Value>) -> Vec<TargetedDependencies> {
    let mut out = Vec::new();
    let Some(targets) = section.and_then(|v| v.as_table()) else {
        return out;
    };
    for (target, value) in targets {
        let Some(target_table) = value.as_table() else {
            continue;
        };
        let mut dependencies = parse_cargo_deps(target_table.get("dependencies"));
        let mut dev_dependencies = parse_cargo_deps(target_table.get("dev-dependencies"));
        let mut build_dependencies = parse_cargo_deps(target_table.get("build-dependencies"));
        if dependencies.is_empty() && dev_dependencies.is_empty() && build_dependencies.is_empty() {
            continue;
        }
        apply_target_to_deps(target, &mut dependencies);
        apply_target_to_deps(target, &mut dev_dependencies);
        apply_target_to_deps(target, &mut build_dependencies);
        out.push(TargetedDependencies {
            target: target.to_string(),
            dependencies,
            dev_dependencies,
            build_dependencies,
        });
    }
    out
}

fn apply_target_to_deps(target: &str, deps: &mut DependencyModelMap) {
    for model in deps.values_mut() {
        model.target = Some(target.to_string());
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn parse_target_dependencies() -> Result<()> {
        let temp = tempdir()?;
        let manifest = temp.path().join("Cargo.toml");
        fs::write(
            &manifest,
            r#"[package]
name = "demo"
version = "0.1.0"

[dependencies]
serde = "1"

[target.'cfg(windows)'.dependencies]
winapi = "0.3"

[target.'x86_64-unknown-linux-gnu'.build-dependencies]
cc = "1"
"#,
        )?;

        let deps = parse_cargo_dependencies(&manifest)?;
        assert!(deps.dependencies.contains_key("serde"));
        assert_eq!(deps.target_dependencies.len(), 2);
        let windows = deps
            .target_dependencies
            .iter()
            .find(|entry| entry.target == "cfg(windows)")
            .expect("windows target deps missing");
        assert!(windows.dependencies.contains_key("winapi"));
        assert_eq!(
            windows
                .dependencies
                .get("winapi")
                .and_then(|dep| dep.target.clone())
                .as_deref(),
            Some("cfg(windows)")
        );
        Ok(())
    }
}
