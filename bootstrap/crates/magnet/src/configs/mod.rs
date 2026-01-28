mod cargo;
mod dependency;
mod nexus;
mod package;
mod workspace;

pub use cargo::*;
pub use dependency::*;
pub use nexus::*;
pub use package::*;
pub use workspace::*;

use fp_core::ast::{Value, ValueMap};

use crate::utils::{get_table, map_get, read_toml_file};
use std::path::Path;

#[derive(Debug, Clone, Default)]
pub struct BuildConfig {
    pub options: Vec<String>,
    pub features: Vec<String>,
}

#[derive(Debug, Clone, Default)]
pub struct ManifestConfig {
    pub build: BuildConfig,
    pub workspace: Option<WorkspaceConfig>,
    pub package: Option<PackageConfig>,
    pub dependencies: DependencyConfigMap,
    pub dev_dependencies: DependencyConfigMap,
    pub build_dependencies: DependencyConfigMap,
    pub source_path: Option<std::path::PathBuf>,
}

impl ManifestConfig {
    pub fn from_file(path: &Path) -> crate::Result<Self> {
        let value = read_toml_file(path)?;
        let mut config = ManifestConfig::from_value(&value);
        config.source_path = Some(path.to_path_buf());
        Ok(config)
    }

    pub fn from_value(value: &Value) -> Self {
        let workspace = get_table(value, "workspace").map(parse_workspace_config);
        let package = get_table(value, "package").map(parse_package_config);
        let dependencies = get_table(value, "dependencies")
            .map(parse_dependency_map)
            .unwrap_or_default();
        let dev_dependencies = get_table(value, "dev-dependencies")
            .map(parse_dependency_map)
            .unwrap_or_default();
        let build_dependencies = get_table(value, "build-dependencies")
            .map(parse_dependency_map)
            .unwrap_or_default();
        let build = parse_build_config(value);
        ManifestConfig {
            build,
            workspace,
            package,
            dependencies,
            dev_dependencies,
            build_dependencies,
            source_path: None,
        }
    }
}

fn parse_build_config(value: &Value) -> BuildConfig {
    let mut config = BuildConfig::default();
    let Some(build) = get_table(value, "build") else {
        return config;
    };
    if let Some(options) = map_get(build, "options") {
        if let Value::Map(map) = options {
            config.options = crate::utils::build_kv_list(map);
        }
    }
    if let Some(features) = map_get(build, "features") {
        config.features = crate::utils::get_string_list(features);
    }
    config
}

pub fn value_from_dependencies(map: &DependencyConfigMap) -> Value {
    let mut entries = Vec::new();
    for (name, dep) in map {
        let value = match dep {
            DependencyConfig {
                version: Some(version),
                path: None,
                optional: false,
                features,
            } if features.is_empty() => Value::string(version.clone()),
            _ => {
                let mut map_entries = Vec::new();
                if let Some(version) = &dep.version {
                    map_entries.push((Value::string("version".to_string()), Value::string(version.clone())));
                }
                if let Some(path) = &dep.path {
                    map_entries.push((Value::string("path".to_string()), Value::string(path.clone())));
                }
                if dep.optional {
                    map_entries.push((Value::string("optional".to_string()), Value::bool(true)));
                }
                if !dep.features.is_empty() {
                    map_entries.push((
                        Value::string("features".to_string()),
                        crate::utils::to_string_list(&dep.features),
                    ));
                }
                Value::Map(ValueMap::from_pairs(map_entries))
            }
        };
        entries.push((Value::string(name.clone()), value));
    }
    Value::Map(ValueMap::from_pairs(entries))
}
