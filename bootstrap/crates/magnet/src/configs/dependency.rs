use std::collections::BTreeMap;

use fp_core::ast::{Value, ValueMap};

use crate::utils::{get_string_list, map_get, map_get_bool, map_get_string};

#[derive(Debug, Clone, Default)]
pub struct DependencyConfig {
    pub version: Option<String>,
    pub path: Option<String>,
    pub optional: bool,
    pub features: Vec<String>,
}

pub type DependencyConfigMap = BTreeMap<String, DependencyConfig>;

pub fn parse_dependency_map(table: &ValueMap) -> DependencyConfigMap {
    let mut out = DependencyConfigMap::new();
    for entry in &table.entries {
        let key = match &entry.key {
            Value::String(s) => s.value.clone(),
            _ => continue,
        };
        let value = parse_dependency_value(&entry.value);
        out.insert(key, value);
    }
    out
}

fn parse_dependency_value(value: &Value) -> DependencyConfig {
    match value {
        Value::String(s) => DependencyConfig {
            version: Some(s.value.clone()),
            ..DependencyConfig::default()
        },
        Value::Map(map) => {
            let version = map_get_string(map, "version");
            let path = map_get_string(map, "path");
            let optional = map_get_bool(map, "optional").unwrap_or(false);
            let features = map_get(map, "features")
                .map(get_string_list)
                .unwrap_or_default();
            DependencyConfig {
                version,
                path,
                optional,
                features,
            }
        }
        _ => DependencyConfig::default(),
    }
}
