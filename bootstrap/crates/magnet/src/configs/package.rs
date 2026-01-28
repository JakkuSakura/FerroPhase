use fp_core::ast::ValueMap;

use crate::utils::{map_get_string, map_get_string_list};

#[derive(Debug, Clone, Default)]
pub struct PackageConfig {
    pub name: Option<String>,
    pub version: Option<String>,
    pub edition: Option<String>,
    pub authors: Vec<String>,
    pub description: Option<String>,
    pub license: Option<String>,
    pub keywords: Vec<String>,
}

pub fn parse_package_config(table: &ValueMap) -> PackageConfig {
    PackageConfig {
        name: map_get_string(table, "name"),
        version: map_get_string(table, "version"),
        edition: map_get_string(table, "edition"),
        authors: map_get_string_list(table, "authors"),
        description: map_get_string(table, "description"),
        license: map_get_string(table, "license"),
        keywords: map_get_string_list(table, "keywords"),
    }
}
