use fp_core::ast::ValueMap;

use crate::utils::map_get_string_list;

#[derive(Debug, Clone, Default)]
pub struct WorkspaceConfig {
    pub members: Vec<String>,
    pub exclude: Vec<String>,
}

pub fn parse_workspace_config(table: &ValueMap) -> WorkspaceConfig {
    WorkspaceConfig {
        members: map_get_string_list(table, "members"),
        exclude: map_get_string_list(table, "exclude"),
    }
}
