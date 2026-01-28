use fp_core::ast::Value;

use crate::utils::{get_string_list, get_table, map_get};

pub fn read_workspace_members(cargo: &Value) -> Vec<String> {
    let Some(workspace) = get_table(cargo, "workspace") else {
        return Vec::new();
    };
    map_get(workspace, "members")
        .map(get_string_list)
        .unwrap_or_default()
}
