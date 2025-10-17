use crate::ast::Node;
use crate::Result;
use serde::{Deserialize, Serialize};
use std::fs;
use std::io::Read;
use std::path::Path;

/// Frontend snapshot capturing the canonical AST and minimal metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AstSnapshot {
    pub schema_version: u32,
    pub tool_version: String,
    /// Milliseconds since UNIX epoch when the snapshot was produced.
    pub created_ms: u64,
    pub ast: Node,
}

pub fn load_snapshot_from_file(path: &Path) -> Result<AstSnapshot> {
    let contents = fs::read_to_string(path)?;
    load_snapshot_from_str(&contents)
}

pub fn load_snapshot_from_reader(mut reader: impl Read) -> Result<AstSnapshot> {
    let mut buf = String::new();
    reader.read_to_string(&mut buf)?;
    load_snapshot_from_str(&buf)
}

pub fn load_snapshot_from_str(contents: &str) -> Result<AstSnapshot> {
    Ok(serde_json::from_str(contents)?)
}

pub fn write_snapshot_to_file(path: &Path, snapshot: &AstSnapshot) -> Result<()> {
    let contents = serde_json::to_string_pretty(snapshot)?;
    fs::write(path, contents)?;
    Ok(())
}
