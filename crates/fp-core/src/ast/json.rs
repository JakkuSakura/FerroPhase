//! Helpers for serializing/deserializing AST snapshots in bootstrap mode.
//!
//! These utilities provide a thin wrapper over `serde_json` so higher-level
//! tooling can load ASTs produced by a previous build when running inside a
//! reduced environment.

use crate::ast::Node;
use crate::Result;
use std::fs;
use std::io::Read;
use std::path::Path;

/// Load an AST `Node` from a JSON file on disk.
pub fn load_node_from_file(path: &Path) -> Result<Node> {
    let contents = fs::read_to_string(path)?;
    load_node_from_str(&contents)
}

/// Load an AST `Node` directly from a string slice containing JSON.
pub fn load_node_from_str(contents: &str) -> Result<Node> {
    Ok(serde_json::from_str(contents)?)
}

/// Load an AST `Node` from any reader producing JSON.
pub fn load_node_from_reader(mut reader: impl Read) -> Result<Node> {
    let mut buf = String::new();
    reader.read_to_string(&mut buf)?;
    load_node_from_str(&buf)
}

/// Persist an AST `Node` as JSON to the provided path.
pub fn write_node_to_file(path: &Path, node: &Node) -> Result<()> {
    let contents = serde_json::to_string_pretty(node)?;
    fs::write(path, contents)?;
    Ok(())
}
