//! Helpers for serializing/deserializing AST snapshots in bootstrap mode.
//!
//! These utilities provide a thin wrapper over `serde_json` so higher-level
//! tooling can load ASTs produced by a previous build when running inside a
//! reduced environment.

use crate::ast::Node;
use crate::Result;
use serde::Deserialize;
use std::fs;
use std::io::Read;
use std::path::Path;

/// Load an AST `Node` from a JSON file on disk.
pub fn load_node_from_file(path: &Path) -> Result<Node> {
    let file = fs::File::open(path)?;
    load_node_from_reader(file)
}

/// Load an AST `Node` directly from a string slice containing JSON.
pub fn load_node_from_str(contents: &str) -> Result<Node> {
    let mut deserializer = serde_json::Deserializer::from_str(contents);
    deserializer.disable_recursion_limit();
    Ok(Node::deserialize(&mut deserializer)?)
}

/// Load an AST `Node` from any reader producing JSON.
pub fn load_node_from_reader(reader: impl Read) -> Result<Node> {
    let mut deserializer = serde_json::Deserializer::from_reader(reader);
    deserializer.disable_recursion_limit();
    Ok(Node::deserialize(&mut deserializer)?)
}

/// Persist an AST `Node` as JSON to the provided path.
pub fn write_node_to_file(path: &Path, node: &Node) -> Result<()> {
    let contents = serde_json::to_string_pretty(node)?;
    fs::write(path, contents)?;
    Ok(())
}
