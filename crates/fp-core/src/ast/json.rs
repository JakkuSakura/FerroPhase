//! Helpers for serializing/deserializing AST snapshots.
//!
//! These utilities provide a thin wrapper over `serde_json` so higher-level
//! tooling can load ASTs produced by a previous build when running inside a
//! reduced environment.

use crate::ast::File;
use crate::Result;
use serde::Deserialize;
use std::fs;
use std::io::Read;
use std::path::Path;

/// Load an AST `File` from a JSON file on disk.
pub fn load_file_from_file(path: &Path) -> Result<File> {
    let file = fs::File::open(path)?;
    load_file_from_reader(file)
}

/// Load an AST `File` directly from a string slice containing JSON.
pub fn load_file_from_str(contents: &str) -> Result<File> {
    let mut deserializer = serde_json::Deserializer::from_str(contents);
    Ok(File::deserialize(&mut deserializer)?)
}

/// Load an AST `File` from any reader producing JSON.
pub fn load_file_from_reader(reader: impl Read) -> Result<File> {
    let mut deserializer = serde_json::Deserializer::from_reader(reader);
    Ok(File::deserialize(&mut deserializer)?)
}

/// Persist an AST `File` as JSON to the provided path.
pub fn write_file_to_file(path: &Path, file: &File) -> Result<()> {
    let contents = serde_json::to_string_pretty(file)?;
    fs::write(path, contents)?;
    Ok(())
}
