//! Common reusable helpers for CLI commands to avoid duplication.

use indicatif::{ProgressBar, ProgressStyle};
use std::path::PathBuf;
use crate::{CliError, Result};
use fp_core::ast::{RuntimeValue, Value};

/// Create a consistently styled progress bar for command loops.
pub fn setup_progress_bar(total: usize) -> ProgressBar {
    let pb = ProgressBar::new(total as u64);
    pb.set_style(
        ProgressStyle::with_template(
            "  {spinner:.cyan} {wide_msg}  {bar:40.cyan/blue}  {pos}/{len}",
        )
        .unwrap()
        .progress_chars("#>-")
        .tick_strings(&["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]),
    );
    pb
}

/// Compact renderer for simple values shown in CLI output.
pub fn format_value_brief(result: &Value) -> String {
    match result {
        Value::Unit(_) => "()".to_string(),
        Value::Bool(b) => if b.value { "true" } else { "false" }.to_string(),
        Value::Int(i) => i.value.to_string(),
        Value::Decimal(f) => f.value.to_string(),
        Value::String(s) => format!("\"{}\"", s.value),
        Value::List(list) => format!("[list with {} elements]", list.values.len()),
        Value::Map(map) => format!("{map with {} entries}", map.len()),
        Value::Struct(s) => format!("struct {}", s.ty.name),
        other => format!("{:?}", other),
    }
}

/// Human-friendly ownership label for runtime values.
pub fn ownership_label(rv: &RuntimeValue) -> &'static str {
    if rv.is_literal() {
        "literal"
    } else if rv.is_owned() {
        "owned"
    } else if rv.is_borrowed() {
        "borrowed"
    } else if rv.is_shared() {
        "shared"
    } else {
        "extension"
    }
}

/// Validate that all provided paths exist (and optionally are files).
pub fn validate_paths_exist(inputs: &[PathBuf], must_be_files: bool) -> Result<()> {
    for input in inputs {
        if !input.exists() {
            return Err(CliError::InvalidInput(format!(
                "Input path does not exist: {}",
                input.display()
            )));
        }
        if must_be_files && !input.is_file() {
            return Err(CliError::InvalidInput(format!(
                "Input path is not a file: {}",
                input.display()
            )));
        }
    }
    Ok(())
}
