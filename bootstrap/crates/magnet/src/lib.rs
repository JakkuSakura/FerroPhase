//! Bootstrap Magnet - std-only workspace helper.

pub mod commands;
pub mod configs;
pub mod generator;
pub mod manager;
pub mod models;
pub mod registry;
pub mod resolver;
pub mod utils;
pub mod workspace_graph;

pub const VERSION: &str = "0.0.0";

pub type Result<T> = fp_core::Result<T>;
