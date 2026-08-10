#[path = "materialize/walkers.rs"]
mod walkers;

#[path = "materialize/op_walker.rs"]
pub mod op_walker;

pub use walkers::{
    materialize_block, materialize_expr, materialize_file, materialize_item, materialize_stmt,
};
