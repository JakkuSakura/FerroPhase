//! Command implementations for the magnet CLI

// Child modules
mod check;
mod build;
pub mod export; // Changed from mod to pub mod to expose ExportOptions
pub mod generate; // Changed from mod to pub mod to expose GenerateOptions
mod bench;
mod graph;
mod init;
mod run;
mod submodule;
mod test;
mod tree;
mod utils;

// Re-export commands
pub use check::check;
pub use build::{build, BuildOptions};
pub use export::export;
pub use generate::generate;
pub use bench::{bench, BenchOptions};
pub use graph::graph;
pub use init::init;
pub use run::{RunMode, RunOptions, run};
pub use submodule::{
    deinit as submodule_deinit, init as submodule_init, list as submodule_list,
    switch as submodule_switch, update as submodule_update,
};
pub use test::{TestOptions, test};
pub use tree::tree;
#[allow(unused_imports)]
pub use utils::*;
