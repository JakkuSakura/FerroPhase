//! Command implementations for the magnet CLI

// Child modules
mod bench;
mod build;
mod check;
pub mod export; // Changed from mod to pub mod to expose ExportOptions
pub mod generate; // Changed from mod to pub mod to expose GenerateOptions
mod graph;
mod init;
mod lock;
mod run;
mod submodule;
mod test;
mod tree;
mod update;
mod utils;

// Re-export commands
pub use bench::{BenchOptions, bench};
pub use build::{BuildOptions, build};
pub use check::check;
pub use export::export;
pub use generate::generate;
pub use graph::graph;
pub use init::init;
pub use lock::{LockOptions, lock};
pub use run::{RunMode, RunOptions, run};
pub use submodule::{
    deinit as submodule_deinit, init as submodule_init, list as submodule_list,
    switch as submodule_switch, update as submodule_update,
};
pub use test::{TestOptions, test};
pub use tree::tree;
pub use update::{UpdateOptions, update};
#[allow(unused_imports)]
pub use utils::*;
