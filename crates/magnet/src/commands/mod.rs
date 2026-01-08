//! Command implementations for the magnet CLI

// Child modules
mod check;
pub mod export; // Changed from mod to pub mod to expose ExportOptions
pub mod generate; // Changed from mod to pub mod to expose GenerateOptions
mod init;
mod run;
mod test;
mod submodule;
mod tree;
mod utils;

// Re-export commands
pub use check::check;
pub use export::export;
pub use generate::generate;
pub use init::init;
pub use run::{run, RunMode, RunOptions};
pub use test::{test, TestOptions};
pub use submodule::{
    deinit as submodule_deinit, init as submodule_init, list as submodule_list,
    switch as submodule_switch, update as submodule_update,
};
pub use tree::tree;
#[allow(unused_imports)]
pub use utils::*;
