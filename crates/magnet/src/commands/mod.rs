//! Command implementations for the magnet CLI

// Child modules
mod check;
mod generate;
mod init;
mod submodule;
mod tree;
mod utils;

// Re-export commands
pub use check::check;
pub use generate::generate;
pub use init::init;
pub use submodule::{deinit, init as submodule_init, list as submodule_list, switch, update};
pub use tree::tree;
#[allow(unused_imports)]
pub use utils::*;
