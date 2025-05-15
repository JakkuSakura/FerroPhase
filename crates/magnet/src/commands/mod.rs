//! Command implementations for the magnet CLI

// Child modules
mod check;
mod generate;
mod init;
mod tree;
mod utils;

// Re-export commands
pub use check::check;
pub use generate::generate;
pub use init::init;
pub use tree::tree;
#[allow(unused_imports)]
pub use utils::*;
