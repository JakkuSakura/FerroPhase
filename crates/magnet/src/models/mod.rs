// filepath: /home/jakku/Dev/SHLL/crates/magnet/src/models/mod.rs
//! Domain models for the Magnet system.
//!
//! These models are the parsed and validated representation of configuration data.
//! They provide a clean domain representation that other modules can use.
#[path = "crate.rs"]
mod crate_;
mod nexus;
mod package;
mod workspace;
mod dependency;

pub use crate_::*;
pub use nexus::*;
pub use package::*;
pub use workspace::*;
pub use dependency::*;

pub enum MagnetModel {
    Nexus(NexusModel),
    Workspace(WorkspaceModel),
    Package(PackageModel),
}
