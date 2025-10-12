pub mod registry;

pub use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};

pub use fp_lang::FerroFrontend;
pub use fp_wit::WitFrontend;
pub use registry::FrontendRegistry;
