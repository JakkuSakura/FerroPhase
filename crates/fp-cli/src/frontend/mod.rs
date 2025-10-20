pub mod registry;

pub use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};

pub use fp_flatbuffers::FlatbuffersFrontend;
pub use fp_jsonschema::JsonSchemaFrontend;
pub use fp_lang::FerroFrontend;
pub use fp_prql::PrqlFrontend;
pub use fp_sql::SqlFrontend;
pub use fp_typescript::TypeScriptFrontend;
pub use fp_wit::WitFrontend;
pub use registry::FrontendRegistry;
