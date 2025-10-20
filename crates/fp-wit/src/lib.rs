//! FerroPhase frontend for WebAssembly Interface Types (WIT).
//!
//! WIT files mix *service IDL* (interfaces/worlds) and *type IDL* blocks. The
//! frontend lowers both facets into the regular AST so downstream tooling sees
//! callable APIs and portable data shapes alongside other runtime types.

pub mod error;
pub mod frontend;
pub mod model;
pub mod parser;
pub mod serializer;

pub use error::{WitError, WitResult};
pub use frontend::WitFrontend;
pub use model::{WitDocument, WitFunction, WitInterface, WitPackage, WitParameter, WitType};
pub use parser::{parse_file, parse_str};
pub use serializer::{WitOptions, WitSerializer, WorldMode};
