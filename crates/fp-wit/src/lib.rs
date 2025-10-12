//! FerroPhase frontend for WebAssembly Interface Types (WIT).

pub mod error;
pub mod frontend;
pub mod model;
pub mod parser;
pub mod serializer;

pub use error::{WitError, WitResult};
pub use frontend::WitFrontend;
pub use model::{WitDocument, WitFunction, WitInterface, WitPackage, WitParameter, WitType};
pub use parser::{parse_file, parse_str};
pub use serializer::WitSerializer;
