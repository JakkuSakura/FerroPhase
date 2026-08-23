pub mod embedded_std;
pub mod frontend;
pub mod normalizer;
pub mod provider;

pub use frontend::RustFrontend;
pub use normalizer::RustIntrinsicNormalizer;
pub use provider::{RustPackageProvider, RustStdProvider};
