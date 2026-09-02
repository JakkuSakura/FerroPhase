//! Package-scoped name-resolution implementation.
//!
//! The implementation is defined in the parent module for now so its shared
//! worklist types remain available without a second dependency layer. This
//! module is the stable package-resolver surface.

pub use super::InPackageResolver;
