// Passes - focused optimization passes that implement OptimizePass

pub mod closure_lowering;
pub mod const_access;
pub mod materialize_intrinsics;
pub mod materialize_types;
pub mod normalize_intrinsics;
pub mod remove_generics;

pub use closure_lowering::*;
pub use const_access::*;
pub use materialize_intrinsics::*;
pub use materialize_types::*;
pub use normalize_intrinsics::*;
pub use remove_generics::*;
