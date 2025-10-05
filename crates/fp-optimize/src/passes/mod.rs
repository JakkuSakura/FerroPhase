// Passes - focused optimization passes that implement OptimizePass

pub mod closure_lowering;
pub mod materialize_intrinsics;
pub mod normalize_intrinsics;
pub mod specialize;

pub use closure_lowering::*;
pub use materialize_intrinsics::*;
pub use normalize_intrinsics::*;
pub use specialize::*;
