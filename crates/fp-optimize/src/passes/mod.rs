// Passes - focused optimization passes that implement OptimizePass

pub mod closure_lowering;
pub mod const_folding;
pub mod dead_code_elimination;
pub mod inline;
pub mod intrinsic_normalize;
pub mod materialize_intrinsics;
pub mod specialize;

pub use closure_lowering::*;
pub use inline::*;
pub use intrinsic_normalize::*;
pub use materialize_intrinsics::*;
pub use specialize::*;
// pub use const_folding::*;
// pub use dead_code_elimination::*;
