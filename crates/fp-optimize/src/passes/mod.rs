// Passes - focused optimization passes that implement OptimizePass

pub mod const_folding;
pub mod dead_code_elimination;
pub mod inline;
pub mod specialize;

pub use inline::*;
pub use specialize::*;
// pub use const_folding::*;
// pub use dead_code_elimination::*;
