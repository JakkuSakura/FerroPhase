/// Backend-specific THIR materialization logic
///
/// Each backend can provide custom logic for transforming intrinsic calls
/// into backend-specific THIR expressions.

pub mod llvm;