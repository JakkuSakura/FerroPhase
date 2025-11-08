// Pipeline stages are split into focused modules to keep pipeline.rs manageable.
// Each submodule extends `impl Pipeline` with its stage methods.

pub(super) mod frontend;
pub(super) mod normalize;
pub(super) mod typing;
pub(super) mod const_eval;
pub(super) mod transpile;
pub(super) mod project;

