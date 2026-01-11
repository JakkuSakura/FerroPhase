// Pipeline stages are split into focused modules to keep pipeline.rs manageable.
// Each submodule extends `impl Pipeline` with its stage methods.

pub(super) mod const_eval;
pub(super) mod frontend;
pub(super) mod link;
pub(super) mod lowering;
pub(super) mod materialize;
pub(super) mod normalize;
pub(super) mod typing;
pub(super) mod wasm;
