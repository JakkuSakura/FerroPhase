use fp_core::intrinsics::IntrinsicMaterializer;

/// AST-level backend strategy for `PipelineMode::Native` compiled through
/// this crate's LLVM-free backend (`NativeEmitter`/`emit`), and for the
/// compiler's own LIR comptime evaluator (`eval`, `PipelineMode::Native`
/// without a real emit step) — both share the same portable-op story.
///
/// Every genuine compiler intrinsic (`IntrinsicKind::Print`/`Println`/
/// `Format`, reflection intrinsics like `sizeof`/`type_of`, ...) is already
/// executed directly by the MIR/LIR pipeline
/// (`hir_to_mir::expr`/`mir_to_lir::instr` in `fp-backend`, and this crate's
/// own `libc`/`emit` modules at the asm-emission stage) — unlike LLVM
/// codegen, which has no builtin `print` and needs
/// `LlvmRuntimeIntrinsicMaterializer` to lower `Print`/`Println` into a real
/// `printf` call first. Nothing here needs AST-level lowering for those.
///
/// Every portable op (`fs::read_to_string`, `Option::unwrap_or`, ...) is
/// deliberately left alone too: those already have a real implementation in
/// the vendored std source, callable like any other function. Normalization
/// only ever reclassifies them into a portable `OpKind` under `Transpile`/
/// `TypedTranspile`, where a *target* backend's own materializer (e.g.
/// `KotlinMaterializer`) needs the shortcut because that target language has
/// no equivalent std function of its own to call directly — `Compile`
/// (Native) mode's normalizer already skips that reclassification entirely
/// (see `fp_lang::normalization::FerroIntrinsicNormalizer::normalize_invoke`),
/// so nothing reaches this materializer that would need rewriting.
///
/// Exists as an explicit, named strategy (rather than reusing
/// `NoopIntrinsicMaterializer`) so the Native pipeline's provider wiring
/// documents *why* it needs no AST-level materialization, and has a
/// concrete place to grow into if this backend ever needs lowering of its
/// own before HIR lowering runs (mirroring `libc::materialize`, which
/// already does backend-specific lowering, just at the asm level instead of
/// the AST level).
#[derive(Debug, Default, Clone, Copy)]
pub struct NativeIntrinsicMaterializer;

impl IntrinsicMaterializer for NativeIntrinsicMaterializer {}
