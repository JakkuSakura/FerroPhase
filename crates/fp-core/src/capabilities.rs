/// What the target language a package is ultimately being compiled to can
/// express directly — lets shared, target-agnostic lowering passes (e.g.
/// closure defunctionalization, `for`-loop desugaring into an index-based
/// `while`) decide whether a surface construct needs pre-decomposing into
/// simpler HIR shapes, or can survive as a real, first-class HIR node
/// because the eventual target already has native syntax/stdlib support
/// for it.
///
/// Each target-emitting crate (`fp-kotlin`, `fp-cil`, ...) that wants
/// anything other than the conservative default declares its own `const`
/// of this type (see `fp-kotlin`'s `CAPABILITIES`); `fp-cli` is the only
/// place that maps a requested output language to the right one
/// (`languages::backend::capabilities_for_target`) and threads it through
/// to the compiler driver. Every pipeline that never opts in (in
/// particular `PipelineMode::Native`, which lowers to MIR — a
/// closure-and-iterator-free IR) keeps exactly today's desugaring
/// behavior via `LanguageCapabilities::NATIVE`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct LanguageCapabilities {
    /// A closure literal can be emitted/lowered directly as the target's
    /// own lambda syntax, rather than needing defunctionalization
    /// (decomposition into an ordinary struct + function pair) before HIR
    /// generation even runs.
    pub first_class_closures: bool,
    /// A `for pat in iter_expr { .. }` can be emitted/lowered directly via
    /// the target's own `for`/`foreach` construct plus its native
    /// collection/iterator methods (e.g. Kotlin's `MutableList.take(n)`),
    /// rather than needing eager desugaring into an index-based `while`
    /// loop before HIR generation.
    pub first_class_for_loops: bool,
}

impl LanguageCapabilities {
    /// The conservative default: nothing is first-class, matching every
    /// pre-existing caller's behavior exactly. Used by any pipeline/target
    /// that hasn't opted into anything more (in particular
    /// `PipelineMode::Native`).
    pub const NATIVE: Self = Self {
        first_class_closures: false,
        first_class_for_loops: false,
    };
}
