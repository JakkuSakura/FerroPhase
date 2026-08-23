use std::collections::HashMap;

use fp_core::ast::{Expr, Item, MacroInvocation, MacroRulesDef, MacroTokenTree};
use fp_core::error::Result;
use fp_core::intrinsics::{IntrinsicNormalizer, NormalizeOutcome};
use fp_lang::FerroIntrinsicNormalizer;

/// Rust-specific wrapper around `fp_lang::FerroIntrinsicNormalizer` — the
/// one place real Rust macro-visibility semantics (right now: disambiguating
/// a same-named `macro_rules!` collision by preferring the shallower-nested
/// definition, a coarse stand-in for real `#[macro_use]`/module scoping)
/// belong, rather than baked into `fp-lang`'s generic, language-agnostic
/// macro engine (`fp_lang::macro_parser`). Every other hook delegates
/// straight through to the wrapped `FerroIntrinsicNormalizer`, since
/// nothing else about real Rust source needs different behavior yet — see
/// `RustFrontend`'s own doc comment for the same "give Rust-specific
/// behavior its own home to grow into, without entangling fp-lang's shared
/// engine" rationale.
///
/// Concrete motivating case (real vendored std): `core::num::uint_macros`'s
/// real `uint_impl!` (`Self = ..., ActualT = ..., ...`, actually invoked by
/// `impl u8 { uint_impl! { .. } }`) and `core::num::imp::int_bits`'s
/// unrelated, much simpler `uint_impl!` (`($U:ident) => { mod $U { .. } }`,
/// private to its own module) share a name. `fp_lang::collect_macro_rules_
/// defs`'s generic last-one-wins flattening let whichever file was visited
/// last silently clobber the other, dropping every real `uint_impl!`
/// invocation (and every integer-primitive method it generates) whenever
/// the private one won. Preferring the definition with the shallower
/// module path resolves this correctly.
pub struct RustIntrinsicNormalizer {
    inner: FerroIntrinsicNormalizer,
}

impl RustIntrinsicNormalizer {
    pub fn new() -> Self {
        Self {
            inner: FerroIntrinsicNormalizer::new(),
        }
    }

    pub fn with_macro_rules_defs(mut self, defs: HashMap<String, MacroRulesDef>) -> Self {
        self.inner = self.inner.with_macro_rules_defs(defs);
        self
    }
}

impl IntrinsicNormalizer for RustIntrinsicNormalizer {
    fn normalize_expr(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.inner.normalize_expr(expr)
    }

    fn normalize_call(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.inner.normalize_call(expr)
    }

    fn normalize_macro(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.inner.normalize_macro(expr)
    }

    fn normalize_invoke(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.inner.normalize_invoke(expr)
    }

    fn normalize_match(&self, expr: Expr) -> Result<NormalizeOutcome<Expr>> {
        self.inner.normalize_match(expr)
    }

    fn expand_item_macro(
        &self,
        invocation: &MacroInvocation,
        defs: &HashMap<String, MacroRulesDef>,
    ) -> Option<Vec<Item>> {
        self.inner.expand_item_macro(invocation, defs)
    }

    fn parse_macro_rules_def(&self, name: &str, tokens: &[MacroTokenTree]) -> MacroRulesDef {
        self.inner.parse_macro_rules_def(name, tokens)
    }

    /// Overridden: see this type's own doc comment. Prefers the definition
    /// declared closer to the crate root whenever the shared single-pass
    /// walker (`ast_to_hir::expand_item_macros`) finds two `macro_rules!`
    /// definitions sharing a name.
    fn prefer_macro_rules_def(&self, existing_depth: usize, new_depth: usize) -> bool {
        new_depth < existing_depth
    }
}
