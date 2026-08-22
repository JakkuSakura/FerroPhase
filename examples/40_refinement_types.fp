#!/usr/bin/env fp interpret
//! Refinement types: `{binder: Type where predicate}` narrows a base type
//! by a boolean predicate over a bound variable (Lean 4's `{x : T // P}`
//! notation, spelled with `where` here since `//` is already this
//! language's line-comment marker). Checked for real at compile time by
//! two decision procedures modeled on Lean's own `decide`/`omega` tactics
//! -- never a runtime assertion, never an external SMT solver:
//!   - `decide`: exact evaluation when the value is fully concrete
//!     (a literal).
//!   - `omega`: a self-contained linear-arithmetic decision procedure for
//!     symbolic values, using whatever facts are already known (e.g. an
//!     unsigned integer's implicit `>= 0`).
//! A predicate outside that decidable fragment (comparisons, `+ - * /`,
//! `&&`, literals, variable references) is a compile error asking for a
//! stronger annotation, never a silent runtime check.

fn main() {
    println!("📘 Tutorial: 40_refinement_types.fp");
    println!("🧭 Focus: refinement types checked by decide/omega at compile time");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");

    // `decide` path: 42 is a literal, so `42 >= 0` is evaluated exactly at
    // compile time -- no runtime check is ever emitted.
    let percent: {p: i64 where p >= 0 && p <= 100} = 42;
    println!("percent (literal, decide-checked) = {}", percent);

    // `omega` path: `count` is a runtime value, not a literal, but its
    // declared type (`u64`) already carries the implicit fact `count >= 0`
    // -- omega proves the refinement from that fact alone, symbolically,
    // with no evaluation of `count` at all.
    let count: u64 = 7;
    let n: {x: u64 where x >= 0} = count;
    println!("n (symbolic, omega-checked) = {}", n);

    // A refined value is always usable wherever its base type is expected
    // (narrowing to a subtype is free) -- ordinary arithmetic on `n` works
    // exactly as it would on a plain `u64`.
    println!("n + 1 = {}", n + 1);

    // Refinements compose under `&&`: `percent` above already proved both
    // halves of its range at once.
    println!("percent is a valid 0-100 value: {}", percent >= 0 && percent <= 100);

    // Violations are compile errors, not runtime surprises -- e.g.
    // `let bad: {p: i64 where p >= 0} = -1;` fails to compile with
    // "refinement predicate violated at compile time", and a predicate
    // using anything outside the decidable fragment (a function call, a
    // nonlinear term, `||`) fails to compile with "outside supported
    // linear-arithmetic fragment" -- neither case is silently accepted or
    // deferred to a runtime assertion.
}
