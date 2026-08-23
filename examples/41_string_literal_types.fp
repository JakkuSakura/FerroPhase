#!/usr/bin/env fp interpret
//! String literal types and TypeScript-style template literal types.
//!
//! A string literal in type position (`"foo"`) is its own type, not
//! immediately widened to `str` -- and a union of them (`"a" | "b"`) is a
//! real, checked union, not rejected the way a union of arbitrary types
//! still is. Both erase to plain `str` after typecheck, so any value typed
//! with one of these aliases behaves exactly like an ordinary string at
//! runtime -- the checking only matters at compile time.
//!
//! "Template literal types" aren't a separate piece of grammar here: type
//! position is already a const-eval context, so a bare `f"..."` type is
//! just an ordinary format string, comptime-evaluated the same way an
//! explicit `const { f"..." }` block would be.
//!
//! Distributing a function over every member of a literal union (the
//! TypeScript behavior `` `a${"x"|"y"}` `` => `"ax"|"ay"` relies on) is
//! `std::intrinsics::unionify`: `unionify(f)` returns a closure -- not
//! currying, just a fixed 1-argument intrinsic that returns a callable
//! value -- and calling that closure with a reflected union type applies
//! `f` to each member and rebuilds the union. See
//! `fp-interpret/src/lib.rs`'s `unionify_closure_maps_over_union_members`
//! test and `fp-typing/src/hir_typeck.rs`'s
//! `union_of_string_literal_types_resolves_to_str` test for this
//! mechanism exercised directly.

const GREETEE: &str = "world";

// A plain string literal type.
type Up = "up";

// A union of string literal types.
type Dir = "up" | "down";

// A bare f-string in type position -- implicitly const-evaluated, exactly
// like `const { f"hello, {GREETEE}!" }` would be.
type Greeting = f"hello, {GREETEE}!";

fn main() {
    println!("📘 Tutorial: 41_string_literal_types.fp");
    println!("🧭 Focus: string literal types, unions of them, and f-string types");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");

    let one: Up = "up";
    println!("one (single literal type) = {}", one);

    let dir: Dir = "down";
    println!("dir (union-of-literals type) = {}", dir);

    let greeting: Greeting = f"hello, {GREETEE}!";
    println!("greeting (f-string type) = {}", greeting);

    // Both string literal types and unions of them erase to plain `str`
    // after typecheck, so ordinary `str` operations work on them exactly
    // as they would on any other string.
    println!("dir starts_with \"do\": {}", dir.starts_with("do"));
}
