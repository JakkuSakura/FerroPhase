# Rust standard library source (`fp-rust/std`)

## What's there

`crates/fp-rust/std/{core,alloc,std}` is a verbatim copy of the real rustc
standard library source (`core`, `alloc`, `std` — not `.fp` reimplementations),
taken from the local toolchain's `rust-src` component:

```
cp -r "$(rustc --print sysroot)/lib/rustlib/src/rust/library/core/src"  crates/fp-rust/std/core
cp -r "$(rustc --print sysroot)/lib/rustlib/src/rust/library/alloc/src" crates/fp-rust/std/alloc
cp -r "$(rustc --print sysroot)/lib/rustlib/src/rust/library/std/src"   crates/fp-rust/std/std
```

~1000 files, ~16MB. Re-run the above against whatever toolchain `rustup` has
installed to refresh.

`crates/fp-rust/build.rs` bakes every `.rs` file under `std/` into the binary
at compile time (`embedded_std` module, mirroring `fp-lang/src/embedded_std.rs`'s
mechanism for its own hand-written `.fp` std).

## Why

`fp-lang/src/std` is a small, hand-written `.fp` reimplementation of a few
stdlib types (`Option`, `Result`, `Vec`, ...) with only the methods anyone
happened to need so far. It's what `magnet transpile`/`fp compile` resolves
`std::*` paths against for `.fp`-dialect projects (via
`fp_lang::provider::FerroPhaseProvider` → its own `embedded_std`).

That's fine for keeping the untyped transpile pipeline moving, but it means
HIR typechecking (`PipelineMode::TypecheckedTranspile`) can only ever resolve
the handful of stdlib APIs someone bothered to hand-write. Real Rust projects
use far more of `std` than that — `HashSet`, `Arc`, `std::sync::atomic::*`,
`Command`, etc. — most of which the hand-written `.fp` std doesn't declare.

## Current state

- `fp_rust::RustPackageProvider` discovers and parses real `.rs`/Cargo
  projects — currently by delegating straight to
  `fp_lang::cargo_provider::CargoWorkspaceProvider` (same discovery +
  `FerroFrontend` parsing), since there's no Rust-specific parser yet.
- `fp_rust::RustStdProvider` serves the `"std"`/`"libc"` package IDs: `"std"`
  from this directory's embedded real rustc source, `"libc"` delegated
  straight to `fp_lang::embedded_libc` (C ABI declarations aren't
  Rust-specific, no need to duplicate them).
- Files that fail to parse are skipped with a warning rather than failing
  the whole `"std"` package load, so whatever subset *does* parse stays
  usable. Real std is far more complex than anything `FerroFrontend` has
  been validated against — heavy `unsafe`, `#[lang = "..."]` items,
  `#[stable]`/`#[unstable]`/`#[rustc_...]` attributes, `cfg`-gated platform
  code, const generics, specialization, inline asm in a few spots, SIMD,
  macro-heavy internals. Expect a low parse success rate until
  `FerroFrontend`'s grammar coverage grows to match.
- Directory inputs (`magnet transpile`/`fp compile <dir>`, the case a whole
  project actually hits) now resolve their source language by manifest
  presence (`crate::languages::detect_project_language`): a real
  `Cargo.toml` → `"rust"` → `RustPackageProvider`/`RustStdProvider`; a
  `Magnet.toml`-only project → `"ferrophase"` → `FerroPhaseProvider`, same
  as before. `--source-language` still overrides this when passed
  explicitly. There's no silent fallback: a directory with neither manifest
  is a compile error, not a guess.

## Not done yet

- **Parse coverage.** No measurement yet of what fraction of real std
  actually parses successfully, or which specific grammar gaps block the
  rest.
