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

- `fp_rust::RustPackageProvider` discovers real `.rs`/Cargo projects (via
  `fp_lang::project`'s Cargo/Magnet manifest walking, the same discovery
  `CargoWorkspaceProvider` uses) and parses them with `fp_rust::RustFrontend`
  — a named Rust frontend distinct from `FerroFrontend`'s `.fp`-dialect
  identity, currently implemented by delegating to `FerroFrontend` internally
  since it's the only Rust-capable parser that exists, but with its own seam
  to grow Rust-specific parsing into.
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
  macro-heavy internals. Measured coverage today (`cargo test -p fp-rust
  measures_real_std_parse_coverage -- --nocapture`): **654/1023 files parse
  (~64%)**. That test asserts a 50% floor as a regression canary, not a
  target — it's meant to catch a wholesale regression, not block gradual
  improvement to `FerroFrontend`'s grammar coverage.
- Directory inputs (`magnet transpile`/`fp compile <dir>`, the case a whole
  project actually hits) now resolve their source language by manifest
  presence (`crate::languages::detect_project_language`): a real
  `Cargo.toml` → `"rust"` → `RustPackageProvider`/`RustStdProvider`; a
  `Magnet.toml`-only project → `"ferrophase"` → `FerroPhaseProvider`, same
  as before. `--source-language` still overrides this when passed
  explicitly. There's no silent fallback: a directory with neither manifest
  is a compile error, not a guess.

## Not done yet

- **Closing the parse-coverage gap.** The ~36% of files that don't parse
  today haven't been triaged into specific grammar gaps (const generics vs.
  inline asm vs. attribute syntax vs. ...) — that triage, and improving
  `FerroFrontend`'s coverage to match, is the remaining work.
