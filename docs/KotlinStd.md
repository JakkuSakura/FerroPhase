# Kotlin standard library source (`fp-kotlin/std`)

## What's there

`crates/fp-kotlin/std/kotlin` is a verbatim copy of the real Kotlin
compiler repo's common stdlib declaration source (`libraries/stdlib/src/
kotlin` — not reimplementations), taken from a shallow/sparse clone of
`https://github.com/JetBrains/kotlin` (not vendored as a git submodule):

```
git clone --depth 1 --filter=blob:none --sparse \
    https://github.com/JetBrains/kotlin.git /tmp/kotlin-vendor-scratch/repo
cd /tmp/kotlin-vendor-scratch/repo
git sparse-checkout set libraries/stdlib/src
cp -r libraries/stdlib/src/kotlin  <ferrophase>/crates/fp-kotlin/std/kotlin
rm -rf /tmp/kotlin-vendor-scratch
```

Vendored from commit `b23447d5b2db80e0520571d0cec12a3c2ef8d31a`
(2026-08-14, `master`). 157 files, ~30,600 lines, ~1.5MB — this is the
*common* source set (`collections/`, `text/`, `io/`, `ranges/`, ...),
not the JVM-specific `external`/intrinsic stub variants, which have
little real declaration content of their own.

## Why

Mirrors `fp-rust/std` (see `docs/RustStd.md`): a hand-written `.fp`/ad
hoc stdlib mapping only ever covers what someone happened to need. This
gives `fp-kotlin` real Kotlin stdlib declarations to validate its own
(forthcoming) declaration-only parser against, the same way `fp-rust`'s
`RustFrontend` is validated against real vendored rustc source.

Unlike `fp-rust/std`, this is **not yet wired into a build-time
embedded-source mechanism** (no `fp-kotlin/build.rs`/`embedded_std`
equivalent) — this first pass is source-on-disk only, read directly by
tests/tooling. Embedding can follow the same pattern as `fp-rust/
build.rs` once there's a real consumer that needs it baked into the
binary.

## Current state

- Declaration-only Kotlin parser: see `crates/fp-kotlin/src/kt_parser/`
  (parses `fun`/`class`/`interface`/`object`/top-level `val`/`var`
  signatures; does not parse function/property bodies).
- Coverage measured by `cargo test -p fp-kotlin
  measures_vendored_stdlib_parse_coverage -- --nocapture`: **157 files,
  45 fully clean (zero warnings), 108 with partial warnings (still
  recovering most declarations via per-declaration resync), 4 hard
  errors (whole file unparseable, usually a raw/triple-quoted string or
  construct the tokenizer/grammar doesn't yet cover) — 1,189
  declarations recovered total.** Same spirit as `fp-rust`'s coverage
  canary: a baseline to improve from, not a target.

## Not done yet

- Body parsing (expressions, control flow) — declarations only so far.
- A `LanguageFrontend` impl for Kotlin-as-a-source-language (needs body
  parsing first).
- Wiring into a build-time embedded-source mechanism, if/when something
  needs the vendored source baked into the `fp` binary rather than read
  from disk.
