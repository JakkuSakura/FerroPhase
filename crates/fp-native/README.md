# fp-native

An experimental LLVM-free native backend for FerroPhase.

## Status

This crate is intentionally minimal right now: it emits a tiny Mach-O object containing a `_main` symbol that returns `0`, then links it into an executable using the system `clang`.

This exists to validate the end-to-end integration (pipeline → object emission → linking) without depending on LLVM or large third-party codegen crates.

## Next steps

- Lower real `LirProgram` into machine code (starting with i64 arithmetic + simple control flow)
- Add arm64 Mach-O emission
- Add relocations + external calls (e.g. `printf`) and a small runtime library
- Add ELF/COFF support and/or an in-process linker

