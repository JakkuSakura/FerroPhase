# fp-native

An experimental LLVM-free native backend for FerroPhase.

## Status

This crate is intentionally minimal right now: it emits tiny Mach-O/ELF/PE binaries and linkable objects in-process. The entrypoint returns `0` and avoids external toolchains.

Emitter vs linker:
- The emitter is responsible for arch-specific machine bytes (x86_64 vs arm64).
- The linker is format-focused (Mach-O/ELF/PE) and should not branch on CPU unless required by the format itself.

Current limitations:
- LIR lowering only supports empty basic blocks with a Return terminator.
- Exit codes are derived from constant return values; other instructions are not implemented yet.

This exists to validate the end-to-end integration (pipeline → object emission → linking) without depending on LLVM or large third-party codegen crates.

## Next steps

- Lower real `LirProgram` into machine code (starting with i64 arithmetic + simple control flow)
- Expand instruction coverage beyond trivial entry stubs
- Add relocations + external calls (e.g. `printf`) and a small runtime library
- Add relocations and symbol resolution for multi-object linking
