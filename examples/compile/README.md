# Compile Mode Example

This example accompanies `docs/CompileQuickstart.md` and demonstrates compiling a FerroPhase program through EAST, THIR,
TAST, and native code generation.

## Files

- `src/main.fp` – Primary source with const evaluation, quoting, and a `main` function.
- `FerroPhase.toml` – Minimal manifest.

## Usage

```
$ fp compile src/main.fp --target native --out target/bin/compile_example \
    --save-intermediates
$ ./target/bin/compile_example
```

Inspect intermediates:

```
$ cat target/east/src_main.east
$ cat target/thir/src_main.thir
$ cat target/tast/src_main.tast
```

