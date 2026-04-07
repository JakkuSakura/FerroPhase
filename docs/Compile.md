# Compile Mode Quickstart

This guide walks through compiling a FerroPhase project to a native target using the current toolchain. It assumes the
codebase matches the architecture described in `docs/Design.md` and that const evaluation behaves as detailed in
`docs/ConstEval.md`.

## Prerequisites

- Rust toolchain (stable) for building `fp` binaries
- LLVM toolchain (llc + clang or compatible) available on PATH
- `fp` CLI built from `crates/fp-cli`

Check availability:

```bash
$ fp --version
$ fp compile --help
```

## Project Setup

Create a new project using the CLI (this example uses the `basic` template):

```bash
$ magnet init my_project --template basic
$ cd my_project
```

Project layout:

```
my_project/
├── src/
│   └── main.fp
├── FerroPhase.toml
└── README.md
```

## Writing Code

Example `src/main.fp` demonstrating const evaluation, quoting, and a simple entry point:

```ferrophase
const API_NAME: str = "FerroPhase";

const GREETING = quote item {
    fn message() -> str {
        "Hello from quote/splice!"
    }
};

const GENERATED: () = splice GREETING;

fn main() {
    println!("Welcome to {}", API_NAME);
    println!("{}", message());
}
```

Highlights:
- `API_NAME` is computed at compile time.
- `quote`/`splice` demonstrate structured metaprogramming (see `docs/Quoting.md`). `quote` yields an AST token whose
  type is inferred by the compiler.
- `main` is a standard entry point.

## Running Const Evaluation

To inspect const-eval output and verify the typed AST, run:

```bash
$ fp compile src/main.fp --emit ast --emit east
```

Artifacts (paths depend on your configuration):
- `target/ast/src_main.ast` – Normalised AST snapshot
- `target/ast/src_main.ast-typed` – Typed AST (`ASTᵗ`)
- `target/ast/src_main.ast-eval` – Post-const-eval AST (`ASTᵗ′`)
- `target/hir/src_main.hir` – HIR emitted from the current type-enrichment stage

## Building to Native (LLVM target)

> ⚠️ The native backend is temporarily unavailable while the typed AST/HIR
> refactor lands. The CLI currently stops after producing the typed AST and
> HIR snapshots.

Common flags (still accepted for future use):
- `--opt {0|1|2|3}` – Optimisation level (default: 2)
- `--debug` – Include debug info
- `--emit {ast,ast-typed,ast-eval,hir}` – Persist intermediates for inspection
- `--save-intermediates` – Shortcut to emit all intermediates

## Inspecting Intermediates

```bash
$ fp compile src/main.fp --emit ast-typed --emit ast-eval --emit hir
$ cat target/ast/src_main.ast-typed
$ cat target/ast/src_main.ast-eval
```

- The typed AST shows the solver’s view before evaluation.
- The evaluated AST includes const-folded expressions and generated items.

## Inspecting Produced Artifacts

Use the `inspect` command to read compiled artifacts back out of their binary form:

```bash
$ fp inspect target/main.fbc
$ fp inspect target/main.fbc --export-text-bytecode target/main.ftbc
$ fp inspect target/main --hex --max-bytes 128
```

- `.fbc` files are decoded as FerroPhase binary bytecode and summarised by entrypoint, constants, and functions.
- `--export-text-bytecode` converts `.fbc` into the stable textual `.ftbc` representation for review and diffing.
- Native/object binaries are summarised by file kind, architecture, sections, symbols, and code sections.
- `--hex` prints a bounded raw byte preview for any inspected artifact.
- Auto-detection is header-based across these families: FerroPhase binary bytecode, FerroPhase text bytecode, JVM class files, Unix archives, WebAssembly modules, and native/object containers (`ELF`, `Mach-O`, `PE`, `COFF`, `XCOFF`, dyld cache). Unknown files fall back to raw bytes.

Current scope:
- FerroPhase can inspect and transcode its own bytecode representation.
- Native executables can be inspected structurally, but there is not yet a reverse pipeline from machine code back into MIR/AST, so high-level decompilation is intentionally out of scope for now.

## Textual Backends

FerroPhase also supports text-oriented backend targets for inspection and experimentation:

```bash
$ fp compile src/main.fp --backend ebpf -o main.ebpf
$ fp compile src/main.fp --backend cil -o main.il
$ fp compile src/main.fp --backend dotnet -o main.exe
```

- `--backend ebpf` emits an experimental eBPF assembly sketch.
- `--backend ebpf -o main.o` emits an ELF eBPF object suitable for execution.
- `--backend cil` emits experimental .NET CIL in textual `.il` form.
- `--backend dotnet` emits a PE-format .NET assembly by assembling generated CIL through `ilasm`.
- `--backend dotnet` defaults to `.exe`; use a `.dll` output path to emit a library instead.
- `--backend ebpf --exec` is supported through an external runtime executable exposed via `FP_EBPF_RUNTIME`.
- `FP_EBPF_RUNTIME_ARGS` can be used for wrapper commands such as `cargo run -q -p fp-ebpf --bin fp-ebpf-runtime --`.
- `--exec` is not currently supported for `cil` targets.
- `--backend dotnet --exec` is supported when `ilasm` and a suitable runtime (`mono`, or `dotnet` as fallback for `.dll`) are available.

## Running with .NET Backend

Use `fp compile` with `--backend dotnet --exec` to compile and run immediately:

```bash
$ fp compile src/main.fp --backend dotnet --exec
$ fp compile src/main.fp --backend dotnet --exec --output app.exe
$ fp compile src/main.fp --backend dotnet --exec --output app.dll
$ fp compile src/main.fp --backend dotnet --exec --release -O3
```

- `--backend dotnet --exec` compiles to a .NET assembly and executes it immediately.
- `--output app.exe` keeps the executable artifact on disk before running it.
- `--output app.dll` keeps a library-style assembly on disk; on Unix-like systems FerroPhase runs it via `mono`.
- `fp compile --exec` also accepts core compile-mode knobs such as `--emitter`, `--native-target`, `--debug`, `--release`, and `-O/--opt-level`.

## Release Artifacts

Production builds must emit the release artifacts defined in
`docs/ReleaseArtifacts.md`. When `--release` is enabled, record:

- Toolchain and build options (see `docs/BuildOptions.md`).
- Output paths (`target/` or `--output` overrides).
- Backend-specific intermediates saved with `--save-intermediates`.

The release record is required for reproducibility audits.

## Troubleshooting

- **Missing LLVM tools**: `fp compile` reports if `llc`/`clang` are unavailable. Install via your package manager or set
  `FP_LLVM_PATH`.
- **Const eval errors**: Review the `ast-eval` artefact. Diagnostics map back to original spans.
- **Target mismatch**: Ensure `docs/Design.md` cross-stage guarantees hold—the evaluated AST should match quoted/spliced code.

## Next Steps

- Explore bytecode mode: `fp bytecode src/main.fp --emit bytecode`
- Generate Rust output (currently disabled while the new pipeline lands)
- Extend the project with modules and custom targets; update `FerroPhase.toml` accordingly.
