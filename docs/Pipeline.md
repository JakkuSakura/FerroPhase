# Pipeline Architecture

FerroPhase's pipeline is organised as a share-nothing driver that threads an
explicit `CompilationContext` and diagnostics between functional stages. The
frontend now produces a language-specific AST (LAST) snapshot alongside the
canonical AST, ensuring multi-language inputs flow through the same lowerings.

## Stage Overview

1. **Frontend (Source → LAST → AST)**
   - Each `LanguageFrontend` normalises source into a LAST snapshot and the
     unified AST (`fp_core::ast::Node`).
   - A language-specific serializer (`Arc<dyn AstSerializer>`) accompanies the
     result; it is registered with `register_threadlocal_serializer` before
     const-eval or interpretation.
   - The frontend stores provenance (`FrontendSnapshot`) inside the
     `CompilationContext` so later tooling can persist or inspect LAST data.

2. **Const Evaluation (AST → EAST)**
   - `ConstEvaluationOrchestrator` consumes the AST and produces an evaluated
     AST with macro expansion and compile-time execution applied.
   - Results from this phase (const tables, globals) are recorded on the
     `CompilationContext` for consumption by lower stages.

3. **Lowering Steps**
   - **AST → HIR** (`HirGenerator`)
   - **HIR → THIR** (`ThirGenerator`)
   - **THIR → MIR** (`MirGenerator`)
   - **MIR → LIR** (`LirGenerator`, seeded with const results)
   - **LIR → LLVM IR** (fp-llvm backend)

   Each step returns a `StageReport<T>` containing the produced IR (or a
   placeholder in tolerant mode), an updated `CompilationContext`, and a list of
   `PipelineDiagnostic` entries. Diagnostics are printed stage-by-stage but also
   bubble up for aggregate reporting.

4. **Binary Emission (optional)**
   - If the target is `Binary`, the LLVM artefact is passed to `llc` and the
     configured linker via `BinaryCompiler` with diagnostics folded into the
     final report.

## CompilationContext

```rust
#[derive(Clone)]
pub struct CompilationContext {
    pub const_results: HashMap<String, Value>,
    pub globals: HashMap<String, Value>,
    pub types: HashMap<String, String>,
    pub imports: HashMap<String, String>,
    pub serializer: Option<Arc<dyn AstSerializer>>,
    pub source_language: Option<String>,
    pub frontend_snapshot: Option<FrontendSnapshot>,
}
```

The context acts as the explicit thread that replaces the previous implicit,
mutable state. Every stage receives the context by value and returns an updated
copy. THIS allows:

- Serializers to be reused by interpretation or downstream emitters.
- LAST snapshots to be persisted alongside intermediates.
- Const results and globals to feed low-level transformations without hidden
  globals.

## Diagnostics

`PipelineDiagnostic` captures stage, severity, message, optional span, and
suggestions. Stages append diagnostics even when they recover with placeholder
IR (when `ErrorToleranceOptions` allows). `emit_diagnostics` honours verbose
flags for informational entries but always surfaces warnings and errors.

## Multi-Language Frontends

- `FrontendRegistry` maps language identifiers and file extensions to
  `LanguageFrontend` implementations. The default registry ships with the
  `RustFrontend`, covering FerroPhase syntax and Rust-compatible `.rs` files.
- `PipelineOptions.source_language` allows callers to override detection when an
  expression lacks a file extension.
- Additional frontends should implement `LanguageFrontend::parse`, returning a
  LAST snapshot and serializer tailored to the language. LAST is stored in the
  context so future tooling can persist or inspect it alongside canonical AST
  artefacts.

### CLI integration

- The `fp compile` command now accepts `--lang <identifier>` (alias `--language`)
  to feed `PipelineOptions.source_language`. Use it when file extensions are
  ambiguous (e.g., stdin or embedded expressions) or when forcing a particular
  frontend during experiments.
- `--save-intermediates` persists stage outputs (_including the LAST snapshot_
  as part of the context hand-off) regardless of the selected language.

## Intermediates & Persistence

When `save_intermediates` is enabled, each stage writes its artefact into the
`base_path` with a conventional extension (`.east`, `.hir`, `.thir`, `.mir`,
`.lir`, `.ll`). The driver handles the IO so the stage helpers remain pure.

## Error Tolerance

Stages cooperate with `ErrorToleranceOptions`. If tolerance is enabled and the
stage can produce a placeholder artefact, it returns it while emitting
`DiagnosticLevel::Error` entries. Fatal failures surface as `CliError::Compilation`.

## Runtime & Interpretation

Interpretation now reuses the serializer selected by the frontend, ensuring that
runtime semantics stay in sync with the LAST/AST produced by multi-language
inputs.

## Extending the Pipeline

To add a new source language:

1. Implement `LanguageFrontend` for the language, producing LAST metadata and a
   serializer.
2. Register the frontend with `FrontendRegistry` (mapping language key and file
   extensions).
3. Ensure the new frontend maps its standard library to the canonical `std`
   vocabulary before returning the AST.
4. Optionally persist the LAST snapshot (`FrontendSnapshot::serialized`) for
   debugging tooling.

With the registry in place, the pipeline automatically detects the language
based on `PipelineOptions.source_language` or the file extension, and the share-
 nothing driver processes the rest identically.
