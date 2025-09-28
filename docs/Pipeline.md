# Pipeline Architecture

FerroPhase's pipeline is organised as a share-nothing driver that threads
diagnostics between functional stages while the `Pipeline` struct caches the
frontend serializer, detected language, and snapshot metadata. The
frontend now produces a language-specific AST (LAST) snapshot alongside the
canonical AST, ensuring multi-language inputs flow through the same
lowerings. The updated flow centres typed interpretation on THIR before
resugaring to TAST and, for compilation backends, re-projecting into the
optimisation IR stack.

## Stage Overview

1. **Frontend (Source → LAST → Annotated AST)**
   - Each `LanguageFrontend` normalises source into a language-specific LAST
     snapshot and the canonical AST (`fp_core::ast::Node`).
   - Annotation captures language metadata (imports, attributes, serializer)
     so later tooling can recover front-end specifics.
   - The frontend stores provenance (`FrontendSnapshot`) on the pipeline so
     later tooling can persist or inspect LAST data without re-parsing.

2. **Parsing & Normalisation (AST → AST)**
   - Macro expansion, annotation, and canonical std remapping execute in this
     stage while spans are preserved.
   - The output is the normalised AST that every downstream mode consumes.

3. **Desugaring & Typing (AST → HIR → THIR)**
   - `HirGenerator` removes surface sugar and resolves bindings.
   - `ThirGenerator` runs Algorithm W style inference to produce a typed HIR
     (THIR). Types are attached to every expression and pattern.

4. **Typed Interpretation (THIR → THIR′)**
   - Compile-time and runtime interpretation operate directly on THIR.
   - The resulting THIR′ snapshot captures any compile-time structural edits and becomes the authoritative typed program after evaluation.

5. **Resugaring (THIR′ → TAST → LAST′)**
   - A lifting phase rebuilds a typed AST (TAST) that mirrors surface syntax
     while preserving the THIR types and spans.
   - Language-specific generators can rehydrate a LAST′ view for transpilers or
     tooling that prefers front-end formats.

6. **Backend Lowering (Compile / Optimisation Modes)**
   - For native/optimised targets, the driver re-projects the evaluated program:
     `TAST → HIR → THIR → MIR → LIR → LLVM IR`.
   - MIR/LIR benefit from evaluation results (folded consts, generated structs)
     while preserving deterministic spans from the original source.

Each step returns a `DiagnosticReport<T>` containing the produced artefact (or a
placeholder in tolerant mode) and the collected `Diagnostic` entries. The
pipeline updates its cached serializer/language metadata in place, and
diagnostics are printed stage-by-stage while also bubbling up for aggregate
reporting.

7. **Binary Emission (optional)**
   - If the target is `Binary`, the LLVM artefact is passed to `llc` and the
     configured linker via `BinaryCompiler` with diagnostics folded into the
     final report.

## Pipeline State

`Pipeline` now owns the lightweight metadata that previously lived inside a
`CompilationContext`:

- `serializer: Option<Arc<dyn AstSerializer>>` – reused by const-eval and
  interpretation to serialise THIR/values.
- `source_language: Option<String>` – records the detected or user-specified
  language for diagnostics and downstream tooling.
- `frontend_snapshot: Option<FrontendSnapshot>` – keeps LAST provenance handy
  for emitters or persistence features.

These fields are reset at the start of `execute_with_options` and repopulated
after parsing so subsequent stages can access them without threading an
explicit context object.

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
  LAST snapshot and serializer tailored to the language. LAST provenance is
  stored on the pipeline so future tooling can persist or inspect it alongside
  canonical AST artefacts.

### CLI integration

- The `fp compile` command now accepts `--lang <identifier>` (alias `--language`)
  to feed `PipelineOptions.source_language`. Use it when file extensions are
  ambiguous (e.g., stdin or embedded expressions) or when forcing a particular
  frontend during experiments.
- `--save-intermediates` persists stage outputs (including the cached LAST
  snapshot) regardless of the selected language.

## Intermediates & Persistence

When `save_intermediates` is enabled, each stage writes its artefact into the
`base_path` with a conventional extension (`.ast`, `.hir`, `.thir`, `.ethir`, `.tast`,
`.mir`, `.lir`, `.ll`). The driver handles the IO so the stage helpers remain
pure.

## Error Tolerance

Stages cooperate with `ErrorToleranceOptions`. If tolerance is enabled and the
stage can produce a placeholder artefact, it returns it while emitting
`DiagnosticLevel::Error` entries. Fatal failures surface as `CliError::Compilation`.

## Runtime & Interpretation

Interpretation (interactive `run`/`eval`) now flows through the same AST → HIR
→ THIR → const-eval pipeline used for compilation. The typed interpreter reuses
the frontend serializer, executes THIR bodies in either const or runtime mode,
and currently produces owned runtime values via the same evaluation machinery.

The runtime path still needs richer semantics (ownership-aware operations,
language intrinsics), but the driver no longer depends on the removed
`RuntimePass` abstraction.

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
