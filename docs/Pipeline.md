# Pipeline Architecture

FerroPhase's pipeline is organised as a share-nothing driver that threads
diagnostics between functional stages while the `Pipeline` struct caches the
frontend serializer, detected language, and snapshot metadata. The frontend
produces a language-specific AST (LAST) snapshot alongside the canonical AST so
multi-language inputs flow through the same normalisation passes. The new
design removes THIR entirely: type inference now annotates the canonical AST in
place, the interpreter operates directly on that typed AST, and the remaining
lowerings consume the already-typed structures.

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

3. **Type Enrichment (AST → ASTᵗ)**
   - An Algorithm W style inferencer walks the normalised AST, resolving names
     and constraints while writing inferred types back onto AST nodes
     (`ast::Expr.ty`, `ast::Pattern.ty`, …).
   - The output is the same structural AST enriched with optional type slots
     (`ASTᵗ`). No HIR/THIR projection occurs during typing.

4. **Typed Interpretation (ASTᵗ → ASTᵗ′)**
   - Compile-time and runtime interpretation operate on the typed AST. Both
     modes share the same evaluator with different configuration flags.
   - Const evaluation may mutate the AST in place (folding expressions,
     synthesising declarations, materialising intrinsic calls). The resulting
     `ASTᵗ′` snapshot becomes the source of truth for subsequent stages.

5. **Typed Projection (ASTᵗ′ → HIRᵗ)**
   - `HirGenerator` consumes the evaluated typed AST and produces a
     type-aware HIR (`HIRᵗ`). Because the AST already carries principal types,
     this step focuses on structural desugaring and borrow checking hooks while
     preserving the attached type metadata.

6. **Backend Lowering**
   - `hir_to_mir` lowers typed HIR into MIR while surfacing diagnostics for
     unsupported constructs. MIR is then converted into LIR and LLVM IR so
     native backends share a single SSA pipeline.

Each step updates the shared `DiagnosticManager`, returning either the produced
artefact (or a placeholder when tolerance is enabled) or a stage failure. The
pipeline maintains its cached serializer/language metadata in place, printing
diagnostics after each stage while also retaining them for aggregate reporting.

7. **Binary Emission**
   - The LLVM bridge writes `*.ll` artefacts and invokes `clang` to link final
     executables when native targets are selected.

## Pipeline State

`Pipeline` now owns the lightweight metadata that previously lived inside a
`CompilationContext`:

- `serializer: Option<Arc<dyn AstSerializer>>` – reused by type inference and
  interpretation to serialise typed AST fragments/values.
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
  `LanguageFrontend` implementations. The default registry currently ships with:
  - `FerroFrontend` for FerroPhase/Rust syntax (`.fp`, `.rs`) — produces runtime types and code.
  - `TypeScriptFrontend` for TypeScript/JavaScript families — also runtime types.
  - `WitFrontend` for WebAssembly Interface Types — treated as *service IDL* + *type IDL* and lowered into regular items.
  - `SqlFrontend` for `.sql` sources — produces *query documents*.
  - `PrqlFrontend` for `.prql` pipelines — also *query documents*.
  - `JsonSchemaFrontend` for `.jsonschema` files — produces *validation schemas* (see `NodeKind::Schema`).
  - `FlatbuffersFrontend` for `.fbs` files — treated as *type IDL* and lowered into struct/enum items (runtime types after generation).
- `NodeKind::Query` carries query documents, while `NodeKind::Schema` captures validation schemas. All other IDL data is normalised into the existing `Item`/`Expr` tree so runtime code generation stays uniform.

### Terminology cheat sheet

Use the following vocabulary when talking about cross-language specifications:

| Term | What it contains | Examples | Notes |
| ---- | ---------------- | -------- | ----- |
| **Service IDL** | Operations/endpoints, signatures, errors/streams | WIT interfaces/worlds, Thrift/Proto services, JSON-RPC method catalogs | Describes callable APIs. Lower these into items (e.g. functions) or keep side metadata. |
| **Type IDL** | Portable data shapes (records, enums, options/results) | WIT type blocks, FlatBuffers tables, Thrift/Proto messages considered abstractly | We normalise these into struct/enum items which later become runtime types per target language. |
| **Runtime types** | Concrete per-language definitions | `struct Foo { … }` in Rust, Python `class Baz`, C# records, TS interfaces | Generated or handwritten code; lives in the usual AST `Item`/`Expr` hierarchy. |
| **Validation schema** | JSON-specific constraints, validation rules | JSON Schema files, OpenAPI component schemas | Captured via `NodeKind::Schema`; never conflated with IDL. |
| **Protocol spec** | Wire format, envelopes, transport mapping | JSON-RPC framing over ZMQ, FlatBuffers envelopes, MsgPack contracts | Track separately when documenting/onboarding but out-of-scope for the AST today. |

When updating existing material, migrate language like “WIT interface” → *Service IDL*, “WIT types” → *Type IDL*, “Rust struct Foo” → *Runtime type*, “JSON schema” → *Validation schema*, and reserve “Protocol spec” for transport-level descriptions.
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

When `save_intermediates` is enabled, each completed stage writes its artefact
into the `base_path` with a conventional extension (`.ast`, `.ast-typed`,
`.ast-eval`, `.hir`). Backends will add their own intermediates once they are
re-enabled.

## Error Tolerance

Stages cooperate with `ErrorToleranceOptions`. If tolerance is enabled and the
stage can produce a placeholder artefact, it returns it while emitting
`DiagnosticLevel::Error` entries. Fatal failures surface as `CliError::Compilation`.

## Runtime & Interpretation

Interpretation (interactive `run`/`eval`) now drives the same
`AST → ASTᵗ → ASTᵗ′` pipeline. The CLI invokes the shared AST interpreter in
runtime mode after const evaluation completes.

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
