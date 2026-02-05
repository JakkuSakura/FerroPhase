# Domain-Driven Design (DDD) Lens for FerroPhase

> FerroPhase is a compiler/toolchain. The “domain” here is compilation itself.
> DDD helps us make the technical domain explicit, stable, and shared across docs, code, and tests.

## 1) Ubiquitous Language (Technical Domain)

These terms should be used consistently across docs, code, and diagnostics:

- **Stage**: A named step in the compilation pipeline (Frontend → Normalization → Typing → Eval → Lowering).
- **Snapshot**: A persisted or cached artifact produced by a stage (LAST, AST, ASTᵗ, HIRᵗ, MIR, LIR).
- **Intrinsic**: A compiler-recognized capability resolved via the intrinsic registry; may lower to runtime or be executed at const time.
- **Module**: The canonical unit of namespacing and compilation, rooted at a package path.
- **Package**: A dependency boundary that provides modules and optional bindings.
- **Query Document**: A query frontend artifact (SQL/PRQL) normalized into the AST.
- **Pipeline**: The orchestrator that threads artifacts and diagnostics through stages.

## 2) Bounded Contexts (Compiler as Domain)

Each context has clear responsibilities and owns its invariants.

- **Frontend Context**
  - Owns parsing, LAST capture, serializer selection, and language detection.
  - Outputs: LAST + canonical AST + FrontendSnapshot.

- **Typing Context**
  - Owns Algorithm W inference and type annotation of AST nodes.
  - Outputs: ASTᵗ (typed AST).

- **Interpretation / ConstEval Context**
  - Owns evaluation (const/runtime) and intrinsic execution at AST level.
  - Outputs: ASTᵗ′ (evaluated AST) + Diagnostics.

- **Lowering / Backend Context**
  - Owns ASTᵗ′ → HIRᵗ → MIR → LIR → Backend IR (LLVM/VM/Transpilers).
  - Outputs: Backend artifacts + backend diagnostics.

- **Tooling / CLI Context**
  - Owns orchestration, flags, logging, and user-facing diagnostics.
  - Does not own domain rules; it should call into domain contexts.

## 3) Aggregates & Invariants

- **Pipeline (Aggregate)**
  - Invariants: stage ordering is monotonic; diagnostics are preserved; snapshots are immutable once persisted.

- **Module (Aggregate)**
  - Invariants: module paths resolve deterministically; exports are stable for bindings.

- **Package (Aggregate)**
  - Invariants: dependency graph is valid; bindings respect declared targets.

- **Query Document (Aggregate)**
  - Invariants: input is normalized into AST with dialect metadata preserved.

## 4) Domain Events (Internal)

These are internal events useful for logging, tests, and boundary enforcement:

- `FrontendParsed`
- `AstNormalized`
- `AstTyped`
- `AstEvaluated`
- `HirGenerated`
- `MirGenerated`
- `LirGenerated`
- `BackendEmitted`
- `DiagnosticEmitted`

Treat them as checkpoints: each event should only occur once per pipeline run.

## 5) Context Map (Integration)

```
Frontend ──► Typing ──► Interpretation ──► Lowering/Backend
   ▲                                      │
   └────────────── Tooling / CLI ─────────┘
```

- Tooling is the host/adapter; it should not contain domain logic.
- Lowering should not reach into frontend structures except via typed AST.

## 6) How This Guides Refactors

1. **Boundary enforcement**: move utilities to the context that owns them.
2. **Encapsulation**: make stage transitions explicit (e.g. constructor checks).
3. **Diagnostics**: attach context + stage info so errors are traceable to the domain.
4. **Tests**: add invariant tests per aggregate (pipeline ordering, module resolution).

## 7) Next Steps

- Add a glossary section to key docs (Design.md, Pipeline.md) referencing this file.
- Introduce small boundary checks in constructors for Pipeline/Module/Package.
- Add tests that assert invariants (stage ordering, module path resolution).
