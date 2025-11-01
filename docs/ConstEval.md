# FerroPhase Const Evaluation (AST)

Const evaluation now operates directly on the typed AST. The interpreter runs in
const mode over the same structures used by runtime execution and backend
lowerings.

```
Source → LAST → AST → ASTᵗ → (Const Evaluation) → ASTᵗ′
```

- `ASTᵗ` is the canonical AST annotated with types by the Algorithm W inferencer.
- Const evaluation mutates the AST when necessary (constant folding, generated
  declarations, intrinsic rewrites) and returns `ASTᵗ′`.
- `ASTᵗ′` is the authoritative programme for all downstream stages (HIRᵗ, MIR,
  LIR, transpilers).

## Const Block Semantics

```rust
const RESULT: i32 = {
    let x = 10;
    let y = 20;
    x + y
};
```

- Blocks execute during compilation using the AST interpreter in const mode.
- Evaluation observes principal types (`expr.ty`) inferred earlier.
- Intrinsics route through the shared registry and consult the
  `TypeQueryEngine` without synthesising intermediate representations.
- Structural edits (new structs, impls, generated functions) update the AST and
  promote any provisional `mut type` tokens to concrete entries.

### Compile-time Generation (overview)

Const evaluation may synthesize runtime code based on compile-time data. The
language provides dedicated keywords for quoting and splicing code, and a
convenience macro for emission. See `docs/Quoting.md` for full syntax and
semantics of `quote`, `splice`, and `emit!`.

### Advanced Patterns

```rust
const DEBUG_CONFIG: Config = {
    let mut cfg = Config::new();

    if cfg!(debug_assertions) {
        cfg.log_level = LogLevel::Debug;
        cfg.enable_tracing = true;
    }

    cfg
};
```

```rust
const STRUCT_INFO: StructInfo = {
    StructInfo {
        size: sizeof!(Point),
        field_count: field_count!(Point),
        has_z: hasfield!(Point, "z"),
    }
};
```

```rust
const GENERATED: Type = {
    struct GeneratedPoint { x: f64, y: f64 }

    if ENABLE_3D {
        struct GeneratedPoint { z: f64 }
    }

    impl GeneratedPoint {
        fn magnitude(&self) -> f64 {
            (self.x * self.x + self.y * self.y).sqrt()
        }
    }

    GeneratedPoint
};
```

## Evaluation Pipeline

1. **Type foundation**
   - Parse, normalise, and run Algorithm W inference to annotate the AST.
   - Capture a `TypeSnapshot` for queries (`sizeof`, `hasfield`, etc.).

2. **Const execution**
   - Build dependency order from the typed AST (const items, const blocks,
     generated symbols).
   - Execute blocks using the AST interpreter in const mode.
   - Promote `mut type` tokens and persist structural edits.

3. **Commit**
   - Merge evaluated values back into the AST.
   - Emit diagnostics using the shared `DiagnosticManager`.
   - Persist `.ast-eval` artefacts when `--save-intermediates` is enabled.

## Emission and Quoting

For emission and quoting/splicing semantics (context, captures, control flow,
diagnostics), see `docs/Quoting.md`. Const evaluation executes those operations
and commits the resulting `ASTᵗ′` but does not redefine their behaviour here.

## Quoting and Splicing (pointer)

- `quote` captures code as a hygienic AST value (compile-time only).
- `splice` inserts a previously quoted fragment into the current AST.
- Both contribute to `ASTᵗ′` during const evaluation.

Full details and examples: `docs/Quoting.md`.

Use `emit!` for simple “write this block into the runtime body.” Use
`quote`/`splice` when you need to treat code as data (store, transform, compose
fragments) before insertion. See `docs/Quoting.md` for full semantics.

## Determinism and Staging

- All const expressions and structural edits must be acyclic. The interpreter
  evaluates const items/blocks in a topologically sorted order derived from
  their dependencies (names and types they reference).
- An emission or splice cannot reference results from a later stage; such
  cross‑stage references produce a compile‑time diagnostic.
- Repeated evaluation with identical inputs yields identical `ASTᵗ′` snapshots.

## Diagnostics

- Cycle or cross‑stage reference during const evaluation → error with a cycle
  report.
- Illegal side effects or capability violations during const execution →
  compile‑time diagnostics from the interpreter.

## Comparison

| Aspect             | Const Block (ASTᵗ)               | Regular Code                  |
|--------------------|----------------------------------|-------------------------------|
| Execution time     | Compile time                     | Runtime                       |
| Allowed functions  | Pure / const-compatible          | All                           |
| Structural edits   | Written into ASTᵗ′               | N/A                           |
| Scope              | Block-local                      | Module/function               |
| Type information   | Inferred AST annotations          | Runtime reflection (if any)   |
| Failure handling   | Compile-time diagnostics         | Runtime errors/exceptions     |

## Artefacts

With `--save-intermediates`, const evaluation writes the post-eval snapshot as
`*.ast-eval`. Downstream tooling will consume this snapshot directly once the
new typed HIR/MIR pipeline is reinstated.

## Summary

- Const evaluation and runtime interpretation share the same AST interpreter with
  different configuration flags.
- The typed AST is the single source of truth; THIR is no longer produced.
- Intrinsic handling and type queries operate through shared resolver/query
  infrastructure.
