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
- Intrinsics route through the shared registry and can query the
  `TypeQueryEngine` without rebuilding THIR.
- Structural edits (new structs, impls, generated functions) update the AST and
  promote any provisional `mut type` tokens to concrete entries.

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
