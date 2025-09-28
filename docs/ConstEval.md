# FerroPhase Const Evaluation

## Overview

Const evaluation in FerroPhase now operates on the typed intermediate representation (THIR). Frontends produce annotated
ASTs, desugar to HIR, and run Algorithm W inference to build THIR before any compile-time execution takes place. Typed
interpretation evaluates const blocks and produces an updated THIR′ snapshot.

Const evaluation itself stops at THIR′. Resugaring or re-projection for particular backends happens in later pipeline phases.

```
Source → LAST → Annotated AST → HIR → THIR → Typed Interpretation (ConstEval) → THIR′
```

**Benefits**

- Typed evaluation: const/runtime interpretation sees principal types for every expression.
- Unified interpreter: const and runtime execution share the same traversal engine (configuration still in progress).

## Const Block Semantics

Const blocks remain declarative compile-time expressions. Evaluation happens in a typed environment, so values, type
queries, and structural edits all observe the THIR schema the rest of the pipeline uses.

```rust
const RESULT: i32 = {
    let x = 10;
    let y = 20;
    x + y
};
```

**Properties**

- Executed at compile time using the typed interpreter.
- Pure evaluation: no runtime side effects; structural edits are visible in the resulting THIR′ snapshot.
- Access to type intrinsics (`sizeof!`, `field_count!`, `hasfield!`, etc.).
- Local scope confined to the block; the final expression yields the const value.

### Advanced Usage

**Conditional configuration**

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

**Typed introspection**

```rust
const STRUCT_INFO: StructInfo = {
    StructInfo {
        size: sizeof!(Point),
        field_count: field_count!(Point),
        has_z_coordinate: hasfield!(Point, "z"),
    }
};
```

**Metaprogramming**

```rust
const GENERATED_STRUCT: Type = {
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

## Evaluation Rules

1. Const blocks execute in dependency order determined from THIR.
2. Circular dependencies are detected and diagnosed before evaluation starts.
3. Only pure functions participate; effectful calls must be encapsulated in runtime hooks.
4. Intrinsics query the shared type tables captured during HIR→THIR inference.
5. Structural edits (`struct`/`impl` declarations, builder intrinsics) are reflected directly in the resulting THIR′ snapshot.
   directly.

## Const Blocks vs Regular Code

| Aspect             | Const Block (typed)              | Regular Code                  |
|--------------------|----------------------------------|-------------------------------|
| Execution time     | Compile time                     | Runtime                       |
| Allowed functions  | Pure / const-compatible          | All                           |
| Structural edits   | Reflected in THIR′ snapshot     | N/A                           |
| Scope              | Block-local                      | Module/function               |
| Type information   | Resolved THIR types              | Runtime reflection (if any)   |
| Failure handling   | Compile-time diagnostics         | Runtime errors/exceptions     |

## Typed Effects

The log replays during subsequent pipeline phases so every downstream stage observes the same evaluated programme.

## Three-Phase Typed Workflow

The historical three-phase structure still applies, now expressed in typed terms:

1. **Phase 1 – Typed foundation**
   - Parse and annotate AST.
   - Lower to HIR and run Algorithm W inference to build THIR.
   - Provide a `TypeSnapshot` for queries.

2. **Phase 2 – Typed interpretation**
   - Analyse const-block dependencies on THIR.
   - Interpret blocks and capture evaluated values.
   - Commit `mut type` tokens by promoting them to `ConcreteType` entries.

3. **Phase 3 – Downstream validation**
   - Outside the const-eval step, later passes replay typed effects, re-run checks, and prepare the program for specific backends or transpilers.

## Example Walkthrough

```
Phase 1: build THIR, resolve Point, Config, etc.
Phase 2: evaluate const blocks, compute sizeof!(Point), append GeneratedPoint fields, emit diagnostics if needed.
Phase 3: downstream passes verify generated definitions and prepare outputs for compilation or transpilation (outside the const-eval step).
```

The updated THIR′ snapshot is persisted when `--save-intermediates` is enabled (e.g., `target/ethir/...`). Later phases may emit additional artefacts (`.tast`, `.hir`, etc.) depending on the selected mode.

## Summary

- Const evaluation operates on THIR with full type information produced by the HIR → THIR Algorithm W inference stage.

```
Phase 1: build THIR, resolve Point, Config, etc.
Phase 2: evaluate const blocks, compute sizeof!(Point), append GeneratedPoint fields, emit diagnostics if needed.
Phase 3: downstream passes verify generated definitions and prepare outputs for compilation or transpilation (outside the const-eval step).
```

When `--save-intermediates` is enabled, const evaluation persists the updated THIR snapshot as `.ethir`. Later phases may produce additional artefacts as needed.

## Summary

- Const evaluation operates on THIR with full type information.
