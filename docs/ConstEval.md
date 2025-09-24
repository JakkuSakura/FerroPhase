# FerroPhase Const Evaluation

## Overview

FerroPhase const evaluation enables compile-time computation and metaprogramming through:
- **AST-based interpretation**: Direct evaluation of const expressions in AST form
- **Type system integration**: Clean separation with established type foundation
- **3-phase compilation**: Basic types → const evaluation → final validation

```
Source Code → Parse & Basic Types → Const Evaluation → Final Types → Target Code
```

**Key Benefits:**
- Clean, predictable compilation phases
- No complex iterative loops or convergence issues  
- Type-safe metaprogramming with compile-time validation
- Rich introspection capabilities via `@sizeof`, `@hasfield`, etc.

## Const Block Semantics

### Basic Const Blocks

Const blocks provide controlled compile-time evaluation environments:

```rust
const RESULT: i32 = {
    let x = 10;
    let y = 20;
    x + y  // Returns 30 at compile time
};
```

**Key Properties:**
- **Compile-time execution**: All computation happens during compilation
- **Pure evaluation**: No runtime effects; const blocks may schedule structural transformations (type/AST edits) that are applied after evaluation.
- **Type system access**: Can query types via intrinsics like `@sizeof`, `@hasfield`
- **Scoped variables**: Local variables exist only within the const block
- **Result value**: Final expression becomes the const value

### Advanced Const Blocks

#### Conditional Compilation
```rust
const DEBUG_CONFIG: Config = {
    let mut config = Config::new();
    
    if cfg!(debug_assertions) {
        config.log_level = LogLevel::Debug;
        config.enable_tracing = true;
    } else {
        config.log_level = LogLevel::Error;
        config.enable_tracing = false;
    }
    
    config
};
```

#### Type Introspection
```rust
const STRUCT_INFO: StructInfo = {
    let size = sizeof!(Point);
    let field_count = field_count!(Point);
    let has_z = hasfield!(Point, "z");
    
    StructInfo {
        size,
        field_count,
        has_z_coordinate: has_z,
    }
};
```

#### Metaprogramming with Transformations
```rust
const GENERATED_STRUCT: Type = {
    let mut builder = create_struct!("GeneratedPoint");
    
    addfield!(builder, "x", f64);
    addfield!(builder, "y", f64);
    
    if ENABLE_3D {
        addfield!(builder, "z", f64);
    }
    
    builder  // Returns type reference
};
```

### Const Block Evaluation Rules

1. **Evaluation Order**: Const blocks are evaluated in dependency order
2. **Dependency Tracking**: Each const block's dependencies are analyzed
3. **Circular Dependencies**: Detected and reported as compile errors
4. **Pure Functions**: Only pure functions can be called (no I/O, no mutation of global state)
5. **Type Queries**: Intrinsics like `sizeof!` query the current type system state
6. **Transformation Capture**: Metaprogramming operations record structural transformations for later application

### Const Block vs Regular Code

| Aspect | Const Block | Regular Code |
|--------|-------------|-------------|
| **Execution Time** | Compile time | Runtime |
| **Transformations** | Metaprogramming-only structural edits | Full runtime effects |
| **Function Calls** | Pure functions only | All functions |
| **Variable Scope** | Block-scoped | Function/module scoped |
| **Type Access** | Via intrinsics | Runtime reflection |
| **Error Handling** | Compile errors | Runtime errors |

### Metaprogramming Transformations

Const blocks can generate code through scheduled transformations:

```rust
const CONTAINER_TYPE: Type = {
    let mut container = create_struct!("Container");
    
    // Transformation: adds field to struct definition
    addfield!(container, "data", Vec<T>);
    addfield!(container, "len", usize);
    
    // Transformation: adds method to struct
    addmethod!(container, "push", |&mut self, item: T| {
        self.data.push(item);
        self.len += 1;
    });
    
    container
};
```

**Transformation Actions:**
- `addfield!`: Add field to struct
- `addmethod!`: Add method to type
- `addimpl!`: Add trait implementation
- `compile_error!`: Trigger compilation error
- `compile_warning!`: Emit compilation warning

### Error Handling in Const Blocks

```rust
const VALIDATED_CONFIG: Type = {
    let mut config = create_struct!("Config");
    addfield!(config, "buffer_size", usize);
    
    // Compile-time validation
    if sizeof!(config) > MAX_STRUCT_SIZE {
        compile_error!("Config struct exceeds maximum size");
    }
    
    config
};
```

## Implementation Architecture

### 3-Phase Evaluation System

This simplified approach eliminates complex iterative feedback loops:

```
Phase 1: Basic Type Checking
    ↓
Phase 2: Const Evaluation & Metaprogramming  
    ↓
Phase 3: Final Type Checking
```

#### Phase 1: Basic Type Checking
- **Parse AST** and build initial type registry
- **Type check non-const code** (functions, structs, regular expressions)
- **Validate basic type references** and struct definitions
- **Establish baseline type system** that const evaluation can query
- **Skip const blocks** - treat them as opaque for now

#### Phase 2: Const Evaluation & Metaprogramming
- **Discover const blocks** and build dependency graph
- **Evaluate const expressions** in topological order
- **Execute intrinsics** like `sizeof!`, `create_struct!`, `addfield!`
- **Record transformations** (generated fields, methods, new types)
- **Apply recorded transformations** to the AST snapshot
- **Query established types** from Phase 1 as needed

#### Phase 3: Final Type Checking  
- **Type check generated code** (new fields, methods, types)
- **Validate all type references** including generated ones
- **Check const block results** against expected types
- **Ensure type system consistency** after metaprogramming
- **Generate final optimized AST**

### Type Query Service

Const evaluation relies on a dedicated type-query layer rather than ad-hoc lookups:

- **Snapshot Inputs**: Phase 1 produces a read-only `TypeSnapshot` that the evaluator consults. It exposes canonical
  handles into a shared `TypeArena` used by later lowering passes.
- **Comptime `mut type` handles**: When transformations need new types (e.g., `create_struct!`), the query engine
  allocates provisional handles that mirror the language-level `mut type` construct (`let mut T = struct Name { ... };`).
  Each handle stores its syntactic shape as an `AstType` plus the data needed to emit a `ConcreteType` record later. They
  live only during Phase 2, carry explicit provenance, and are immutable once committed.
- **Memoised Queries**: Intrinsics such as `@sizeof`/`@hasfield` route through a `TypeQueryEngine` that caches results by
  handle + parameters, avoiding redundant computation across const blocks. Cache entries are keyed by `(query, type_id,
  mut_revision)` so they automatically stay valid until a new `mut type` handle commits.
- **Deterministic Commits**: When a const block finishes, all `mut type` handles it introduced are atomically promoted to
  full `ConcreteType` entries in the shared arena (or discarded on failure). No in-place mutation occurs on previously
  materialised types.
- **Diagnostics**: Failed queries emit structured diagnostics that Phase 3 can surface alongside traditional type errors.

When Phase 2 completes for a compilation unit, all committed `mut type` handles are frozen into concrete definitions and
the updated AST snapshot is tagged as **EAST** (Evaluated AST). This EAST becomes the hand-off point for downstream
stages (surface transpile, HIR lowering, etc.), guaranteeing they all consume identical, evaluation-stable source
structure.

### Benefits of 3-Phase Approach

**Simplicity:**
- No complex iterative loops or convergence logic
- Clear separation of concerns between phases  
- Predictable execution order

**Correctness:**
- Phase 1 establishes stable foundation for const evaluation
- Phase 2 can safely query types without affecting type checking
- Phase 3 validates everything including generated code

**Performance:**
- Each phase runs exactly once
- No repeated type checking or re-evaluation
- Clear caching boundaries between phases

**Debuggability:**
- Easy to inspect state between phases
- Clear failure points if issues occur
- Straightforward error attribution

**Cross-Mode/Stage Consistency:**
- EAST becomes the canonical source for every downstream stage and backend.
- Compile, bytecode, transpile, and interpret pipelines observe identical evaluated structure, differing only in
  execution cost or emission format.

### Example: 3-Phase Execution

Consider this FerroPhase code:

```rust
struct Point {
    x: i64,
    y: i64,
}

const POINT_SIZE: usize = sizeof!(Point);

const EXTENDED_POINT: Type = {
    let mut builder = create_struct!("ExtendedPoint");
    addfield!(builder, "x", i64);
    addfield!(builder, "y", i64);
    addfield!(builder, "z", i64);
    builder
};

fn process_point(p: Point) -> ExtendedPoint {
    ExtendedPoint { x: p.x, y: p.y, z: 0 }
}
```

**Phase 1: Basic Type Checking**
- ✅ Type check `struct Point { x: i64, y: i64 }`
- ✅ Type check `fn process_point(p: Point) -> ExtendedPoint` (ExtendedPoint unknown, skip for now)
- ✅ Register `Point` in type registry with size calculation
- ⏸️ Skip const blocks `POINT_SIZE` and `EXTENDED_POINT` (treat as opaque)

**Phase 2: Const Evaluation**
- 🔍 Discover const blocks: `POINT_SIZE`, `EXTENDED_POINT`
- 📊 Build dependency graph: `POINT_SIZE` has no dependencies, `EXTENDED_POINT` has no dependencies
- ⚡ Evaluate `POINT_SIZE = sizeof!(Point)` → queries type registry → returns 16
- ⚡ Evaluate `EXTENDED_POINT` const block:
  - Execute `create_struct!("ExtendedPoint")`
  - Execute `addfield!(builder, "x", i64)`, `addfield!(builder, "y", i64)`, `addfield!(builder, "z", i64)`
- Collect transformations: create new struct type `ExtendedPoint`
- 🔧 Apply transformations: add `ExtendedPoint` struct to AST

**Phase 3: Final Type Checking**
- ✅ Type check generated `ExtendedPoint` struct
- ✅ Re-check `fn process_point(p: Point) -> ExtendedPoint` with now-known `ExtendedPoint`
- ✅ Validate const results: `POINT_SIZE: usize = 16` ✓
- ✅ Ensure all references resolve correctly

**Result:** Clean, predictable execution without complex feedback loops!

## Core Intrinsics

### Type Introspection
- `sizeof!(Type)`: Get size in bytes
- `field_count!(Type)`: Number of fields in struct
- `hasfield!(Type, "name")`: Check if field exists
- `reflect_fields!(Type)`: Get field metadata array

### Struct Creation
- `create_struct!("Name")`: Create new struct type
- `clone_struct!(Type)`: Clone existing struct
- `addfield!(struct, "name", Type)`: Add field to struct

### Code Generation
- `addmethod!(struct, "name", body)`: Add method to struct
- `compile_error!("message")`: Trigger compile error
- `compile_warning!("message")`: Emit compile warning

## Example Syntax: t! Macro

### Current Implementation Helper

The `t!` macro in FerroPhase examples is currently used as a **parsing helper** to demonstrate future metaprogramming capabilities. It serves as a placeholder syntax while the full const evaluation system is being implemented.

```rust
// Current helper syntax for examples
t! {
    struct Point {
        x: f64,
        y: f64,
        
        fn distance(&self, other: &Point) -> f64 {
            let dx = self.x - other.x;
            let dy = self.y - other.y;
            (dx * dx + dy * dy).sqrt()
        }
    }
}
```

**Important Notes:**
- The `t!` macro is **only for parsing demonstration** in examples
- It helps visualize how struct+method generation will work
- The actual implementation will use const blocks and intrinsics (shown above)
- Examples use `t!` to show the target functionality while the compiler develops

### Future Implementation

The final FerroPhase implementation will replace `t!` usage with proper const evaluation:

```rust
// Future: Real const evaluation syntax
const POINT_TYPE: Type = {
    let mut point = @create_struct("Point");
    @addfield(point, "x", f64);
    @addfield(point, "y", f64);
    
    @addmethod(point, "distance", |&self, other: &Point| -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx * dx + dy * dy).sqrt()
    });
    
    point
};
```

### Migration Path

1. **Current**: `t!` macro for examples and demonstrations
2. **Phase 1**: Add @ symbol parsing and basic intrinsics
3. **Phase 2**: Implement transformation tracking and application
4. **Phase 3**: Replace `t!` usage with proper const evaluation blocks
5. **Final**: Remove `t!` macro entirely, use pure const evaluation

## Implementation Status

**Next Priority Tasks:**
1. Add transformation tracking system
2. Implement 3-phase const evaluation system
3. Replace t! macro usage with proper const evaluation
4. Implement intrinsic macros (sizeof!, hasfield!, etc.)

**Current Capabilities:**
- ✅ Basic const evaluation with existing AST interpreter
- ✅ Intrinsic function registration system
- ✅ Comprehensive test framework
- ✅ Example programs demonstrating const evaluation
- ✅ t! macro as parsing helper for examples
- ❌ Intrinsic macro implementations
- ❌ Side effect collection and application
- ❌ 3-phase evaluation system
