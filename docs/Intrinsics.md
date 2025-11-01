# Intrinsic Normalisation (AST-centric)

The removal of THIR shifts intrinsic handling to the typed AST. Every frontend
now lowers directly into the canonical AST, type inference annotates nodes in
place, and both the interpreter and backends read the same typed structures.
This document describes how intrinsic normalisation and backend materialisation
work in the new world.

Callout: keywords vs builtins
- Keywords like `const`, `quote`, and `splice` affect staging and are handled by
  the parser/const evaluator; they are not intrinsics.
- Builtin macros like `emit!` are sugar that expand to AST after parsing and are
  not intrinsics either. Intrinsics are library-like operations (e.g.,
  `sizeof!`, `hasfield!`) resolved through the intrinsic registry.

## Two-Phase Approach

1. **Symbolic AST normalisation**
   - After parsing and macro expansion, a dedicated pass rewrites
     language-specific helpers into canonical `std::…` symbols directly on the
     AST.
   - The pass operates before type inference so the inferencer sees the unified
     vocabulary. Metadata (spans, attributes) is preserved.
   - The result is a *symbolic typed AST* once inference runs: structure matches
     the source but every intrinsic reference resolves to a canonical `std::…`
     symbol (I/O, math, allocation, etc.).

2. **Backend materialisation**
   - When a backend flavour is chosen, it materialises the canonical intrinsic
     into target-specific constructs. Because HIR now remains type-aware, the
     materialiser can run either on the typed AST (for interpreter / transpile
     targets) or during HIR projection / MIR lowering (for native backends).
   - The same resolver table drives every flavour—LLVM, runtime interpreter,
     transpilers, etc.—so mappings live in one place.

## Resolver Model

The shared data model lives in `crates/fp-core/src/intrinsics`. Going forward we
intend to split the catalogue into two registries:

1. **Normalisation tables** authored by the language frontends (e.g. `fp-rust`)
   that describe how surface constructs lower into canonical `std::…` symbols.
2. **Materialisation tables** owned by backend-facing crates (LLVM, transpilers,
   interpreter) that describe how each symbol is implemented for a given target.

`fp_core` will continue to host the canonical structs (`IntrinsicSpec`,
`ResolvedCall`, `ResolvedIntrinsic`, etc.) so both registries can share them
without duplicating definitions. Today each backend wires its own lookup tables
(for example `crates/fp-llvm/src/intrinsics.rs`) that return one of the shared
`ResolvedIntrinsic` variants:

```rust
pub enum ResolvedIntrinsic {
    Call(ResolvedCall),
    InlineEmitter,
    Unsupported(String),
}
```

Key ideas:

- **Canonical vocabulary**: everything is registered under a descriptive
  `std::…` symbol so lookups remain stable across frontends. When the registries
  move into language-/backend-specific crates they will still reference these
  shared symbols.
- **Per-backend behaviour**: each entry records const legality, runtime-only
  status, argument expectations, and the backend-specific implementation.
- **Identity friendly**: interpreters can execute many intrinsics directly
  (either via inline handlers or a `ResolvedCall`), while other backends lower
  them into their target runtimes.

### Intrinsic families

- **Function-style intrinsics** &mdash; Registered in
  `crates/fp-core/src/intrinsics/catalog.rs`. The catalogue currently covers
  three core modules:
  - `std::io` (print/println/eprintln family)
  - `std::alloc` (alloc/dealloc/realloc)
  - `std::math` (f64/f32 trig, pow, sqrt, log, exp)

  Additional helpers such as `std::str::len`, `std::str::cmp`, and
  `std::process::exit` share the same resolver mechanism and are declared next
  to the core modules in the catalogue. LLVM materialises them as `printf`,
  `malloc`, libm calls, `strlen`, etc., while interpreters/transpilers either
  execute inline handlers or surface an unsupported-in-runtime diagnostic.

- **Type utilities** &mdash; Intrinsics that interrogate or construct struct
  metadata. They are invoked as free functions or macros (`sizeof!`,
  `field_count!`, `hasfield!`, etc.)—not as member methods—and map to entries in
  `IntrinsicCallKind` that operate on the runtime `ValueStruct` /
  `ValueStructural` representations:

  | Symbol/method | `IntrinsicCallKind` | Notes |
  |---------------|--------------------|-------|
  | `std::type::size_of` | `SizeOf` | Const-only helper; produces an integer literal. |
  | `reflect_fields` | `ReflectFields` | Returns a reflected `ValueStructural` describing field names/types. |
  | `type_name` | `TypeName` | Produces the fully-qualified name of a struct/enum. |
  | `create_struct` | `CreateStruct` | Builds a `ValueStruct` from field assignments. |
  | `clone_struct` | `CloneStruct` | Duplicates a `ValueStruct`, preserving field ordering. |
  | `struct_size` | `StructSize` | Returns the byte size of a struct; const-only. |
  | `add_field!` macro | `AddField` | Used by metaprogramming helpers to extend struct metadata. |

  Member-style helpers are parsed as `IntrinsicCallKind` variants and dispatched
  through the struct metadata tables. Only the reflection/size helpers listed
  below are treated specially; everyday methods such as `Vec::push`,
  `Vec::pop`, or `HashMap::insert` are regular method calls and do **not** go
  through the intrinsic pipeline (they compile just like user-defined
  functions).

  | Method call | `IntrinsicCallKind` | Source |
  |-------------|--------------------|--------|
  | `.has_field(name)` | `HasField` | `crates/fp-core/src/intrinsics/calls.rs` (method-style variant) |
  | `.field_count()` | `FieldCount` | same as above |
  | `.field_type(name)` | `FieldType` | same as above |
  | `.has_method(name)` | `HasMethod` | same as above |
  | `.method_count()` | `MethodCount` | same as above |

  During const evaluation (`crates/fp-interpret/src/ast/interpreter.rs` and
  `crates/fp-interpret/src/intrinsics.rs`) these calls emit concrete
  `ValueStruct` / `ValueStructural` values. If any reach a backend, the resolver
  reports them as unsupported because no materialisations exist yet.

- **Method-style helpers** &mdash; Only a small set of member calls are treated as
  intrinsics. The Rust frontend emits `ExprInvokeTarget::Method` nodes tagged
  with the corresponding `IntrinsicCallKind`, which allows type inference
  (`crates/fp-typing/src/lib.rs`) and the interpreter
  (`crates/fp-interpret/src/ast/interpreter.rs`) to evaluate them eagerly. After
  evaluation they collapse back into ordinary method calls, so backends do not
  require special handling.

  | Method call | Applies to | `IntrinsicCallKind` | Notes |
  |-------------|------------|--------------------|-------|
  | `.len()` | Arrays, slices, `Vec<T>`, `HashMap<K, V>` | `Len` | Type inference recognises these containers; const evaluation produces an integer literal. |
  | `.to_string()` | Primitive scalars: string, integers, decimals, bool, char (compile-time only) | *(handled via `try_infer_primitive_method`)* | Converts the value to a `Value::String` during const evaluation; no backend lowering. |

### Function-style intrinsics

The bulk of function-style intrinsics live in `crates/fp-core/src/intrinsics/catalog.rs`.
Each entry enumerates a canonical symbol (`std::io::print`, `std::math::f64::sin`,
`std::alloc::realloc`, `std::type::size_of`, etc.), optional aliases, and per-backend
behaviour. LLVM backends resolve to `ResolvedCall` specs (e.g. `printf`, `malloc`,
`sin`), while interpreters/transpilers either execute inline handlers or report
unsupported diagnostics. Because these are ordinary function calls in the AST, no
additional `IntrinsicCallKind` tagging is required—the resolver performs all
materialisation.

### Method-style intrinsics

Helpers such as `.len()`, `.has_field()`, `.field_type()`, and `.method_count()`
are expressed as `IntrinsicCallKind` variants in
`crates/fp-core/src/intrinsics/calls.rs`. The Rust frontend translates surface
syntax (e.g. `vec.len()`) into `ExprInvokeTarget::Method` nodes. The type
inferencer (`crates/fp-typing/src/lib.rs:1425-1450`) and interpreter
(`crates/fp-interpret/src/ast/interpreter.rs:579-618`, `1974-1999`) inspect those
variants to enforce compile-time restrictions and compute results. Backends see
regular method calls because the intrinsic logic reduces them to constants or
standard helper invocations before code generation.


## Pipeline Integration

1. Frontends annotate their AST with enough information for the normaliser to
   recognise intrinsic forms (macro names, language-specific helpers).
2. The **AST normaliser** rewrites everything into canonical symbols before type
   inference.
3. Type inference annotates the canonical AST.
4. The interpreter executes the typed AST using the resolver's identity flavour
   (intrinsics map to interpreter evaluators or runtime closures).
5. Backend materialisers invoke the resolver for their flavour and produce
   backend-specific nodes during projection/lowering.

Because everyone consults the same resolver, adding a new intrinsic involves:

1. Registering the canonical symbol and metadata.
2. Providing materialisers for the desired backend flavours.
3. (Optionally) updating the AST normaliser to detect more surface spellings.

## Container Intrinsics (`Vec`, `HashMap`, and friends)

The current pipeline treats collection literals as *compile-time* conveniences
without introducing new runtime `Value` variants. Instead, the Rust frontend
emits `ExprKind::IntrinsicContainer` nodes that are normalised away before type
inference runs.

### Frontend lowering

- `vec!` / `hashmap!` macros create an `ExprIntrinsicContainer` variant:
  - `VecElements { elements: Vec<Expr> }`
  - `VecRepeat { elem: BExpr, len: BExpr }`
  - `HashMapEntries { entries: Vec<ExprIntrinsicContainerEntry> }`
- Elements remain ordinary `Expr` nodes. There is **no** `Value::Vec` or
  `Value::HashMap`; the existing `ValueList` / `ValueMap` continues to represent
  evaluated data.

### Normalisation

- `fp-optimize::passes::normalize_intrinsics` rewrites the intrinsic container
  nodes into const blocks that invoke `Vec::from` / `HashMap::from` with array
  or tuple literals. Once this pass finishes the AST contains only canonical
  helper calls, so type inference and later passes operate on familiar shapes.

### Const evaluation & typing

- Because the desugared AST uses standard helper calls, const evaluation simply
  produces `ValueList` or `ValueMap` instances—no bespoke handling is required.
- Method support remains explicit: `len()` on lists/maps is recognised both by
  the interpreter and the type inferencer, and currently works only in the
  compile-time regime.

### Adding additional collection forms

Extending the system with new collection intrinsics follows the general recipe:

1. Teach the parser to emit an `ExprIntrinsicContainer` (or equivalent
   desugaring) for the surface syntax you want to support.
2. Update the intrinsic normaliser so the new variant rewrites to canonical
   helper calls before type inference.
3. Add interpreter / type-inference hooks only when the collection introduces
   new runtime behaviours (e.g. method families).
4. Ensure transpilers either leverage const-eval or translate the helper calls
   appropriately.

## Open Items

- Decide whether resolver entries live in data files (`serde`/`ron`) or remain in
  Rust tables.
- Establish testing fixtures that validate each backend flavour can materialise
  the intrinsic (compile to MIR/LLVM, run under the interpreter, etc.).
- Ensure diagnostics clearly report when an intrinsic lacks support for the
  requested backend.

This AST-centric approach keeps intrinsic logic in one place while allowing each
backend to emit the representation it needs without reviving THIR.
