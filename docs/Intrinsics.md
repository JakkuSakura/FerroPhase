# Intrinsics And Operations

FerroPhase has two related call vocabularies. The distinction is deliberate:
high-level operations are useful to source transpilers, while compiler
intrinsics describe capabilities that must be understood by the compile
pipeline.

## `#[op]`

An operation is a high-level standard-library-facing call:

```ferro
#[op(func = "fs_read_to_string")]
pub fn read_to_string(path: &Path) -> str {
    std::intrinsics::fs::read_to_string(path)
}
```

In transpile mode, the operation remains an `Op` call in the normalized AST.
This preserves a useful source-level abstraction for target printers and lets
each target provide its own implementation or wrapper.

In compile mode, the Ferro frontend maps supported operations to their ordinary
`std` wrapper paths, for example `fs_read_to_string` becomes
`std::fs::read_to_string`. The backend then sees a regular function call rather
than a frontend-only operation.

## Three-language operation design

Operations are declared consistently in the three language layers:

1. `fp-rust/std` annotates the real Rust standard-library declaration with
   `#[op(...)]`.
2. `fp-lang/std` annotates the FerroPhase-facing wrapper with the same
   canonical operation name.
3. `fp-kotlin/std` (and other target std packages) annotates the target-side
   declaration with corresponding `@Op(...)` metadata.

The compiler collects these declarations into source and target operation
registries. HIR lowering carries the canonical operation identity forward;
the target materializer performs the language-specific transformation. The
std declarations remain ordinary implementations and do not need new
intrinsic declarations just to participate in operation lowering.

`func` identifies a free function. `class` marks an implementation's receiver
type, and `method` identifies a receiver operation. Operation names must be
identical across participating std packages; do not create layer-specific
aliases.

## `#[intrinsic]`

An intrinsic is a compiler-pipeline primitive:

```ferro
#[intrinsic = "create_struct"]
pub fn create_struct(name: &str) -> Type;
```

Intrinsic declarations belong in `std::intrinsics::*` where possible. They are
recognized by the shared AST and represented as `IntrinsicCallKind` values.
They are not general-purpose high-level APIs and should not be added to
ordinary `std` functions merely to make a backend recognize a call.

Use an ordinary `std` wrapper when a public API can express the operation. The
wrapper may call a low-level intrinsic, just as Rust standard-library wrappers
delegate to compiler primitives.

## Normalization Modes

The frontend receives an `IntrinsicNormalizationMode`:

| Mode | `Op` calls | `Intrinsic` calls |
| --- | --- | --- |
| Transpile | Preserve high-level operation | Preserve compiler primitive identity |
| Compile | Lower supported operations to `std` wrappers | Keep for compiler lowering |

Normalization is performed before HIR lowering. It does not change the
semantic contract; it changes which representation is most useful to the next
stage.

## Canonical Representation

The shared AST represents calls as:

```text
CallKind::Op(OpKind)
CallKind::Intrinsic(IntrinsicKind)
```

`OpKind` includes printing, filesystem, environment, I/O, process, and task
operations. `IntrinsicKind` includes type/layout queries, structural type
construction, panic/control primitives, and low-level compiler hooks.

The canonical low-level standard-library namespace is
`std::intrinsics::*`. The C ABI declarations are separate, generated under the
top-level `::libc` package; `std::libc` is retired.

## Resolution Flow

1. A `LanguageFrontend` parses source into the shared AST.
2. The frontend normalizes macros, operations, and intrinsic declarations for
   the selected mode.
3. Package and module resolution obtains provider-owned package identities.
4. HIR lowering and typing consume the normalized AST.
5. MIR/LIR lowering, interpretation, target emission, or AST printing consumes
   the same semantic call identity.

Unsupported intrinsic behavior must produce a capability or typing diagnostic.
It must not silently fall back to unrelated interpreter-only behavior.

## Adding A New Capability

1. Decide whether the public surface is an `#[op]`, an `#[intrinsic]`, or an
   ordinary wrapper.
2. Put compiler primitives in the appropriate `std::intrinsics::*` module.
3. Add the operation or intrinsic kind to `fp-core` only when shared compiler
   handling is required.
4. Teach the Ferro normalizer about compile-mode lowering if the operation has
   a `std` wrapper.
5. Add tests for transpile preservation, compile-mode normalization, typing,
   and supported runtime/backends.
