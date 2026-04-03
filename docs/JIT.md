# JIT Design (Interpreter Mode)

## Goals

- Keep interpreter semantics as the source of truth.
- Reuse the existing AST-centric pipeline as much as possible.
- Add a low-latency in-process JIT for hot paths.
- Make the JIT optional and safe to disable at runtime.

## Non-Goals (Initial)

- Full deoptimization support with mid-frame state reconstruction.
- Aggressive speculative optimization.
- On-disk caching across runs.

## CLI Usage

Enable the JIT in interpreter mode:

```bash
fp interpret --jit path/to/main.fp
```

Override the hot call threshold:

```bash
fp interpret --jit --jit-hot-threshold 64 path/to/main.fp
```

## High-Level Approach

We add a new crate, `fp-jit`, that provides an in-process JIT backend. The JIT
compiles FerroPhase functions to native code and exposes a stable ABI for
interpreter calls. The interpreter stays in control and only transfers to
JIT code when a compiled entry is available.

The compilation path reuses the existing pipeline:

```
AST_t_prime -> HIR -> MIR -> LIR -> JIT backend -> executable code
```

Where `AST_t_prime` is the post-const-eval AST (const folds and intrinsic rewrites
already applied). This ensures the JIT sees the same semantics as the runtime
interpreter.

## Crate Layout

- `crates/fp-jit`
  - Owns JIT session state, code cache, symbol resolution, and ABI glue.
  - Depends on `fp-backend` for lowering.
  - Integrates with `fp-interpret` for call dispatch.

## Interpreter Integration

Entry points for JIT dispatch are runtime calls:

- `AstInterpreter::call_function_runtime`
- `AstInterpreter::call_value_function_runtime`

At these call sites:

1. Check JIT cache for a compiled entry.
2. If present, invoke compiled code through the JIT ABI.
3. If absent, run interpreted code and update hotness counters.
4. If hotness threshold is exceeded, enqueue JIT compilation.

The current invocation remains interpreted; compiled code is used on subsequent
calls.

## JIT ABI

The ABI must bridge interpreter `Value` and native code. The initial ABI should:

- Use a small, stable C-compatible struct layout for `Value`.
- Pass arguments as an array (or pointer + length) of `Value`.
- Return a single `Value`.

### Proposed C ABI (V0)

The JIT-exposed entry points use a minimal C ABI that allows the interpreter to
call compiled code without marshaling into a custom calling convention.

```c
typedef struct FpValue FpValue;
typedef struct FpJitContext FpJitContext;

typedef struct {
  const FpValue *args;
  uint32_t len;
} FpJitArgs;

typedef FpValue (*FpJitFn)(FpJitContext *ctx, FpJitArgs args);
```

Notes:
- `FpValue` must be ABI-stable and identical to the interpreter runtime layout.
- `FpJitContext` exposes runtime services (allocators, error reporting, string
  interning, and intrinsic helpers).
- The call returns an owned `FpValue`. The interpreter owns any cleanup rules.

### Adapter Layer

For functions with a known fixed arity, the JIT may emit a wrapper:

- Wrapper takes `FpJitArgs`, unpacks into fixed slots, and tail-calls the
  compiled function body.
- This keeps the internal compiled function signature efficient while keeping
  the external ABI stable.

## Symbol Registry

The JIT must map a runtime call target to a compiled function entry point.
We use a stable lookup key derived from the resolved function and a signature
hash.

### Key Structure

```
struct JitKey {
  canonical_name: String,  // fully-qualified path
  sig_hash: u64,           // stable hash of param/return types
  abi: u16,                // ABI version (e.g., 1)
}
```

### Registry Operations

- `lookup(JitKey) -> Option<FpJitFn>`
- `insert(JitKey, FpJitFn)`
- `invalidate(prefix | module)` for hot reload or diagnostics

### Name Canonicalization

The interpreter should register and query using the same canonicalization:

- Fully-qualified module path (e.g., `std::math::sin`).
- Impl methods: `TypeName::method`.
- Trait impls: `TraitName::TypeName::method` (if needed).

### Signature Hashing

Compute a stable hash of:

- Parameter types (in order).
- Return type.
- Receiver kind (for methods).
- Any ABI-relevant calling attributes.

Type names should be canonicalized in the same way the interpreter uses for
type comparisons to avoid hash mismatches.

If needed, a thin wrapper can be generated per function to adapt from the
interpreter calling convention to the JIT function signature.

## Symbol Resolution

Compiled functions are exposed via a symbol registry owned by `fp-jit`.
The registry maps fully-qualified function names plus a signature hash to a
JIT entry point.

The interpreter uses the same key to query the cache.

## Safety

- JIT can be disabled by option or environment flag.
- When a JIT call fails (missing symbol, ABI mismatch, or runtime fault),
  the interpreter falls back to normal execution and records a diagnostic.

## Limitations (Initial Scope)

- Only `ItemDefFunction` targets, no closures.
- No generic specialization; compile the monomorphic form the interpreter
  resolves at runtime.
- No inlining or speculative optimizations.

## Future Work

- Add a deoptimization protocol and OSR for hot loops.
- Support closures and capturing environments.
- Add profile-guided inline caches for dynamic calls.
- Persist compiled code across runs.
