# Glossary

This glossary defines the shared domain language for FerroPhase. Use these
terms consistently in docs, code, and diagnostics.

- **CompilerWorkScheduler** - The component that orders compiler work, records
  dependencies, and delivers request answers. It may use a stack internally.
- **Work item** - A scheduled piece of compiler work, such as parse, type,
  lower, execute, emit, or revalidate.
- **Work subject** - The AST node, item, function, block, expression, generated
  fragment, or resolved path that a work item is currently about. A work
  subject is not a "code unit"; it may start as an AST position and only later
  gain a resolved path.
- **Request** - A need for an artefact or answer that can block other work.
- **RequestId** - Stable identity for a pending request before it is answered.
  Once identity-forming needs are answered, the semantic identity is expressed
  by `FullyQualifiedPath`.
- **FullyQualifiedPath** - Resolved identity for a concrete item, function,
  block, or specialization. It includes resolved generic and comptime arguments
  when those affect identity. It names the resolved work subject; it does not
  imply a separate code-unit abstraction.
- **Request answer** - The value, type, declaration, AST update, lowered
  artefact, or emitted output that satisfies a request.
- **CompileTimeNeed** - A value, type, declaration, code fragment, or
  identity-forming argument required before current work can continue.
- **TypeNeed** - A typing blocker such as an unknown or unresolved type. If it
  depends on comptime, answer-producing work still runs through typed AST, HIR,
  MIR, LIR, and execution before typing retries.
- **GenericWork** - Scheduler work discovered by a completed typing pass for a
  generic-bearing item, declaration, trait, impl, or type whose resolved path
  must be tracked.
- **Typed storage id** - Representation-specific identity for stored compiler
  data, such as `AstId`, `TypedAstId`, `HirId`, `MirId`, `LirId`,
  `ConstValueId`, `BytecodeId`, or `NativeObjectId`. These ids are storage
  handles, not specialization identity.
- **Artefact** - Persisted or emitted compiler data such as bytecode, object
  code, saved IR, or target AST output. Public build outputs should use
  artefact/output terminology; in-memory scheduler edges should use typed
  storage ids.
- **Frontend** - The parsing and normalization boundary that produces canonical
  AST plus provenance.
- **Typing** - Type inference and type query work that annotates canonical AST
  nodes and can emit `CompileTimeNeed`.
- **Comptime** - Compile-time execution through scheduler requests and shared
  scoped lowering/execution services.
- **Interpretation** - A compiler mode that requests executable artefacts and
  values. It is not a separate AST-only semantics.
- **Scoped lowering** - The typed AST -> HIR -> MIR -> LIR refinement for a
  resolved work subject such as a function, item, block, const body, generated
  fragment, or resolved specialization path.
- **Intrinsic** - A compiler-recognized capability resolved once and consumed by
  typing, execution, and target emission.
- **Module** - The canonical unit of namespacing and compilation, rooted at a
  package path.
- **Package** - A dependency boundary that provides modules and optional
  bindings.
- **Query Document** - A query frontend artefact, such as SQL or PRQL,
  normalized into AST `NodeKind::Query`.
- **Binding** - A generated language-specific facade that mirrors module paths
  and public APIs.
- **Diagnostic** - Structured error, warning, or info emitted by compiler work
  with request identity, context, and span when available.
- **Semantic Contract** - The project-wide declaration of observable semantics
  that must remain consistent across frontends, IRs, and modes.
- **Semantic Point** - A single, testable behavior listed in the semantic
  matrix.
- **Semantic Mapping Matrix** - The matrix that enumerates semantic points,
  frontends, IRs, modes, guarantees, and evidence.
- **Cross-IR Consistency** - The requirement that a semantic point is preserved
  across typed AST, HIR, MIR, and LIR unless explicitly declared otherwise.
- **Cross-Mode Consistency** - The requirement that interpret, bytecode,
  native, and FFI modes agree on a semantic point unless explicitly declared
  otherwise.
