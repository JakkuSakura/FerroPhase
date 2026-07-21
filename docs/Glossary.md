# Glossary

This glossary defines the shared domain language for FerroPhase. Use these
terms consistently in docs, code, and diagnostics.

- **CompilerWorkScheduler** - The component that orders compiler work, records
  dependencies, and delivers request answers. It may use a stack internally.
- **Work item** - A scheduled unit of compiler work, such as parse, type, lower,
  execute, emit, or revalidate.
- **Request** - A need for an artefact or answer that can block other work.
- **RequestId** - Stable identity for a pending request before it is answered.
  Once resolved, artefacts are keyed by `FullyQualifiedPath`.
- **FullyQualifiedPath** - Resolved identity for a concrete item, function,
  block, or specialization. It includes resolved generic and comptime arguments
  when those affect identity.
- **Request answer** - The value, type, declaration, AST update, lowered
  artefact, or emitted output that satisfies a request.
- **CompileTimeNeed** - A value, type, declaration, code fragment, or
  specialization identity required before current work can continue.
- **Artefact** - Persisted or cached compiler data such as AST, typed
  annotations, HIR, MIR, LIR, bytecode, object code, or target AST output.
- **Frontend** - The parsing and normalization boundary that produces canonical
  AST plus provenance.
- **Typing** - Type inference and type query work that annotates canonical AST
  nodes and can emit `CompileTimeNeed`.
- **Comptime** - Compile-time execution through scheduler requests and shared
  scoped lowering/execution services.
- **Interpretation** - A compiler mode that requests executable artefacts and
  values. It is not a separate AST-only semantics.
- **Scoped lowering** - The typed AST -> HIR -> MIR -> LIR refinement for a
  concrete function, item, block, const body, generated fragment, or requested
  specialization.
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
