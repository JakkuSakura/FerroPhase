# Minimum Developer Experience (DX) Bar

This document defines the minimum DX requirements for a production-ready
FerroPhase toolchain. It does not change language semantics.

## Minimum Requirements

1. LSP
- Parsing and type navigation must work for core language features.
- Diagnostics must include category, location, and primary cause.

2. Formatter
- Deterministic output for core syntax.
- No formatting must change semantics.

3. Diagnostics Quality
- Error categories and codes must be stable.
- Messages must be actionable and reference the failing work item, request, or
  artefact.

4. Work Debugging
- Ability to persist requested artefacts (AST, typed AST annotations, HIR, MIR,
  LIR, bytecode, native objects).
- Ability to isolate a work kind or request when supported.

## Release Gate Tie-in

- The DX bar is part of L1 release gates.
- Any release that opts out of L1 must document the opt-out reason and impact.

See `docs/Language.md` and `docs/QualityAssurance.md` for gate policy and evidence.
