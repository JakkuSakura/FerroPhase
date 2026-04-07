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
- Messages must be actionable and reference the failing stage.

4. Staged Debugging
- Ability to persist intermediates by stage (AST, typed AST, HIR, MIR, LIR).
- Ability to disable a stage for isolation when supported.

## Release Gate Tie-in

- The DX bar is part of L1 release gates.
- Any release that opts out of L1 must document the opt-out reason and impact.

See `docs/Language.md` and `docs/QualityAssurance.md` for gate policy and evidence.
