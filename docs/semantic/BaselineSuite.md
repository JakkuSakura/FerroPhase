# Semantic Consistency Baseline Suite

This document defines the baseline suite that validates semantic equivalence
across frontends, IR paths, and execution modes.

## Scope

- Cross-frontend equivalence.
- Cross-IR equivalence (LAST/AST/HIR/MIR/LIR).
- Cross-mode equivalence (interpreter, bytecode VM, native backends).

## Matrix Coverage

The suite is organized as a matrix:

```
[Frontend] x [IR Path] x [Mode]
```

Coverage requirements:
- L0 semantic points: full coverage across all frontends/IRs/modes.
- L1: coverage for default-enabled frontends or non-opted-out points.
- L2/L3: only explicit opt-in targets.

## Mapping Rules

- Each SemanticPointId must map to one or more BaselineTestId entries.
- Each BaselineTestId must cite the SemanticPointId it validates.
- Matrix changes and baseline changes must land in the same change set.

## Evidence and Gates

- Evidence format and storage are defined in `docs/QualityAssurance.md`.
- Gate execution entrypoint: `scripts/qa_local.sh high`.
- IR snapshot and hash rules are defined by the semantic matrix gate.

## Baseline Test Template

```
BaselineTestId: sem/obs/01
SemanticPointId: L0-OBS-01
Frontends: [ferro, typescript]
IRPaths: [AST->HIR, AST->MIR]
Modes: [interpreter, bytecode, native]
Fixtures:
  - tests/fixtures/semantic/obs/01.fp
Expected:
  - outputs match across modes
  - error category is identical
Artifacts:
  - qa/ir_snapshots/<test-id>/
  - qa/evidence/<change-id>.yaml
```
