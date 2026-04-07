# FerroPhase Language Specification (Draft)

## 1. Scope

This document defines the language-level semantic contract, cross-IR/mode guarantees,
and release governance rules for FerroPhase. It does not introduce new semantics; it
codifies invariants that implementations must uphold across frontends, IRs, and
execution modes.

## 2. Core Semantic Invariant

FerroPhase must preserve identical observable semantics across all representations
(CST/LAST/AST/HIR/MIR/LIR) and across all execution modes (interpreter, bytecode
VM, compiled backends). Any semantic divergence is a correctness bug.

Observable semantics includes:
- User-visible outputs and side effects.
- Error/panic categories and propagation behavior.
- Resource release ordering when it is defined by the language contract.
- ABI/FFI-visible behavior that is part of the published contract.

Undefined behavior is not an escape hatch. If a frontend cannot preserve a
semantic point, it must declare a degradation rule and emit diagnostics that
explain the substitution.

`FERROPHASE_LOSSY` is explicitly outside this guarantee and must remain disabled
for correctness validation.

## 3. Language Model

- Multi-frontend, single semantic contract.
- Staged pipeline with canonical typed AST.
- Multiple execution modes that must remain semantically equivalent.

Pipeline overview:

```
SOURCE -> LAST -> AST -> AST^t -> AST^t' -> HIR^t -> MIR -> LIR -> backend
```

Mode entry points:

- Interpreter: AST^t -> runtime evaluation
- Bytecode: AST^t' -> HIR^t -> MIR -> LIR -> VM
- Native backends: AST^t' -> HIR^t -> MIR -> LIR -> AsmIR/LLVM
- AST target emit: AST^t' -> HIR^t -> target AST

## 4. Semantic Contract Levels

The contract is organized by constraint strength rather than by domain.
Each semantic point is assigned a level; frontends map that point explicitly.

- L0 Critical Requirement: mandatory, no opt-out.
- L1 Preferred Requirement: enabled by default, explicit opt-out required.
- L2 Optional Feature: disabled by default, explicit opt-in required.
- L3 Experimental Best Effort: explicit opt-in, coverage may be incomplete.

## 5. Cross-IR and Cross-Mode Guarantees

- Any frontend -> any IR path -> any execution mode must satisfy the same semantic
  contract.
- Observable behavior must be identical: outputs, error/panic categories, and any
  defined resource-release sequencing.
- Allowed differences are restricted to performance, diagnostics detail, and
  debug UX. Observable semantics must remain identical.
- Ownership/borrowing/lifetime and zero-cost abstraction rules must remain
  consistent across IRs/modes. If a frontend cannot fully preserve them, it must
  declare explicit degradation rules and emit explanatory diagnostics.

## 6. Frontend Semantic Mapping Matrix

Each frontend must declare how its features map to the shared semantic contract.
Non-applicable features must be explicitly opted out and must provide a fallback
or diagnostic rule.

Minimum matrix fields:

- SemanticPointId (e.g. L0-OBS-01)
- Description
- ContractLevel (L0/L1/L2/L3)
- FrontendMapping (keep / opt-out / opt-in)
- DegradationRule (required when not fully preserved)
- IRModeVariance (allowed or forbidden)
- BaselineTestId
- OwnershipBorrowingModel (full / conservative / degraded)
- LifetimeRuleSource (inferred / explicit / degraded)

If OwnershipBorrowingModel or LifetimeRuleSource is degraded, DegradationRule
and diagnostics requirements are mandatory.

Matrix location and update rules:
- Canonical template: `docs/semantic/Matrix.md`
- Frontend-specific matrices: `docs/semantic/matrix/<frontend>.md`
- Any new frontend or semantic point must update the matrix in the same change.

Example template:

| SemanticPointId | Description | Level | FrontendMapping | DegradationRule | IRModeVariance | BaselineTestId |
|---|---|---|---|---|---|---|
| L0-OBS-01 | Observable output equivalence | L0 | keep | N/A | forbidden | sem/obs/01 |

## 7. Semantic Consistency Baseline Suite

The baseline suite verifies cross-frontend, cross-IR, and cross-mode equivalence.
The suite is organized as a matrix:

```
[Frontend] x [IR Path] x [Mode]
```

Required coverage:

- L0: full coverage across all frontends/IRs/modes.
- L1: coverage for frontends that opt in by default or do not opt out.
- L2/L3: coverage only for explicit opt-in targets.

Matrix-to-suite mapping is mandatory:
- Each SemanticPointId must map to one or more BaselineTestId entries.
- Baseline tests must reference the semantic point they validate.

Baseline suite specification:
- Definition: `docs/semantic/BaselineSuite.md`
- Evidence and gate mapping: `docs/QualityAssurance.md`

## 8. Standard Library Layering

- Stable: public APIs with compatibility guarantees.
- Experimental: opt-in APIs, allowed to change without stability guarantees.
- Internal: compiler/runtime-only APIs, no external reliance.

Compatibility rules:

- Stable layer changes that break behavior require a major version bump and a
  migration note.
- Experimental layer changes must be documented as breaking or unstable.
- Internal layer changes are unconstrained but must not leak into public
  contracts.

High-level stdlib abstractions must preserve zero-cost semantics where claimed.
If a zero-cost claim cannot be honored, the library must document the cost and
provide a diagnostic or alternative path.

## 9. Release Gates

L0 release gates (mandatory):

- Semantic consistency baseline suite passes.
- No known semantic divergences across core IR/mode pairs.
- API/ABI changes are versioned with migration guidance.

L1 release gates (default, opt-out allowed):

- Diagnostics meet minimum quality bar (category, location, primary cause).
- Minimal tooling support: parsing + diagnostics + navigation in LSP.
- Stable stdlib APIs documented with compatibility promises.

L2/L3 gates:

- Optional or experimental features must declare coverage and risk.

## 10. Versioning and Change Governance

- Semantic contract versions are bound to release versions.
- Any semantic change must pass through release gates.
- Release artifacts must be traceable to source revision and build record.

## 11. Non-Goals

- This document does not define syntax or surface-language features.
- This document does not prescribe performance targets.
