# Semantic Mapping Matrix (Template)

This document defines the canonical matrix template for mapping frontend semantics
into the shared FerroPhase semantic contract. Each frontend must maintain its own
matrix under `docs/semantic/matrix/<frontend>.md`.

## Matrix Rules

- Every SemanticPointId in the contract must appear in each frontend matrix.
- Any opt-out or degradation must include a DegradationRule and diagnostics.
- OwnershipBorrowingModel and LifetimeRuleSource are mandatory fields.
- Each SemanticPointId must map to one or more BaselineTestId entries.

## Field Definitions

- SemanticPointId: Stable identifier (e.g. L0-OBS-01).
- Description: Contract requirement statement.
- ContractLevel: L0/L1/L2/L3.
- FrontendMapping: keep / opt-out / opt-in.
- DegradationRule: Required when not fully preserved.
- IRModeVariance: allowed or forbidden.
- BaselineTestId: Links to baseline suite tests.
- OwnershipBorrowingModel: full / conservative / degraded.
- LifetimeRuleSource: inferred / explicit / degraded.
- Diagnostics: Required messages when degradation is applied.

## Template

| SemanticPointId | Description | Level | FrontendMapping | DegradationRule | IRModeVariance | BaselineTestId | OwnershipBorrowingModel | LifetimeRuleSource | Diagnostics |
|---|---|---|---|---|---|---|---|---|---|
| L0-OBS-01 | Observable output equivalence | L0 | keep | N/A | forbidden | sem/obs/01 | full | inferred | N/A |
