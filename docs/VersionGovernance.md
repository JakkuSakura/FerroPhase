# Version Governance and Breaking Changes

This document defines version governance for FerroPhase and its ecosystem. It
codifies process and evidence requirements; it does not change language semantics.

## Versioning Rules

- Semantic contract versions are bound to release versions.
- Any change that affects L0/L1 semantic points requires a recorded breaking
  change decision and a major version bump.
- Minor versions may add opt-in features or L2/L3 points.
- Patch versions are reserved for bug fixes without semantic contract changes.

## Breaking Change Process (Required)

1. Proposal
- Create a change record that references impacted SemanticPointId entries.
- Update the frontend semantic mapping matrix and baseline suite references.

2. Deprecation Window
- Announce the change and provide migration notes.
- Maintain old behavior for a minimum deprecation window unless security
  requires immediate removal.

3. Removal
- Remove deprecated behavior only after the deprecation window.
- Update release notes and evidence records to reflect the change.

## Ecosystem Coordination

- Standard library stable layer changes require migration guidance and a major
  version bump.
- Multi-language bindings must remain version-aligned with the FerroPhase
  package version.

## Evidence Requirements

- Evidence file must include:
  - impacted SemanticPointId list
  - deprecation window description
  - migration guide location
  - baseline suite results before/after change

See `docs/QualityAssurance.md` for evidence format and gate execution.
