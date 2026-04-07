# Production Readiness Map

This document maps the production-ready requirements to their documentation
sources. It does not change language semantics; it defines where each contract,
policy, and gate is documented.

## Requirement Map

1. Semantic contract + semantic mapping matrix
- Language contract: `docs/Language.md`
- Matrix template + per-frontend matrices: `docs/semantic/Matrix.md`, `docs/semantic/matrix/<frontend>.md`
- Baseline suite definition: `docs/semantic/BaselineSuite.md`

2. Frontend semantic goldens + regression suite
- Baseline suite definition: `docs/semantic/BaselineSuite.md`
- Gate and evidence rules: `docs/QualityAssurance.md`

3. Ownership/borrowing/lifetime + zero-cost abstraction constraints
- Contract statement: `docs/Language.md`
- Pipeline/Design embedding points: `docs/Design.md`, `docs/Pipeline.md`

4. Release gates (must-pass, non-bypassable)
- Gate policy and evidence: `docs/QualityAssurance.md`
- Release gate summary: `docs/Language.md`

5. Version governance + breaking change process
- Governance policy: `docs/VersionGovernance.md`
- Package-facing rules: `docs/Packages.md`

6. Standard library layering + deprecation policy
- Layering policy: `docs/Language.md`
- Module boundaries + deprecation windows: `docs/Modules.md`

7. Reproducible builds + signed releases
- Build reproducibility: `docs/ReleaseArtifacts.md`
- Build options & artifacts: `docs/BuildOptions.md`, `docs/Compile.md`
- Audit logs: `docs/Logging.md`

8. Minimum DX (LSP/formatter/diagnostics/staged debugging)
- Minimum DX bar: `docs/DeveloperExperience.md`
- Release gates tie-in: `docs/Language.md`, `docs/QualityAssurance.md`

## Navigation

- Semantic contract: `docs/Language.md`
- QA gates and evidence: `docs/QualityAssurance.md`
- Versioning and breaking changes: `docs/VersionGovernance.md`
- Release artifacts and signatures: `docs/ReleaseArtifacts.md`
- DX minimum: `docs/DeveloperExperience.md`
