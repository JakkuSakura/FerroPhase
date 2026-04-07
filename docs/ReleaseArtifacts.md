# Release Artifacts, Reproducible Builds, and Signatures

This document defines the minimum requirements for reproducible builds and
signed release artifacts. It does not change runtime semantics.

## Reproducible Build Definition

A build is reproducible if the same inputs produce identical artifacts:

Required fixed inputs:
- Source tree
- `Ferrophase.toml`
- `Ferrophase.lock`
- `workspace-graph.json`
- Toolchain version
- Build options (features, optimization, const-eval policy)

Environmental constraints:
- Use a fixed environment variable allowlist.
- Avoid time- or randomness-dependent behavior, or set a fixed seed.
- Normalize paths, locale, and timezone.

## Artifact Inventory

Each release must enumerate:
- Compiler outputs (executables, libraries, bytecode)
- Language bindings (TS/JS, Python, etc.)
- Debug symbols and metadata
- Build record (inputs + options + hashes)

## Build Record

Each release must publish a build record containing:
- Input digest (source + lockfile + workspace graph + toolchain + options)
- Artifact hashes (SHA-256 or stronger)
- Build environment summary (platform, env allowlist)

## Signed Releases

Required signing steps:
- Sign the artifact bundle and build record.
- Store signature alongside the published artifacts.
- Verify signatures during publish and during consumption.

Key governance:
- Keys must be rotated on a schedule or on compromise.
- CI must fail closed on missing or invalid signatures.

## Evidence

Release evidence must include:
- Build record and hashes
- Signature and verification logs

See `docs/QualityAssurance.md` for evidence format and storage.
