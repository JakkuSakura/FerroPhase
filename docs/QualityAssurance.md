# Automated QA for AI Coding Agents (Compiler Projects)

This is a concise, fully automated QA framework for compiler codebases. It is
designed for AI coding agents, not humans. Human review is optional and used
only for policy overrides.

## Scope

Applies to:
- New changes (features, refactors, fixes).
- Historical code (legacy modules, regressions, untested paths).

## Core Rules (Non‑Negotiable)

1. **Evidence or reject**: If required checks are missing, the change is blocked.
2. **Fail closed**: Unknown risk defaults to highest risk.
3. **Self-correcting**: The agent must repair failures and re-run.
4. **No silent behavior change**: All behavior changes must be declared and tested.

## Quick Start (Actionable)

1) Classify risk: `low | medium | high`
2) Select gates based on impacted compiler stages.
3) Run gates, collect evidence.
4) Auto-decision: pass → accept; fail → fix and repeat.

## Risk Levels

| Risk | Required Gates |
|------|----------------|
| Low | Build + lint + unit tests |
| Medium | Low + integration + stage-specific gates |
| High | Medium + runtime fixtures + bootstrap + perf |

If risk is unknown, treat as High.

## Stage-Specific Gates (Compiler)

Select only the gates that match the change:

- **Parsing/AST**: parser tests, AST snapshots, round‑trip tests.
- **Typing**: inference tests, negative cases, ambiguity checks.
- **Lowering (AST→HIR→MIR→LIR)**: IR snapshots, structural invariants.
- **Const eval**: const-folding tests and parity vs runtime.
- **Diagnostics**: UI/snapshot tests for messages/spans/codes.
- **Codegen/Backend**: IR/ASM snapshots, executable fixtures.
- **Optimization**: pass unit tests, semantic equivalence checks.
- **Bootstrap**: self-host build if supported.
- **Performance**: compile‑time and runtime benchmarks for hot paths.

If a gate is applicable but missing, the agent must add tests or block.

## Evidence Format (Machine‑Readable)

Each change produces a single file:

```
qa/evidence/<change-id>.yaml

change_id: <unique id>
intent: <short description>
risk: <low|medium|high>
stages: [parser, typing, lowering, ...]
invariants: [ ... ]
expected_behavior: [ ... ]
checks:
  build: pass
  lint: pass
  unit: pass
  integration: pass
  stage_gates: pass
  runtime_fixtures: pass
  bootstrap: pass
  perf: pass
artifacts:
  logs: [ ... ]
  reports: [ ... ]
```

Missing fields or failed checks are hard failures.

## Automated Review Rules (No Human Required)

- Reject broad refactors unless explicitly requested.
- Reject any new `unwrap`/panic on user-controlled inputs.
- Require stable IR/diagnostic snapshots unless the change declares deltas.
- Require regression tests for all fixed bugs.

## File-by-File Review (AI Agent, One-by-One)

When the agent is asked to review code quality or historical code, it performs
a deterministic one-by-one read of each relevant source file (in repo order),
not just pattern matching. Each file gets a short, explicit reasoning note from 
the source file, not individual lines.

### Procedure

1) **Enumerate files** in a stable order (lexicographic), excluding `target/`,
   `.git/`, `bootstrap/`, and any `tests/` paths unless explicitly requested.
2) **Read the entire file** top to bottom; do not stop at the first match.
3) **Summarize file intent** from module docs, public APIs, and main types.
4) **Identify concerns with explicit reasoning**:
   - Correctness gaps (incomplete lowering, missing invariants, undefined behavior).
   - Confusing semantics (implicit fallbacks, silent behavior changes).
   - Reliability risks (panic/unwrap in user paths, unsafe without invariants).
   - Each finding must include *why* it is a problem in this file, not just a label.
5) **Record findings** with:
   - File path and key lines.
   - Why it is risky/confusing (explicit reasoning tied to actual code flow).
   - Suggested remediation class (diagnostic, refactor, test, or guard).

### Output Format (Recommended)

```
File: <path>
Intent: <short statement>
Findings:
- <issue> — <reasoning> — <remediation>
Notes:
- <any follow-ups>
```

The agent must keep a cumulative report and never skip files unless told to.

## Historical Code (Legacy Stabilization)

When touching legacy areas:

1) Add a **baseline gate** first (snapshot or fixture) to lock current behavior.
2) Then change code and re-run gates.
3) Any change must be explicitly declared in the evidence file.

This prevents accidental regressions while enabling safe modernization.

## Automation Hooks (Suggested)

- CI pipeline runs all selected gates.
- Agent loop:
  1) Run gates.
  2) Fix failures.
  3) Re-run until pass or block.
- Evidence stored under `qa/evidence/`.

This framework is intentionally strict: the agent must prove correctness.
