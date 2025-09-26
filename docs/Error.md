# Pipeline Diagnostics: Non-Recoverable MIR→LIR Locals

The MIR→LIR lowering helper previously fabricated zero-valued registers when it
encountered a local without a mapped register. That approach produced invalid
LLVM IR (phantom `add i32 i32 0, i32 0` instructions) and masked genuine
lowering defects.

## Behaviour Change

`LirGenerator::get_or_create_register_for_place` now returns an error when a
`mir::Place` lacks a register assignment. The lowering stage surfaces the error
through `StageReport`. Even when error tolerance is enabled we keep processing
the current stage so its diagnostics are collected, but the stage result is
marked as a failure and the pipeline halts before handing partial LIR to later
phases. The CLI reports diagnostics such as:

```
❌ [mir→lir] MIR→LIR transformation failed: Optimization error: MIR→LIR: missing value for local 5; cannot lower MIR
```

There is no fallback LIR program. Upstream transforms must either materialise
the local's value or provide a placeholder before MIR→LIR runs.

## Next Steps

- Investigate which MIR locals are emitted without assignments for the control
  flow demo (`examples/03_control_flow.fp`).
- Extend MIR generation to populate `register_map` for every live local before
  invoking the LIR builder.
- Add targeted tests that ensure missing locals raise this error instead of
  fabricating values.
