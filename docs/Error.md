# Lowering Diagnostics: Non-Recoverable MIR To LIR Locals

The MIR to LIR lowering helper previously fabricated zero-valued registers when it
encountered a local without a mapped register. That approach produced invalid
LLVM IR (phantom `add i32 i32 0, i32 0` instructions) and masked genuine
lowering defects.

## Behaviour Change

`LirGenerator::get_or_create_register_for_place` now returns an error when a
`mir::Place` lacks a register assignment. Lowering work surfaces the error
through diagnostics attached to the work item. Even when error tolerance is
enabled, the compiler can keep processing the current work item so diagnostics
are collected, but the result is marked as failed and dependent requests cannot
consume partial LIR. The CLI reports diagnostics such as:

```
[mir-to-lir] MIR to LIR transformation failed: Optimization error: missing value for local 5; cannot lower MIR
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
