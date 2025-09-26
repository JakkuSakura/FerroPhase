# Pipeline Diagnostics: Non-Recoverable MIR→LIR Locals

The MIR→LIR lowering helper previously fabricated zero-valued registers when it
encountered a local without a mapped register. That approach produced invalid
LLVM IR (phantom `add i32 i32 0, i32 0` instructions) and masked genuine
lowering defects.

## Behaviour Change

`LirGenerator::get_or_create_register_for_place` now returns an error when a
`mir::Place` lacks a register assignment. The lowering stage surfaces the error
through `StageReport`, causing the pipeline to halt with a diagnostic such as:

```
❌ [mir→lir] MIR→LIR transformation failed: Optimization error: MIR→LIR: missing value for local 5; cannot lower MIR
```

No fallback occurs. Upstream transforms must either materialise the local's
value or provide a placeholder before MIR→LIR runs.

## Next Steps

- Investigate which MIR locals are emitted without assignments for control-flow
  examples (e.g. `examples/13_control_flow.rs`).
- Extend MIR generation to populate `register_map` for every live local before
  invoking the LIR builder.
- Add targeted tests that ensure missing locals raise this error instead of
  fabricating values.

