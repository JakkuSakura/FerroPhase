# fp-ebpf

An experimental eBPF backend for FerroPhase.

## Scope

This crate is intentionally separated from `fp-native`.

eBPF is not a host-native executable target: it has a verifier-constrained VM,
its own calling convention, helper/map model, and usually emits eBPF-oriented
artifacts rather than normal host binaries.

## Current status

- Integrated into the CLI/pipeline as a dedicated backend target.
- Emits validated textual eBPF assembly for a restricted scalar/control-flow subset.
- Emits minimal ELF eBPF objects for the same subset.
- Keeps the artifact boundary separate so verifier-aware lowering can be added
  without leaking eBPF constraints into `fp-native`.

## External runtime ABI

`fp-ebpf` currently targets an external user-mode runtime contract. The default
workspace runtime binary now ships from this crate as `fp-ebpf-runtime`.

- Helper `1`: `time_now()` -> `u64`
- Helper `2`: `print(format_id, arg0, arg1, arg2, arg3)`
- Helper `3`: `println(format_id, arg0, arg1, arg2, arg3)`

ELF objects also carry:

- `.fp.ebpf.abi`: textual ABI description and helper IDs
- `.fp.ebpf.helpers`: helper table with stable helper names and symbol names
- `.fp.ebpf.fmt`: format-string table referenced by `format_id`
- `.fp.ebpf.calls`: helper callsite metadata `(function, byte_offset, helper, format_id, arg_count)`
- undefined helper symbols such as `__fp_helper_time_now` and `__fp_helper_println`
- helper call instructions are emitted with ELF relocations against those symbols instead of baked-in helper immediates
- `license`: currently `GPL\0`

From the CLI, execution is delegated to an external runtime executable via:

- `FP_EBPF_RUNTIME=/path/to/fp-ebpf-runtime`
- `FP_EBPF_RUNTIME_ARGS='...'`

Workspace example:

- `FP_EBPF_RUNTIME='cargo'`
- `FP_EBPF_RUNTIME_ARGS='run -q -p fp-ebpf --bin fp-ebpf-runtime --'`

Then run:

- `fp compile -b ebpf --exec input.fp`
- `fp run --backend ebpf input.fp`

## Next steps

- Expand the runtime ABI beyond `time_now` and print helpers.
- Add verifier-aware lowering rules for stack usage, helper calls, and control flow.
- Emit richer ELF eBPF objects with maps, relocations, and BTF metadata.
