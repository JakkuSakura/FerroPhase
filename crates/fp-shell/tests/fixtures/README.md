`operations/` and `connectors/` contain retained shell fixture data used by the `fp-shell`
test suite. This tree should contain only the cases needed by `fp-shell`, not a vendored
source mirror.

`../fp_shell_tests.fp` is the FP-authored harness entrypoint. It is intended to run via the core
interpreter and shell out to the built `fp` / `fp-shell` binaries:
`./target/debug/fp interpret crates/fp-shell/tests/fp_shell_tests.fp`
