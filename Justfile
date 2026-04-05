set shell := ["bash", "-lc"]

default:
    @just --list

qa-low:
    scripts/qa_local.sh --risk low

qa-medium:
    scripts/qa_local.sh --risk medium

qa-high:
    scripts/qa_local.sh --risk high

verify-semantics:
    scripts/verify_semantics_matrix.sh

snapshot-ir:
    scripts/snapshot_ir.sh

qa-evidence label="local":
    scripts/qa_evidence.sh {{label}}

verify-prod:
    scripts/verify_prod.sh

check-fp example="examples/01_const_eval_basics.fp":
    scripts/check_fp.sh {{example}}

run-examples:
    scripts/run_examples_exec.sh
    scripts/run_examples_bytecode.sh

rust-ui:
    scripts/run_rust_ui_tests.sh both

bench:
    scripts/bench_eight_queens.sh
