#!/usr/bin/env bash
set -euo pipefail

QA_PREFIX="[qa]"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/qa_helpers.sh"
RISK="medium"

usage() {
  cat <<'USAGE'
Usage: scripts/qa_local.sh [--risk low|medium|high]

Local QA orchestrator for FerroPhase.
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --risk)
      shift
      RISK="${1:-}"
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      qa_err "Unknown argument: $1"
      usage >&2
      exit 2
      ;;
  esac
  shift
 done

case "$RISK" in
  low|medium|high)
    ;;
  *)
    qa_err "--risk must be low|medium|high (got: $RISK)"
    exit 2
    ;;
 esac

pick_fp_file() {
  local preferred="examples/01_const_eval_basics.fp"
  if [[ -f "$preferred" ]]; then
    echo "$preferred"
    return 0
  fi
  local first
  first=$(ls examples/*.fp 2>/dev/null | head -n 1 || true)
  if [[ -n "$first" ]]; then
    echo "$first"
    return 0
  fi
  return 1
}

select_pytest_runner() {
  if command -v uv >/dev/null 2>&1; then
    PYTEST_RUNNER=(uv run pytest)
    return 0
  fi
  if command -v pytest >/dev/null 2>&1; then
    PYTEST_RUNNER=(pytest)
    return 0
  fi
  if command -v python3 >/dev/null 2>&1; then
    PYTEST_RUNNER=(python3 -m pytest)
    return 0
  fi
  return 1
}

marker_present_in_tests() {
  local marker="$1"
  if command -v rg >/dev/null 2>&1; then
    rg -n "${marker}" "tests" >/dev/null 2>&1
    return $?
  fi
  if command -v grep >/dev/null 2>&1; then
    grep -R "${marker}" "tests" >/dev/null 2>&1
    return $?
  fi
  return 1
}

run_pytest_stage() {
  local name="$1"
  shift
  if ! select_pytest_runner; then
    qa_err "pytest runner not found (uv/pytest/python3)"
    qa_add_failure "${name}: missing pytest runner"
    qa_summarize_and_exit 1
  fi
  qa_run_stage "$name" "${PYTEST_RUNNER[@]}" "$@"
}

run_pytest_optional_marker() {
  local name="$1"
  local marker="$2"
  if marker_present_in_tests "$marker"; then
    run_pytest_stage "$name" -m "$marker"
  else
    qa_warn "marker not found (${marker}), skipped: ${name}"
  fi
}

qa_log "risk=${RISK}"

# Low risk: check_fp + quick examples + pytest e2e quick set
fp_file=""
if fp_file=$(pick_fp_file); then
  qa_require_script "check_fp" "scripts/check_fp.sh" "$fp_file"
else
  qa_err "no examples/*.fp found; cannot run check_fp"
  qa_add_failure "check_fp: missing examples"
  qa_summarize_and_exit 1
fi

qa_require_script "examples_exec" "scripts/run_examples_exec.sh"
qa_require_script "examples_bytecode" "scripts/run_examples_bytecode.sh"
qa_require_script "example_snapshots" "scripts/run_example_snapshots.sh"

run_pytest_optional_marker "pytest_risk_low" "risk_low"

if [[ "$RISK" == "medium" || "$RISK" == "high" ]]; then
  qa_require_script "rust_ui_tests" "scripts/run_rust_ui_tests.sh" both
  qa_require_script "semantic_matrix" "scripts/verify_semantics_matrix.sh"
  run_pytest_optional_marker "pytest_risk_medium" "risk_medium"
fi

if [[ "$RISK" == "high" ]]; then
  qa_require_script "verify_prod" "scripts/verify_prod.sh"
  run_pytest_optional_marker "pytest_risk_high" "risk_high"
  qa_require_script "bench_eight_queens" "scripts/bench_eight_queens.sh"
fi

qa_log "QA completed"
qa_summarize_and_exit 0
