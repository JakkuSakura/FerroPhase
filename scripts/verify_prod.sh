#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=./qa_helpers.sh
source "${ROOT_DIR}/scripts/qa_helpers.sh"

RISK_INVENTORY="${ROOT_DIR}/qa/risk_inventory.yaml"
if [[ ! -f "${RISK_INVENTORY}" ]]; then
  qa_err "missing risk inventory: ${RISK_INVENTORY}"
  exit 1
fi
qa_log "risk inventory: ${RISK_INVENTORY}"
qa_log "risk inventory lines: $(wc -l < "${RISK_INVENTORY}")"

FP_BIN="${FP_BIN:-${ROOT_DIR}/target/release/fp}"
if [[ -x "${FP_BIN}" ]]; then
  :
elif command -v fp >/dev/null 2>&1; then
  FP_BIN="fp"
else
  qa_run_stage "build-fp-release" cargo build -p fp-cli --release
  FP_BIN="${ROOT_DIR}/target/release/fp"
fi

if [[ "${FP_BIN}" != "fp" && ! -x "${FP_BIN}" ]]; then
  qa_err "fp binary not found after build: ${FP_BIN}"
  exit 1
fi

export FP_BIN

if command -v uv >/dev/null 2>&1; then
  PYTEST_RUNNER=(uv run pytest)
elif command -v pytest >/dev/null 2>&1; then
  PYTEST_RUNNER=(pytest)
elif command -v python3 >/dev/null 2>&1; then
  PYTEST_RUNNER=(python3 -m pytest)
else
  qa_err "pytest runner not found (uv/pytest/python3)"
  exit 1
fi

qa_require_script "risk-min" "${ROOT_DIR}/scripts/run_risk_min.sh"

RUST_UI_LIMIT="${FP_RUST_UI_LIMIT:-50}"
RUST_UI_ARGS=(both --fp "${FP_BIN}")
if [[ -n "${RUST_UI_LIMIT}" ]]; then
  if [[ "${RUST_UI_LIMIT}" =~ ^[0-9]+$ ]]; then
    RUST_UI_ARGS+=(--limit "${RUST_UI_LIMIT}")
  else
    qa_err "FP_RUST_UI_LIMIT must be a number: ${RUST_UI_LIMIT}"
    exit 1
  fi
fi

qa_require_script "rust-ui-tests" "${ROOT_DIR}/scripts/run_rust_ui_tests.sh" "${RUST_UI_ARGS[@]}"

qa_run_stage "jit-interpret-e2e" "${PYTEST_RUNNER[@]}" "tests/test_jit_interpret_e2e.py"

echo "${QA_PREFIX} high-risk gates: pass"
