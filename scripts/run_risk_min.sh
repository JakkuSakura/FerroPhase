#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=./qa_helpers.sh
source "${ROOT_DIR}/scripts/qa_helpers.sh"

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
    rg -n "${marker}" "${ROOT_DIR}/tests" >/dev/null 2>&1
    return $?
  fi
  if command -v grep >/dev/null 2>&1; then
    grep -R "${marker}" "${ROOT_DIR}/tests" >/dev/null 2>&1
    return $?
  fi
  return 1
}

if ! select_pytest_runner; then
  qa_err "pytest runner not found (uv/pytest/python3)"
  exit 1
fi

if ! marker_present_in_tests "risk_min"; then
  qa_warn "marker not found (risk_min), skipped"
  exit 0
fi

qa_run_stage "pytest_risk_min" "${PYTEST_RUNNER[@]}" -m risk_min "$@"
