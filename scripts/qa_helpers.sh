#!/usr/bin/env bash
set -euo pipefail

QA_PREFIX="${QA_PREFIX:-[qa]}"
QA_FAILURES=()

qa_now() {
  date "+%Y-%m-%d %H:%M:%S"
}

qa_log() {
  echo "${QA_PREFIX} $*"
}

qa_warn() {
  echo "${QA_PREFIX} WARN: $*" >&2
}

qa_err() {
  echo "${QA_PREFIX} ERROR: $*" >&2
}

qa_add_failure() {
  local message="$1"
  QA_FAILURES+=("${message}")
}

qa_summarize_and_exit() {
  local code="$1"
  if [[ ${#QA_FAILURES[@]} -gt 0 ]]; then
    qa_log "failures:"
    for item in "${QA_FAILURES[@]}"; do
      qa_log "- ${item}"
    done
  else
    qa_log "failures: none"
  fi
  exit "$code"
}

qa_run_stage() {
  local name="$1"
  shift
  qa_log "start: ${name}"
  local start_ts
  start_ts=$(date +%s)
  local status=0
  set +e
  "$@"
  status=$?
  set -e
  local end_ts
  end_ts=$(date +%s)
  local elapsed=$((end_ts - start_ts))
  if [[ $status -ne 0 ]]; then
    qa_err "failed: ${name} (exit=${status}, ${elapsed}s)"
    qa_add_failure "${name}"
    qa_summarize_and_exit 1
  fi
  qa_log "done: ${name} (${elapsed}s)"
}

qa_require_script() {
  local name="$1"
  local script_path="$2"
  shift 2
  if [[ ! -f "$script_path" ]]; then
    qa_err "missing script: ${script_path}"
    qa_add_failure "${name}: missing ${script_path}"
    qa_summarize_and_exit 1
  fi
  qa_run_stage "$name" bash "$script_path" "$@"
}

qa_optional_script() {
  local name="$1"
  local script_path="$2"
  shift 2
  if [[ ! -f "$script_path" ]]; then
    qa_warn "missing script, skipped: ${script_path}"
    return 0
  fi
  qa_run_stage "$name" bash "$script_path" "$@"
}
