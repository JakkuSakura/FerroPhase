#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

QA_HELPERS="${ROOT_DIR}/scripts/qa_helpers.sh"
if [[ -f "${QA_HELPERS}" ]]; then
  # shellcheck source=/dev/null
  source "${QA_HELPERS}"
else
  qa_log() { echo "[qa] $*"; }
  qa_err() { echo "[qa] ERROR: $*" >&2; }
  qa_run_stage() { local name="$1"; shift; qa_log "start: ${name}"; "$@"; qa_log "done: ${name}"; }
fi

SNAPSHOT_SCRIPT="${ROOT_DIR}/scripts/snapshot_ir.sh"
HASH_GUARD="${ROOT_DIR}/scripts/ir_hash_guard.py"
SNAPSHOT_DIR="${ROOT_DIR}/qa/ir_snapshots"
BASELINE="${SNAPSHOT_DIR}/ir_hashes.json"

if [[ ! -f "${SNAPSHOT_SCRIPT}" ]]; then
  qa_err "missing script: ${SNAPSHOT_SCRIPT}"
  exit 1
fi
if [[ ! -f "${HASH_GUARD}" ]]; then
  qa_err "missing script: ${HASH_GUARD}"
  exit 1
fi

qa_run_stage "snapshot_ir" bash "${SNAPSHOT_SCRIPT}"

if [[ "${UPDATE_IR_HASHES:-}" == "1" ]]; then
  qa_run_stage "ir_hash_guard(update)" \
    python -X utf8 "${HASH_GUARD}" \
    --snapshots-dir "${SNAPSHOT_DIR}" \
    --baseline "${BASELINE}" \
    --update
else
  qa_run_stage "ir_hash_guard" \
    python -X utf8 "${HASH_GUARD}" \
    --snapshots-dir "${SNAPSHOT_DIR}" \
    --baseline "${BASELINE}"
fi

qa_log "semantic matrix verification complete"
