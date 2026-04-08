#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

QA_HELPERS="${ROOT_DIR}/scripts/qa_helpers.sh"
if [[ -f "${QA_HELPERS}" ]]; then
  # shellcheck source=/dev/null
  source "${QA_HELPERS}"
else
  qa_log() { echo "[qa] $*"; }
  qa_warn() { echo "[qa] WARN: $*" >&2; }
  qa_err() { echo "[qa] ERROR: $*" >&2; }
  qa_run_stage() { local name="$1"; shift; qa_log "start: ${name}"; "$@"; qa_log "done: ${name}"; }
fi

SNAPSHOT_SCRIPT="${ROOT_DIR}/scripts/snapshot_ir.sh"
HASH_GUARD="${ROOT_DIR}/scripts/ir_hash_guard.py"
SNAPSHOT_ROOT="${ROOT_DIR}/qa/ir_snapshots"
BASELINE="${SNAPSHOT_ROOT}/ir_hashes.json"

EVIDENCE_PATH=""

create_evidence() {
  local evidence_out
  if ! evidence_out="$(bash "${ROOT_DIR}/scripts/qa_evidence.sh" "semantic-matrix" 2>/dev/null)"; then
    qa_warn "qa_evidence failed (semantic-matrix)"
    return 0
  fi
  qa_log "${evidence_out}"
  if [[ "${evidence_out}" =~ created:\ (.*)$ ]]; then
    EVIDENCE_PATH="${BASH_REMATCH[1]}"
  fi
}

update_evidence_line() {
  local path="$1"
  local needle="$2"
  local replacement="$3"
  if [[ -z "${path}" || ! -f "${path}" ]]; then
    return 0
  fi
  python -X utf8 - "${path}" "${needle}" "${replacement}" <<'PY'
import sys
from pathlib import Path

path = Path(sys.argv[1])
needle = sys.argv[2]
replacement = sys.argv[3]
text = path.read_text(encoding="utf-8")
if needle not in text:
    sys.exit(0)
path.write_text(text.replace(needle, replacement, 1), encoding="utf-8")
PY
}

BACKENDS_ARG=""
if [[ "${1:-}" == "--backends" ]]; then
  BACKENDS_ARG="${2:-}"
  shift 2
fi

BACKENDS_CSV="${BACKENDS_ARG:-${BACKENDS:-}}"

create_evidence
if [[ -n "${BACKENDS_CSV}" ]]; then
  update_evidence_line "${EVIDENCE_PATH}" "Notes: pending" "Notes: backends=${BACKENDS_CSV}"
else
  update_evidence_line "${EVIDENCE_PATH}" "Notes: pending" "Notes: backends=auto"
fi
update_evidence_line "${EVIDENCE_PATH}" "Snapshots: pending" "Snapshots: ${SNAPSHOT_ROOT} (baseline ${BASELINE})"

EXTS=(
  ".ast"
  ".ast-typed"
  ".ast-eval"
  ".hir"
  ".mir"
  ".lir"
)

if [[ ! -f "${SNAPSHOT_SCRIPT}" ]]; then
  qa_err "missing script: ${SNAPSHOT_SCRIPT}"
  exit 1
fi
if [[ ! -f "${HASH_GUARD}" ]]; then
  qa_err "missing script: ${HASH_GUARD}"
  exit 1
fi

if [[ -n "${BACKENDS_CSV}" ]]; then
  qa_run_stage "snapshot_ir" bash "${SNAPSHOT_SCRIPT}" --backends "${BACKENDS_CSV}"
else
  qa_run_stage "snapshot_ir" bash "${SNAPSHOT_SCRIPT}"
fi

HAS_ROOT_FILES=0
for ext in "${EXTS[@]}"; do
  if find "${SNAPSHOT_ROOT}" -maxdepth 1 -type f -name "*${ext}" -print -quit | grep -q .; then
    HAS_ROOT_FILES=1
    break
  fi
done

HAS_SUBDIRS=0
if find "${SNAPSHOT_ROOT}" -mindepth 1 -maxdepth 1 -type d -print -quit | grep -q .; then
  HAS_SUBDIRS=1
fi

IR_HASH_ARGS=()
if [[ -n "${BACKENDS_CSV}" ]]; then
  IR_HASH_ARGS+=(--snapshots-root "${SNAPSHOT_ROOT}" --backends "${BACKENDS_CSV}")
elif [[ "${HAS_ROOT_FILES}" == "1" ]]; then
  IR_HASH_ARGS+=(--snapshots-dir "${SNAPSHOT_ROOT}")
elif [[ "${HAS_SUBDIRS}" == "1" ]]; then
  IR_HASH_ARGS+=(--snapshots-root "${SNAPSHOT_ROOT}" --backends "bytecode")
else
  IR_HASH_ARGS+=(--snapshots-root "${SNAPSHOT_ROOT}" --backends "bytecode")
fi

if [[ "${UPDATE_IR_HASHES:-}" == "1" ]]; then
  qa_run_stage "ir_hash_guard(update)" \
    python -X utf8 "${HASH_GUARD}" \
    "${IR_HASH_ARGS[@]}" \
    --baseline "${BASELINE}" \
    --update
else
  qa_run_stage "ir_hash_guard" \
    python -X utf8 "${HASH_GUARD}" \
    "${IR_HASH_ARGS[@]}" \
    --baseline "${BASELINE}"
fi

qa_log "semantic matrix verification complete"
