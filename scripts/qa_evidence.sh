#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EVIDENCE_DIR="${ROOT_DIR}/qa/evidence"
TEMPLATE="${EVIDENCE_DIR}/template.md"

label="${1:-local}"
ts="$(date "+%Y%m%d_%H%M%S")"
run_id="${ts}_${label}"
out_dir="${EVIDENCE_DIR}/${run_id}"
out_file="${out_dir}/evidence.md"
run_date="$(date "+%Y-%m-%d %H:%M:%S")"

mkdir -p "${out_dir}"

if [[ ! -f "${TEMPLATE}" ]]; then
  echo "[qa] ERROR: template missing: ${TEMPLATE}" >&2
  exit 1
fi

git_sha="unknown"
if command -v git >/dev/null 2>&1; then
  git_sha="$(git -C "${ROOT_DIR}" rev-parse HEAD 2>/dev/null || echo unknown)"
fi

host="$(hostname 2>/dev/null || echo unknown)"
os="$(uname -a 2>/dev/null || echo unknown)"

sed \
  -e "s/{{run_id}}/${run_id}/g" \
  -e "s/{{date}}/${run_date}/g" \
  -e "s/{{git_sha}}/${git_sha}/g" \
  -e "s/{{host}}/${host}/g" \
  -e "s/{{os}}/${os}/g" \
  -e "s#{{command}}#${0} ${label}#g" \
  -e "s/{{risk}}/unknown/g" \
  -e "s/{{fixtures}}/unknown/g" \
  -e "s/{{overrides}}/none/g" \
  -e "s/{{summary}}/pending/g" \
  -e "s/{{failures}}/none/g" \
  -e "s/{{notes}}/pending/g" \
  -e "s/{{logs}}/pending/g" \
  -e "s/{{snapshots}}/pending/g" \
  "${TEMPLATE}" > "${out_file}"

echo "[qa] created: ${out_file}"
