#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FIXTURE_DIR="${ROOT_DIR}/tests/fixtures/semantic"
OUT_DIR="${ROOT_DIR}/qa/ir_snapshots"

FP_BIN="${FP_BIN:-${ROOT_DIR}/target/release/fp}"
if [[ ! -x "${FP_BIN}" ]]; then
  if command -v fp >/dev/null 2>&1; then
    FP_BIN="fp"
  else
    cargo build -p fp-cli --release
  fi
fi

if [[ ! -d "${FIXTURE_DIR}" ]]; then
  echo "[qa] ERROR: fixtures not found: ${FIXTURE_DIR}" >&2
  exit 1
fi

mkdir -p "${OUT_DIR}"

while IFS= read -r -d '' file; do
  rel="${file#${FIXTURE_DIR}/}"
  base="${rel%.fp}"
  out_base="${OUT_DIR}/${base}"
  mkdir -p "$(dirname "${out_base}")"
  echo "[qa] snapshot ${rel}"
  "${FP_BIN}" compile --backend bytecode --save-intermediates --output "${out_base}.fbc" "${file}"
done < <(find "${FIXTURE_DIR}" -type f -name "*.fp" -print0 | sort -z)

echo "[qa] snapshots saved to ${OUT_DIR}"
