#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FP_BIN="${FP_BIN:-${ROOT_DIR}/target/release/fp}"

if [[ ! -x "${FP_BIN}" ]]; then
  if command -v fp >/dev/null 2>&1; then
    FP_BIN="fp"
  else
    cargo build -p fp-cli --release
  fi
fi

SOURCE="${ROOT_DIR}/examples/34_ffi_basics.fp"
if [[ ! -f "${SOURCE}" ]]; then
  echo "[qa] missing FFI example: ${SOURCE}" >&2
  exit 1
fi

OUT_DIR="${ROOT_DIR}/target/qa"
mkdir -p "${OUT_DIR}"
OUT_FILE="${OUT_DIR}/ffi_basics.fbc"

"${FP_BIN}" compile --backend bytecode --output "${OUT_FILE}" "${SOURCE}"
test -f "${OUT_FILE}"
