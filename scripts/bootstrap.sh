#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"

cargo build -p fp-cli -p magnet

export FP_BIN="${ROOT}/target/debug/fp"
OUT_DIR="${ROOT}/target/bootstrap/build"
mkdir -p "${OUT_DIR}"

# NOTE: `magnet build` builds full crates using the workspace graph and currently
# requires more language support than bootstrap mode provides. For bootstrap we
# compile a single entrypoint without a workspace graph and verify the produced
# native executable exists.
"${FP_BIN}" compile "${ROOT}/src/bin/fptest.fp" \
  --backend binary \
  --output "${OUT_DIR}/fptest.out"

echo "Build artifacts: ${OUT_DIR}"
ls -la "${OUT_DIR}"
