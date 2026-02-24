#!/usr/bin/env bash
set -euo pipefail

target_triple="${1:-x86_64-unknown-linux-gnu}"
input="${2:-examples/25_cross_compile.fp}"
output="${3:-examples/generated/25_cross_compile.ll}"

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  cat <<'EOF'
Usage: scripts/cross_compile_ll.sh [target_triple] [input] [output]

Defaults:
  target_triple: x86_64-unknown-linux-gnu
  input: examples/25_cross_compile.fp
  output: examples/generated/25_cross_compile.ll
EOF
  exit 0
fi

mkdir -p "$(dirname "${output}")"

cargo run -p fp-cli --bin fp -- \
  compile \
  --emitter llvm \
  --target "${target_triple}" \
  --output "${output}" \
  "${input}"

echo "LLVM IR written to: ${output}"
