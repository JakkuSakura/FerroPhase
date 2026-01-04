#!/usr/bin/env bash
set -euo pipefail

llvm_prefix="${LLVM_SYS_210_PREFIX:-}"
if [[ -z "${llvm_prefix}" ]] && command -v brew >/dev/null 2>&1; then
  llvm_prefix="$(brew --prefix llvm@21)"
fi

if [[ -n "${llvm_prefix}" ]]; then
  export LLVM_SYS_210_PREFIX="${llvm_prefix}"
fi

for f in examples/*.fp; do
  echo "==> ${f}"
  if ! cargo run --bin fp -- compile --exec --save-intermediates "${f}"; then
    echo "FAILED: ${f}"
  fi
done
