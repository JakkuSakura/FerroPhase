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
  cargo run --bin fp --release -- compile --backend text-bytecode --save-intermediates "${f}"
  cargo run --bin fp --release -- compile --backend bytecode --save-intermediates "${f}"
  fbc="${f%.fp}.fbc"
  if [[ ! -f "${fbc}" ]]; then
    echo "Missing bytecode output: ${fbc}"
    exit 1
  fi
  cargo run --bin fp --release -- interpret "${fbc}"
 done
