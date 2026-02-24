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
  name="$(basename "${f}" .fp)"
  out_dir="examples/generated"
  mkdir -p "${out_dir}"
  out_file="${out_dir}/${name}.out"
  if ! cargo run --bin fp --release -- compile --exec --save-intermediates --output "${out_file}" "${f}"; then
    echo "FAILED: ${f}"
  fi
done
