#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${root_dir}"

llvm_prefix="${LLVM_SYS_210_PREFIX:-}"
if [[ -z "${llvm_prefix}" ]] && command -v brew >/dev/null 2>&1; then
  if llvm_prefix="$(brew --prefix llvm@21 2>/dev/null)"; then
    :
  else
    llvm_prefix=""
  fi
fi

if [[ -n "${llvm_prefix}" ]]; then
  export LLVM_SYS_210_PREFIX="${llvm_prefix}"
fi

if ! command -v timeout >/dev/null 2>&1; then
  echo "The 'timeout' command is required to bound example execution" >&2
  exit 1
fi

example_timeout="${EXAMPLE_TIMEOUT_SECONDS:-300}s"
fp_bin="${root_dir}/target/release/fp"
cargo build --release --bin fp

shopt -s nullglob
examples=(examples/*.fp)
if [[ ${#examples[@]} -eq 0 ]]; then
  echo "No examples/*.fp files found" >&2
  exit 1
fi

failures=()
for f in "${examples[@]}"; do
  echo "==> ${f}"
  name="$(basename "${f}" .fp)"
  out_dir="examples/generated"
  mkdir -p "${out_dir}"
  out_file="${out_dir}/${name}.out"
  if ! timeout --foreground -k 1s "${example_timeout}" "${fp_bin}" compile --package examples --exec --save-intermediates --output "${out_file}" "${f}"; then
    echo "FAILED: ${f}"
    failures+=("${f}")
    continue
  fi
  if [[ ! -f "${out_file}" ]]; then
    echo "FAILED: missing executable ${out_file}"
    failures+=("${f}")
  fi
done

if [[ ${#failures[@]} -ne 0 ]]; then
  echo "${#failures[@]} example(s) failed:" >&2
  printf '  %s\n' "${failures[@]}" >&2
  exit 1
fi
