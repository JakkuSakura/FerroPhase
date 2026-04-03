#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
work_dir="${TMPDIR:-/tmp}/fp-rust-cross-transpile-smoke"
rm -rf "${work_dir}"
mkdir -p "${work_dir}"

say() { printf "[e2e] %s\n" "$*"; }

say "Root: ${root_dir}"
say "Work: ${work_dir}"

say "Ensuring Rust target: x86_64-unknown-linux-gnu"
rustup target add x86_64-unknown-linux-gnu >/dev/null

say "Building fp CLI"
(
  cd "${root_dir}"
  cargo build -p fp-cli --bin fp >/dev/null
)

compile_and_run() {
  local name="$1"
  local src_rel="$2"
  local expected_rc="$3"

  local obj_in="${work_dir}/${name}.linux-x86_64.o"
  local exe_out="${work_dir}/${name}.darwin-aarch64.out"

  say "rustc -> object: ${src_rel}"
  rustc \
    --crate-type=lib \
    --target x86_64-unknown-linux-gnu \
    -C opt-level=0 \
    --emit=obj \
    -o "${obj_in}" \
    "${root_dir}/${src_rel}"

  say "fp compile (object->exec): ${name}"
  (
    cd "${root_dir}"
    ./target/debug/fp compile \
      --backend binary \
      --emitter native \
      --target-triple aarch64-apple-darwin \
      --exec \
      -o "${exe_out}" \
      "${obj_in}" >/dev/null
  )

  say "Running: ${exe_out}"
  chmod +x "${exe_out}"
  set +e
  "${exe_out}"
  local rc=$?
  set -e

  say "Exit code: ${rc} (expected ${expected_rc})"
  if [[ "${rc}" -ne "${expected_rc}" ]]; then
    say "FAILED: exit code mismatch"
    exit 1
  fi
}

compile_and_run "exit0" "tests/e2e/linux_x86_64_exit42.rs" 0
compile_and_run "getpid_exit0" "tests/e2e/linux_x86_64_getpid_exit0.rs" 0

say "OK"
