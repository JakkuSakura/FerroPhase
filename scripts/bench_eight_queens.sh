#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXAMPLE="${ROOT_DIR}/examples/27_bench_eight_queens.fp"
OUT_DIR="${ROOT_DIR}/target/bench"

mkdir -p "${OUT_DIR}"

FP_BIN="${FP_BIN:-${ROOT_DIR}/target/release/fp}"
if [[ ! -x "${FP_BIN}" ]]; then
  cargo build -p fp-cli --release
fi

bench_cmd() {
  local label="$1"
  local cmd="$2"

  echo ""
  echo "==> ${label}"
  local status=0
  set +e
  bash -c "${cmd}"
  status=$?
  set -e
  if [[ $status -ne 0 ]]; then
    echo "command failed with status ${status}"
  fi
}

run_wasm() {
  local wasm_path="$1"

  if command -v wasmtime >/dev/null 2>&1; then
    wasmtime run --invoke main "${wasm_path}" || wasmtime run --invoke _start "${wasm_path}"
    return
  fi
  if command -v wasmer >/dev/null 2>&1; then
    wasmer run "${wasm_path}" --invoke main || wasmer run "${wasm_path}" --invoke _start
    return
  fi

  local js_runner
  js_runner="$(mktemp)"
  cat > "${js_runner}" <<'JS'
import fs from "fs";

const wasmPath = process.argv[2];
const bytes = fs.readFileSync(wasmPath);
const module = await WebAssembly.instantiate(bytes, {});
const exports = module.instance.exports;

const candidates = ["main", "_start", "fp_main", "run_main"];
for (const name of candidates) {
  if (typeof exports[name] === "function") {
    exports[name]();
    process.exit(0);
  }
}

console.error("no callable entrypoint in wasm exports");
process.exit(2);
JS

  if command -v bun >/dev/null 2>&1; then
    bun "${js_runner}" "${wasm_path}"
  elif command -v node >/dev/null 2>&1; then
    node "${js_runner}" "${wasm_path}"
  else
    echo "no wasm runtime found (need wasmtime/wasmer/bun/node)"
  fi
  rm -f "${js_runner}"
}

echo "==> native rust (criterion)"
cargo bench -p fp --bench eight_queens

BIN_OUT="${OUT_DIR}/eight_queens_bin.out"
bench_cmd "fp compile (binary)" \
  "${FP_BIN} compile --backend binary --release --output ${BIN_OUT} ${EXAMPLE}"
bench_cmd "fp binary run" "${BIN_OUT}"

BYTECODE_OUT="${OUT_DIR}/eight_queens.fbc"
bench_cmd "fp compile (bytecode)" \
  "${FP_BIN} compile --backend bytecode --save-intermediates --output ${BYTECODE_OUT} ${EXAMPLE}"
bench_cmd "fp interpret (bytecode)" \
  "${FP_BIN} interpret ${BYTECODE_OUT}"

WASM_OUT="${OUT_DIR}/eight_queens_wasm"
bench_cmd "fp compile (wasm)" \
  "${FP_BIN} compile --backend wasm --release --output ${WASM_OUT} ${EXAMPLE}"
WASM_FILE="${WASM_OUT}.wasm"
if [[ ! -f "${WASM_FILE}" ]]; then
  WASM_FILE="${WASM_OUT}"
fi
bench_cmd "fp run (wasm)" "bash -c '$(declare -f run_wasm); run_wasm ${WASM_FILE}'"
