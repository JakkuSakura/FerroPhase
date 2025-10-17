#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/bootstrap.sh [OPTIONS] <input>

Run the two-stage bootstrap flow:
  1. Build and run the full toolchain to emit an AST snapshot.
  2. Rebuild in bootstrap mode and replay the snapshot.

Options:
  --target <target>     Compilation target (rust|llvm|binary). Default: rust
  --output-dir <dir>    Directory for artifacts. Default: target/bootstrap
  -h, --help            Show this help message

Example:
  scripts/bootstrap.sh --target llvm examples/05_struct_generation.fp
USAGE
}

target="rust"
output_dir="target/bootstrap"
input=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --target)
      target="${2:-}"
      shift 2
      ;;
    --output-dir)
      output_dir="${2:-}"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    -*)
      echo "Unknown option: $1" >&2
      usage
      exit 1
      ;;
    *)
      if [[ -z "$input" ]]; then
        input="$1"
      else
        echo "Unexpected argument: $1" >&2
        usage
        exit 1
      fi
      shift
      ;;
  esac
done

if [[ -z "$input" ]]; then
  echo "Input source path is required." >&2
  usage
  exit 1
fi

case "$target" in
  rust|rs)
    target="rust"
    ext="rs"
    ;;
  llvm|ll)
    target="llvm"
    ext="ll"
    ;;
  binary)
    target="binary"
    ext="bin"
    ;;
  *)
    echo "Unsupported target: $target" >&2
    exit 1
    ;;
esac

mkdir -p "$output_dir"

input_path="$(realpath "$input")"
stem="$(basename "$input_path")"
stem="${stem%.*}"

stage0_output="${output_dir}/${stem}.stage0.${ext}"
stage1_output="${output_dir}/${stem}.stage1.${ext}"
snapshot_path="${input_path%.*}.ast.json"

echo "== Stage 0: building full toolchain =="
cargo build --release

echo "== Stage 0: emitting snapshot from ${input_path} =="
FERROPHASE_BOOTSTRAP_SNAPSHOT=1 \
  target/release/fp compile "$input_path" \
  --target "$target" \
  --output "$stage0_output"

if [[ ! -f "$snapshot_path" ]]; then
  echo "Snapshot not found at ${snapshot_path}" >&2
  exit 1
fi

stage0_snapshot="${output_dir}/${stem}.stage0.ast.json"
mv "$snapshot_path" "$stage0_snapshot"
snapshot_path="$stage0_snapshot"

echo "Snapshot written to ${snapshot_path}"

echo "== Stage 1: building bootstrap toolchain =="
cargo build --release --no-default-features --features bootstrap

echo "== Stage 1: replaying snapshot =="
FERROPHASE_BOOTSTRAP=1 \
  target/release/fp compile "$snapshot_path" \
  --target "$target" \
  --output "$stage1_output"

echo "Bootstrap compilation complete."
echo "  Stage 0 output: ${stage0_output}"
echo "  Stage 1 output: ${stage1_output}"
echo "  Snapshot:       ${snapshot_path}"
