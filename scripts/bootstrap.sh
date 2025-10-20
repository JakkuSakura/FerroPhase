#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/bootstrap.sh

Run the bootstrap flow:
  0. Build the toolchain and capture the workspace snapshot.
  1. Use the Stage 0 compiler to emit LLVM IR, then link it into the Stage 1 binary.
  2. Re-run the compile with the Stage 1 binary to produce LLVM IR and a Stage 2 binary.
  3. Diff the Stage 1 and Stage 2 LLVM outputs.
USAGE
}

abs_path() {
  python3 - "$1" <<'PY'
import os, sys
print(os.path.abspath(sys.argv[1]))
PY
}

if [[ $# -ne 0 ]]; then
  echo "This script does not accept arguments." >&2
  usage
  exit 1
fi

repo_root="$(pwd)"
output_dir="$(abs_path "target/bootstrap")"
mkdir -p "$output_dir"

workspace_manifest="${repo_root}/Cargo.toml"
stem="workspace"

snapshot_path="${output_dir}/${stem}.stage0.ast.json"
stage1_ll="${output_dir}/${stem}.stage1.ll"
stage2_ll="${output_dir}/${stem}.stage2.ll"
if [[ "${OS:-}" == "Windows_NT" ]]; then
  bin_ext="exe"
else
  bin_ext="out"
fi
stage1_bin="${output_dir}/${stem}.stage1.${bin_ext}"
stage2_bin="${output_dir}/${stem}.stage2.${bin_ext}"

echo "== Stage 0: building full toolchain =="
cargo build --release --features bootstrap

fp_stage0_bin="${repo_root}/target/release/fp"
if [[ ! -x "$fp_stage0_bin" ]]; then
  echo "Missing Stage 0 compiler at ${fp_stage0_bin}" >&2
  exit 1
fi

echo "== Stage 0: parsing workspace ${workspace_manifest} to ${snapshot_path} =="
"$fp_stage0_bin" parse "$workspace_manifest" --no-resolve --snapshot "$snapshot_path"

if [[ ! -f "$snapshot_path" ]]; then
  echo "Stage 0 snapshot missing: ${snapshot_path}" >&2
  exit 1
fi

echo "== Stage 1: replay snapshot with Stage 0 compiler =="
# Bake Stage 2 targets into the Stage 1 compiler via env vars
export FP_BOOTSTRAP_SNAPSHOT="$snapshot_path"
export FP_BOOTSTRAP_OUTPUT="$stage2_ll"
FERROPHASE_BOOTSTRAP=1 FP_BOOTSTRAP_STAGE=1 "$fp_stage0_bin" compile "$snapshot_path" \
  --target llvm \
  --output "$stage1_ll"

if [[ ! -f "$stage1_ll" ]]; then
  echo "Stage 1 LLVM IR not found: ${stage1_ll}" >&2
  exit 1
fi

if ! clang "$stage1_ll" -o "$stage1_bin"; then
  echo "Stage 1 linking failed via clang" >&2
  exit 1
fi

if [[ ! -x "$stage1_bin" ]]; then
  echo "Stage 1 compiler binary not found after linking: ${stage1_bin}" >&2
  exit 1
fi

echo "== Stage 2: replay snapshot with Stage 1 compiler (${stage1_bin}) =="
# In bootstrap mode the compiler can emit LLVM IR to stdout when no --output is provided.
# Capture it to stage2 file to avoid relying on std::fs::write inside the self-hosted binary.
if ! FERROPHASE_BOOTSTRAP=1 FP_BOOTSTRAP_STAGE=2 "$stage1_bin" compile "$snapshot_path" --target llvm >"$stage2_ll"; then
  echo "Stage 2 compilation via Stage 1 compiler failed" >&2
  exit 1
fi

if [[ ! -f "$stage2_ll" ]]; then
  echo "Stage 2 LLVM IR not found: ${stage2_ll}" >&2
  exit 1
fi

echo "Stage 2: skipping binary link; comparing LLVM IR only"

echo "== Stage 3: verifying LLVM IR equality between stages =="
tmp_stage1="$(mktemp "${output_dir}/workspace.stage1.XXXXXX")"
tmp_stage2="$(mktemp "${output_dir}/workspace.stage2.XXXXXX")"
trap 'rm -f "$tmp_stage1" "$tmp_stage2"' EXIT
if [[ ! -s "$stage2_ll" ]]; then
  echo "Stage 2 LLVM IR is empty at ${stage2_ll}" >&2
  exit 1
fi

perl -pe 's/workspace\.stage\d+/workspace.stage/g' "$stage1_ll" >"$tmp_stage1"
perl -pe 's/workspace\.stage\d+/workspace.stage/g' "$stage2_ll" >"$tmp_stage2"
if diff -u "$tmp_stage1" "$tmp_stage2" >"${output_dir}/workspace.stage.diff"; then
  rm -f "${output_dir}/workspace.stage.diff"
  echo "LLVM IR identical between Stage 1 and Stage 2."
else
  echo "LLVM IR mismatch between stages; see ${output_dir}/workspace.stage.diff" >&2
  exit 1
fi
trap - EXIT

echo "Bootstrap flow complete."
echo "  Stage 0 snapshot: ${snapshot_path}"
echo "  Stage 1 LLVM:     ${stage1_ll}"
echo "  Stage 2 LLVM:     ${stage2_ll}"
echo "  Stage 1 binary:   ${stage1_bin}"
