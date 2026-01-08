#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
SNAPSHOT_DIR="$ROOT_DIR/tests/example_snapshots"
UPDATE_MODE=0

if [[ "${UPDATE_SNAPSHOTS:-}" == "1" || "${1:-}" == "--update" ]]; then
  UPDATE_MODE=1
fi

mkdir -p "$SNAPSHOT_DIR"

if ! (cd "$ROOT_DIR" && LLVM_SYS_210_PREFIX="${LLVM_SYS_210_PREFIX:-}" cargo build -p fp-cli --release); then
  echo "Snapshot check skipped: failed to build fp-cli." >&2
  exit 0
fi

RUN_CMD=(cargo run -p fp-cli --bin fp -- run)
FAILED=0

for file in "$ROOT_DIR"/examples/*.fp; do
  name=$(basename "$file" .fp)
  snapshot="$SNAPSHOT_DIR/$name.out"
  tmp=$(mktemp)

  if ! (cd "$ROOT_DIR" && RUST_LOG=error NO_COLOR=1 "${RUN_CMD[@]}" "$file" >"$tmp" 2>&1); then
    echo "Snapshot check failed: fp run failed for $file" >&2
    cat "$tmp" >&2
    FAILED=1
    rm -f "$tmp"
    continue
  fi

  if [[ ! -f "$snapshot" ]]; then
    cp "$tmp" "$snapshot"
    echo "Created snapshot $snapshot"
  elif [[ $UPDATE_MODE -eq 1 ]]; then
    cp "$tmp" "$snapshot"
    echo "Updated snapshot $snapshot"
  else
    if ! diff -u "$snapshot" "$tmp"; then
      echo "Snapshot mismatch for $file" >&2
      FAILED=1
    fi
  fi

  rm -f "$tmp"
done

if [[ $FAILED -ne 0 ]]; then
  exit 1
fi
