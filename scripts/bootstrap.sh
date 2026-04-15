#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT}"

cargo build -p fp-cli -p magnet

export FP_BIN="${ROOT}/target/debug/fp"
"${ROOT}/target/debug/magnet" build

mapfile -t OUTPUT_DIRS < <(find "${ROOT}" -type d -path "*/target/magnet/*/build" | sort)
if [ "${#OUTPUT_DIRS[@]}" -eq 0 ]; then
  echo "No magnet build output directory found under target/magnet/*/build"
  exit 1
fi

for dir in "${OUTPUT_DIRS[@]}"; do
  echo "Build artifacts: ${dir}"
  ls -la "${dir}"
  mapfile -t BINARIES < <(find "${dir}" -maxdepth 1 -type f \( -name "*.out" -o -name "*.exe" \) | sort)
  if [ "${#BINARIES[@]}" -gt 0 ]; then
    echo "Executable outputs:"
    printf '%s\n' "${BINARIES[@]}"
  else
    echo "No executable outputs found in ${dir}"
  fi
done
