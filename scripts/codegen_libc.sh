#!/usr/bin/env bash
set -euo pipefail

output_path=${1:?usage: scripts/codegen_libc.sh OUTPUT_PATH [HEADER ...]}
shift

if (($# == 0)); then
    set -- sys/types.h sys/wait.h unistd.h fcntl.h
fi

tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/fp-libc.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT
wrapper="$tmp_dir/libc.c"

fp_bin=${FP_BIN:-fp}
command -v "$fp_bin" >/dev/null 2>&1 || {
    echo "fp executable not found; set FP_BIN or install fp" >&2
    exit 1
}

for header in "$@"; do
    printf '#include <%s>\n' "$header" >> "$wrapper"
done

"$fp_bin" compile "$wrapper" \
    --package libc \
    --target fp \
    --skip-typing \
    --output "$output_path"
