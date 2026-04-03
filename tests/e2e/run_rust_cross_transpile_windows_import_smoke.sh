#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
work_dir="${TMPDIR:-/tmp}/fp-rust-cross-transpile-windows-import-smoke"
rm -rf "${work_dir}"
mkdir -p "${work_dir}"

say() { printf "[e2e-win] %s\n" "$*"; }

say "Root: ${root_dir}"
say "Work: ${work_dir}"

say "Ensuring Rust target: x86_64-unknown-linux-gnu"
rustup target add x86_64-unknown-linux-gnu >/dev/null

say "Building fp CLI"
(
  cd "${root_dir}"
  cargo build -p fp-cli --bin fp >/dev/null
)

obj_in="${work_dir}/exit0.linux-x86_64.o"
exe_out="${work_dir}/exit0.windows-x86_64.exe"

say "rustc -> object: tests/e2e/linux_x86_64_exit42.rs"
rustc \
  --crate-type=lib \
  --target x86_64-unknown-linux-gnu \
  -C opt-level=0 \
  --emit=obj \
  -o "${obj_in}" \
  "${root_dir}/tests/e2e/linux_x86_64_exit42.rs"

say "fp compile (object->windows exe): ${exe_out}"
(
  cd "${root_dir}"
  ./target/debug/fp compile \
    --backend binary \
    --emitter native \
    --target-triple x86_64-pc-windows-msvc \
    --exec \
    -o "${exe_out}" \
    "${obj_in}" >/dev/null
)

say "Validating PE structure and imports"
EXE_OUT="${exe_out}" python3 - <<'PY'
import os
import pathlib
import struct

p = pathlib.Path(os.environ["EXE_OUT"])
b = p.read_bytes()
assert b[:2] == b"MZ", "missing MZ header"
e_lfanew = struct.unpack_from("<I", b, 0x3C)[0]
assert b[e_lfanew:e_lfanew+4] == b"PE\0\0", "missing PE signature"
# Conservative import checks (we don't fully parse import directory here).
hay = b.lower()
assert b"kernel32.dll" in hay, "missing kernel32.dll string"
assert b"exitprocess" in hay, "missing ExitProcess string"
assert b"\x0f\x05" not in b, "unexpected syscall instruction bytes (0f 05) in PE output"
print("ok")
PY

say "OK"
