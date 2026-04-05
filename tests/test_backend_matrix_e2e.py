import os
import re
import subprocess
import tempfile
from pathlib import Path

import pytest

pytestmark = [pytest.mark.e2e, pytest.mark.risk_high]

_BUILT = False


def repo_root() -> Path:
    return Path(__file__).resolve().parents[1]


def build_fp(root: Path) -> Path:
    global _BUILT
    fp_bin_env = os.environ.get("FP_BIN")
    if fp_bin_env:
        fp_path = Path(fp_bin_env)
        if fp_path.exists():
            return fp_path
    if _BUILT:
        return root / "target" / "debug" / "fp"
    subprocess.run(
        ["cargo", "build", "-p", "fp-cli", "--bin", "fp"],
        check=True,
        cwd=root,
    )
    _BUILT = True
    return root / "target" / "debug" / "fp"


def pick_example(root: Path) -> Path:
    preferred = root / "examples" / "01_const_eval_basics.fp"
    if preferred.exists():
        return preferred
    candidates = sorted((root / "examples").glob("*.fp"))
    if not candidates:
        raise RuntimeError("Missing examples/*.fp for backend matrix smoke")
    return candidates[0]


def parse_backends(raw: str) -> list[str]:
    parts = re.split(r"[\s,]+", raw.strip())
    return [p for p in parts if p]


def missing_tool_for_backend(backend: str, stderr: str) -> bool:
    lowered = stderr.lower()
    if backend == "wasm":
        patterns = [
            "wasm-ld",
            "wasm-opt",
            "lld",
            "linker",
            "not found",
            "no such file",
        ]
        return any(p in lowered for p in patterns)
    return False


def compile_backend(fp_bin: Path, backend: str, source: Path, out_path: Path) -> subprocess.CompletedProcess:
    return subprocess.run(
        [
            str(fp_bin),
            "compile",
            "--backend",
            backend,
            "--output",
            str(out_path),
            str(source),
        ],
        check=False,
        cwd=repo_root(),
        capture_output=True,
        text=True,
    )


def test_backend_matrix_smoke() -> None:
    root = repo_root()
    fp_bin = build_fp(root)
    if not fp_bin.exists():
        raise RuntimeError(f"fp binary missing: {fp_bin}")

    backends = parse_backends(os.environ.get("BACKEND_MATRIX_BACKENDS", "bytecode,wasm"))
    if len(backends) < 2:
        raise AssertionError("BACKEND_MATRIX_BACKENDS must include at least two backends")

    source = pick_example(root)

    skipped = []
    succeeded = []
    failures = []

    with tempfile.TemporaryDirectory() as temp_dir:
        out_root = Path(temp_dir)
        for backend in backends:
            ext = ".fbc" if backend == "bytecode" else f".{backend}"
            out_path = out_root / f"{source.stem}_{backend}{ext}"
            result = compile_backend(fp_bin, backend, source, out_path)
            if result.returncode == 0 and out_path.exists():
                succeeded.append(backend)
                continue
            stderr = (result.stderr or "")
            if missing_tool_for_backend(backend, stderr):
                skipped.append(backend)
                continue
            failures.append((backend, stderr.strip() or "compile failed"))

    if failures:
        details = "\n".join([f"{name}: {err}" for name, err in failures])
        raise AssertionError(f"Backend matrix compile failures:\n{details}")

    if len(succeeded) < 2:
        pytest.skip(f"Only {len(succeeded)} backends available; skipped={skipped}")
