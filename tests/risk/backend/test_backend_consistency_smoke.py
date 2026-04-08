import os
import shutil
import subprocess
import tempfile
from pathlib import Path

import pytest

pytestmark = [pytest.mark.risk_backend, pytest.mark.risk_min]


def repo_root() -> Path:
    return Path(__file__).resolve().parents[3]


def resolve_fp_bin() -> str | None:
    env_bin = os.environ.get("FP_BIN")
    if env_bin:
        if Path(env_bin).exists():
            return env_bin
    resolved = shutil.which("fp")
    if resolved:
        return resolved
    return None


def compile_backend(fp_bin: str, backend: str, source: Path, out_path: Path) -> None:
    subprocess.run(
        [
            fp_bin,
            "compile",
            "--backend",
            backend,
            "--output",
            str(out_path),
            str(source),
        ],
        check=True,
        cwd=repo_root(),
    )


def test_backend_consistency_smoke() -> None:
    fp_bin = resolve_fp_bin()
    if fp_bin is None:
        pytest.skip("fp binary not found (set FP_BIN or install fp)")

    with tempfile.TemporaryDirectory() as temp_dir:
        temp_root = Path(temp_dir)
        source = temp_root / "main.fp"
        source.write_text("fn main() {}\n", encoding="utf-8")

        out_bytecode = temp_root / "main.fbc"
        compile_backend(fp_bin, "bytecode", source, out_bytecode)
        assert out_bytecode.exists(), "bytecode output missing"

        out_text = temp_root / "main.text.fbc"
        compile_backend(fp_bin, "text-bytecode", source, out_text)
        assert out_text.exists(), "text-bytecode output missing"
