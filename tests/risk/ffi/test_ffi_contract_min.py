import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

import pytest

pytestmark = [pytest.mark.risk_ffi, pytest.mark.risk_min]


def repo_root() -> Path:
    return Path(__file__).resolve().parents[3]


def resolve_fp_bin() -> str | None:
    env_bin = os.environ.get("FP_BIN")
    if env_bin and Path(env_bin).exists():
        return env_bin
    resolved = shutil.which("fp")
    if resolved:
        return resolved
    return None


def compile_help(fp_bin: str) -> str:
    result = subprocess.run(
        [fp_bin, "compile", "--help"],
        check=False,
        cwd=repo_root(),
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        raise AssertionError(
            f"fp compile --help failed (code={result.returncode}): {result.stderr.strip()}"
        )
    return result.stdout


def test_bindgen_contract_clang() -> None:
    if shutil.which("bindgen") is None:
        pytest.skip("bindgen not available")

    script = repo_root() / "scripts" / "generate_libc_bindings.py"
    if not script.exists():
        raise RuntimeError(f"missing script: {script}")

    with tempfile.TemporaryDirectory() as temp_dir:
        out_path = Path(temp_dir) / "generated.fp"
        result = subprocess.run(
            [sys.executable, str(script), "--output", str(out_path)],
            check=False,
            cwd=repo_root(),
            capture_output=True,
            text=True,
        )
        if result.returncode != 0:
            stderr = (result.stderr or "").lower()
            if "libclang" in stderr:
                pytest.skip("libclang not available for bindgen")
            raise AssertionError(
                f"bindgen contract failed (code={result.returncode}): {result.stderr.strip()}"
            )
        assert out_path.exists(), "bindgen output missing"


def test_dotnet_backend_contract() -> None:
    fp_bin = resolve_fp_bin()
    if fp_bin is None:
        pytest.skip("fp binary not found (set FP_BIN or install fp)")

    help_text = compile_help(fp_bin)
    assert "cil" in help_text, "expected cil backend in fp compile --help"
    assert "dotnet" in help_text, "expected dotnet backend in fp compile --help"


def test_jvm_backend_contract() -> None:
    fp_bin = resolve_fp_bin()
    if fp_bin is None:
        pytest.skip("fp binary not found (set FP_BIN or install fp)")

    help_text = compile_help(fp_bin)
    assert "jvm-bytecode" in help_text, "expected jvm-bytecode backend in fp compile --help"


def test_python_target_contract() -> None:
    fp_bin = resolve_fp_bin()
    if fp_bin is None:
        pytest.skip("fp binary not found (set FP_BIN or install fp)")

    help_text = compile_help(fp_bin)
    assert "python" in help_text, "expected python target in fp compile --help"
