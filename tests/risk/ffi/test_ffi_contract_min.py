import os
import shutil
import subprocess
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


def test_libc_codegen_script_contract() -> None:
    script = repo_root() / "scripts" / "codegen_libc.sh"
    assert script.is_file(), f"missing libc codegen script: {script}"
    assert os.access(script, os.X_OK), f"libc codegen script is not executable: {script}"


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
