import platform
import shutil
import subprocess
import sys
from pathlib import Path

import pytest


def repo_root() -> Path:
    return Path(__file__).resolve().parents[1]


def require_tool(name: str) -> None:
    tool_path = shutil.which(name)
    if tool_path is None:
        pytest.skip(f"Missing required tool: {name}")


def require_macos_arm64() -> None:
    if sys.platform != "darwin":
        pytest.skip("Cross-transpile e2e scripts target macOS outputs")
    if platform.machine() != "arm64":
        pytest.skip("Cross-transpile e2e scripts expect arm64 host")


def run_script(script_rel: str) -> None:
    root = repo_root()
    script_path = root / script_rel
    if not script_path.exists():
        raise RuntimeError(f"Missing script: {script_path}")
    subprocess.run(
        ["bash", str(script_path)],
        check=True,
        cwd=root,
    )


def test_cross_transpile_smoke() -> None:
    require_tool("cargo")
    require_tool("rustc")
    require_tool("rustup")
    require_macos_arm64()
    run_script("tests/e2e/run_rust_cross_transpile_smoke.sh")


def test_cross_transpile_windows_import_smoke() -> None:
    require_tool("cargo")
    require_tool("rustc")
    require_tool("rustup")
    require_macos_arm64()
    run_script("tests/e2e/run_rust_cross_transpile_windows_import_smoke.sh")
