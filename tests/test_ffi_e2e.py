import subprocess
from pathlib import Path

import pytest

pytestmark = [pytest.mark.e2e, pytest.mark.risk_high]


def repo_root() -> Path:
    return Path(__file__).resolve().parents[1]


def test_ffi_basics_smoke() -> None:
    root = repo_root()
    script_path = root / "tests" / "e2e" / "ffi_basics_smoke.sh"
    if not script_path.exists():
        raise RuntimeError(f"Missing script: {script_path}")
    subprocess.run(
        ["bash", str(script_path)],
        check=True,
        cwd=root,
    )
