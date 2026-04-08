import shutil
import subprocess
from pathlib import Path

import pytest

pytestmark = [pytest.mark.risk_pipeline, pytest.mark.risk_min]


def repo_root() -> Path:
    return Path(__file__).resolve().parents[3]


def test_pipeline_guard_strict() -> None:
    if shutil.which("rg") is None:
        pytest.skip("rg not available for pipeline guard")

    script_path = repo_root() / "scripts" / "fp_lang_frontend_guard.sh"
    if not script_path.exists():
        raise RuntimeError(f"missing script: {script_path}")

    subprocess.run(
        ["bash", str(script_path), "--strict"],
        check=True,
        cwd=repo_root(),
    )
