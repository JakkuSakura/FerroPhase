import json
import os
import shutil
import subprocess
import tempfile
from pathlib import Path

_BUILT = False


def repo_root() -> Path:
    return Path(__file__).resolve().parents[1]


def build_binaries(root: Path) -> None:
    global _BUILT
    if _BUILT:
        return
    subprocess.run(
        [
            "cargo",
            "build",
            "-p",
            "magnet",
            "--bin",
            "magnet",
        ],
        check=True,
        cwd=root,
    )
    subprocess.run(
        [
            "cargo",
            "build",
            "-p",
            "fp-cli",
            "--bin",
            "fp",
        ],
        check=True,
        cwd=root,
    )
    _BUILT = True


def copy_example(root: Path, temp_root: Path) -> Path:
    example_dir = root / "crates" / "magnet" / "examples" / "ferrophase"
    workspace_dir = temp_root / "workspace"
    shutil.copytree(example_dir, workspace_dir)
    return workspace_dir


def write_fp_stub(temp_root: Path) -> Path:
    stub_path = temp_root / "fp-stub.sh"
    stub_path.write_text("#!/bin/sh\nexit 0\n")
    os.chmod(stub_path, 0o755)
    return stub_path


def test_workspace_graph_emitted_and_consumed() -> None:
    root = repo_root()
    build_binaries(root)

    base_env = os.environ.copy()
    magnet_bin = root / "target" / "debug" / "magnet"
    fp_bin = root / "target" / "debug" / "fp"

    with tempfile.TemporaryDirectory() as temp_dir:
        temp_root = Path(temp_dir)
        workspace_dir = copy_example(root, temp_root)

        fp_stub = write_fp_stub(temp_root)
        magnet_env = base_env.copy()
        magnet_env["FP_BIN"] = str(fp_stub)

        subprocess.run(
            [
                str(magnet_bin),
                "build",
                str(workspace_dir),
            ],
            check=True,
            cwd=root,
            env=magnet_env,
        )

        graph_path = (
            workspace_dir
            / "target"
            / "magnet"
            / "debug"
            / "build"
            / "workspace-graph.json"
        )
        assert graph_path.exists()

        graph_data = json.loads(graph_path.read_text())
        assert "manifest" in graph_data
        assert "packages" in graph_data
        assert len(graph_data["packages"]) > 0

        input_path = workspace_dir / "src" / "main.fp"
        input_path.write_text("fn main() {}\n")
        output_path = temp_root / "main.rs"

        subprocess.run(
            [
                str(fp_bin),
                "compile",
                "--graph",
                str(graph_path),
                "--target",
                "rust",
                "-o",
                str(output_path),
                str(input_path),
            ],
            check=True,
            cwd=root,
            env=base_env,
        )

        assert output_path.exists()
