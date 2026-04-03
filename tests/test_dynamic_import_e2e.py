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


def write_dynamic_import_sources(workspace_dir: Path) -> Path:
    src_dir = workspace_dir / "src"
    util_path = src_dir / "util.fp"
    util_path.write_text("pub fn ping() {}\n")
    main_path = src_dir / "main.fp"
    main_path.write_text('fn main() { let _ = import("crate::util"); }\n')
    return main_path


def test_dynamic_import_interpret_uses_workspace_graph() -> None:
    root = repo_root()
    build_binaries(root)

    base_env = os.environ.copy()
    magnet_bin = root / "target" / "debug" / "magnet"
    fp_bin = root / "target" / "debug" / "fp"

    with tempfile.TemporaryDirectory() as temp_dir:
        temp_root = Path(temp_dir)
        workspace_dir = copy_example(root, temp_root)
        main_path = write_dynamic_import_sources(workspace_dir)

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

        subprocess.run(
            [
                str(fp_bin),
                "interpret",
                "--graph",
                str(graph_path),
                str(main_path),
            ],
            check=True,
            cwd=root,
            env=base_env,
        )
