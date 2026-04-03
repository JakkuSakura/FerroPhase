import subprocess
import tempfile
from pathlib import Path

_BUILT = False


def repo_root() -> Path:
    return Path(__file__).resolve().parents[1]


def build_fp(root: Path) -> None:
    global _BUILT
    if _BUILT:
        return
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


def test_interpret_jit_runs() -> None:
    root = repo_root()
    build_fp(root)

    fp_bin = root / "target" / "debug" / "fp"

    with tempfile.TemporaryDirectory() as temp_dir:
        temp_root = Path(temp_dir)
        input_path = temp_root / "main.fp"
        input_path.write_text("fn main() {}\n")

        subprocess.run(
            [
                str(fp_bin),
                "interpret",
                "--jit",
                "--jit-hot-threshold",
                "1",
                str(input_path),
            ],
            check=True,
            cwd=root,
        )
