import json
import subprocess
from pathlib import Path

import pytest

pytestmark = [pytest.mark.e2e, pytest.mark.risk_medium]

_BUILT = False


def repo_root() -> Path:
    return Path(__file__).resolve().parents[1]


def build_magnet(root: Path) -> Path:
    global _BUILT
    if not _BUILT:
        subprocess.run(
            ["cargo", "build", "-p", "magnet", "--bin", "magnet"],
            check=True,
            cwd=root,
        )
        _BUILT = True
    return root / "target" / "debug" / "magnet"


def magnet_identity(magnet_bin: Path, root: Path, path: Path) -> dict:
    proc = subprocess.run(
        [str(magnet_bin), "identity", str(path)],
        check=True,
        cwd=root,
        capture_output=True,
        text=True,
    )
    return json.loads(proc.stdout)


def write(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)


def test_magnet_identity_prefers_nested_rust_package(tmp_path: Path) -> None:
    root = repo_root()
    magnet_bin = build_magnet(root)

    workspace = tmp_path / "workspace"
    write(
        workspace / "Cargo.toml",
        '[workspace]\nmembers = ["crates/*"]\n',
    )
    write(
        workspace / "crates" / "skln-cli" / "Cargo.toml",
        '[package]\nname = "skln-cli"\nversion = "0.1.0"\nedition = "2024"\n',
    )
    write(workspace / "crates" / "skln-cli" / "src" / "main.rs", "fn main() {}\n")

    identity = magnet_identity(
        magnet_bin,
        root,
        workspace / "crates" / "skln-cli" / "src" / "main.rs",
    )

    primary = identity["candidates"][0]
    assert primary["language"] == "rust"
    assert primary["manifest_kind"] == "package"
    assert primary["name"] == "skln-cli"

    workspace_candidates = [
        candidate
        for candidate in identity["candidates"]
        if candidate["manifest_kind"] == "workspace"
    ]
    assert workspace_candidates


@pytest.mark.parametrize(
    ("layout", "path_parts", "expected_language", "expected_name", "expected_version"),
    [
        (
            {
                "pyproject.toml": (
                    '[project]\nname = "py-demo"\nversion = "0.1.0"\n'
                    'dependencies = ["requests>=2.0"]\n'
                ),
                "src/main.py": "print('demo')\n",
            },
            ("src", "main.py"),
            "python",
            "py-demo",
            "0.1.0",
        ),
        (
            {
                "package.json": '{"name":"js-demo","version":"1.2.3"}',
                "src/index.js": "console.log('demo')\n",
            },
            ("src", "index.js"),
            "javascript",
            "js-demo",
            "1.2.3",
        ),
        (
            {
                "package.json": '{"name":"ts-demo","version":"2.0.0"}',
                "tsconfig.json": '{"compilerOptions": {"target": "ES2022"}}',
                "src/index.ts": "export const value = 1;\n",
            },
            ("src", "index.ts"),
            "typescript",
            "ts-demo",
            "2.0.0",
        ),
        (
            {
                "go.mod": "module example.com/demo\n\ngo 1.22\n",
                "main.go": "package main\nfunc main() {}\n",
            },
            ("main.go",),
            "go",
            "example.com/demo",
            None,
        ),
    ],
)
def test_magnet_identity_parses_multilang_manifests(
    tmp_path: Path,
    layout: dict[str, str],
    path_parts: tuple[str, ...],
    expected_language: str,
    expected_name: str,
    expected_version: str | None,
) -> None:
    root = repo_root()
    magnet_bin = build_magnet(root)

    project = tmp_path / "project"
    for relative, content in layout.items():
        write(project / relative, content)

    identity = magnet_identity(magnet_bin, root, project.joinpath(*path_parts))
    primary = identity["candidates"][0]

    assert primary["language"] == expected_language
    assert primary["manifest_kind"] == "package"
    assert primary["name"] == expected_name
    assert primary["version"] == expected_version
