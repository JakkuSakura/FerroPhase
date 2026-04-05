import subprocess
from pathlib import Path

import pytest

pytestmark = [pytest.mark.rust_ui, pytest.mark.risk_medium]


def test_rust_ui_case(request, rust_ui_case):
    state = request.config._rust_ui_state
    mode = rust_ui_case["mode"]
    file_path = rust_ui_case["path"]
    rel_path = rust_ui_case["rel_path"]

    rel_slug = rel_path.replace("/", "__")
    log_dir = state["log_dir"]
    out_dir = state["out_dir"]

    log_file = log_dir / f"{mode}__{rel_slug}.log"
    log_file.parent.mkdir(parents=True, exist_ok=True)

    expected = _has_expected_stderr(file_path)
    fp_bin = state["fp_bin"]

    if mode == "parse":
        command = [fp_bin, "parse", "--no-resolve", file_path.as_posix()]
    elif mode == "compile":
        out_path = out_dir / mode
        out_path.mkdir(parents=True, exist_ok=True)
        output_file = out_path / (file_path.stem + ".out")
        command = [
            fp_bin,
            "compile",
            file_path.as_posix(),
            "--output",
            output_file.as_posix(),
        ]
    else:
        _record(state, "skip", f"{mode} {rel_path}")
        pytest.skip(f"unknown mode: {mode}")

    exit_code = _run_and_capture(command, log_file)

    if exit_code == 0:
        if expected:
            _record(state, "allowed_pass", f"{mode} {rel_path}")
        else:
            _record(state, "pass", f"{mode} {rel_path}")
        return

    if expected:
        _record(state, "expected_fail", f"{mode} {rel_path}")
        pytest.xfail("expected failure (.stderr present)")
    else:
        _record(state, "fail", f"{mode} {rel_path}")
        raise AssertionError(f"{mode} failed: {rel_path}")


def _run_and_capture(command, log_file):
    with log_file.open("w", encoding="utf-8") as handle:
        result = subprocess.run(
            command,
            stdout=handle,
            stderr=subprocess.STDOUT,
            check=False,
        )
    return result.returncode


def _has_expected_stderr(file_path):
    base = file_path.with_suffix("")
    matches = []

    direct = Path(str(base) + ".stderr")
    if direct.is_file():
        matches.append(direct)

    glob_matches = list(base.parent.glob(base.name + ".*.stderr"))
    glob_matches.sort(key=lambda p: p.as_posix())
    if len(glob_matches) > 0:
        matches.extend(glob_matches)

    return len(matches) > 0


def _record(state, key, value):
    summary = state["summary"]
    summary[key].append(value)
