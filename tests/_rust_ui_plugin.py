import shutil
import time
from pathlib import Path

import pytest


def pytest_addoption(parser):
    group = parser.getgroup("rust-ui")
    group.addoption(
        "--rust-ui",
        action="store_true",
        help="Enable Rust UI tests (requires --mode).",
    )
    group.addoption(
        "--mode",
        choices=["parse", "compile", "both"],
        default=None,
        help="Rust UI mode: parse, compile, or both.",
    )
    group.addoption(
        "--root",
        default="tests/rust/ui",
        help="Root test directory (default: tests/rust/ui).",
    )
    group.addoption(
        "--filter",
        action="append",
        default=[],
        help="Substring filter on path (repeatable).",
    )
    group.addoption(
        "--limit",
        type=int,
        default=None,
        help="Limit number of tests processed.",
    )
    group.addoption(
        "--output",
        default="target/fp-ui",
        help="Output root (default: target/fp-ui).",
    )
    group.addoption(
        "--fp",
        default="fp",
        help="Path to fp binary (default: fp).",
    )
    group.addoption(
        "--dry-run",
        action="store_true",
        help="Print matched files without running fp-cli.",
    )


def pytest_configure(config):
    config.addinivalue_line(
        "markers", "rust_ui: Rust UI tests (requires --mode)"
    )

    mode = config.getoption("--mode")
    rust_ui = config.getoption("--rust-ui")
    if mode is None:
        if rust_ui:
            pytest.exit("--mode is required when --rust-ui is set", returncode=2)
        enabled = False
    else:
        enabled = True

    root = Path(config.getoption("--root"))
    filters = config.getoption("--filter")
    limit = config.getoption("--limit")
    output = Path(config.getoption("--output"))
    fp_bin = config.getoption("--fp")
    dry_run = config.getoption("--dry-run")

    state = {
        "enabled": enabled,
        "mode": mode,
        "root": root,
        "filters": filters,
        "limit": limit,
        "output": output,
        "fp_bin": fp_bin,
        "dry_run": dry_run,
        "run_dir": None,
        "log_dir": None,
        "out_dir": None,
        "cases": [],
        "summary": {
            "pass": [],
            "fail": [],
            "expected_fail": [],
            "allowed_pass": [],
            "skip": [],
        },
    }
    config._rust_ui_state = state

    if not enabled:
        return

    if not root.is_dir():
        pytest.exit(f"Test root not found: {root}", returncode=1)

    fp_path = Path(fp_bin)
    if fp_path.exists():
        fp_resolved = fp_path
    else:
        resolved = shutil.which(fp_bin)
        if resolved is None:
            pytest.exit(f"fp-cli not found: {fp_bin}", returncode=1)
        fp_resolved = Path(resolved)
    state["fp_bin"] = str(fp_resolved)

    timestamp = time.strftime("%Y%m%d_%H%M%S")
    run_dir = output / f"{mode}_{timestamp}"
    state["run_dir"] = run_dir
    state["log_dir"] = run_dir / "logs"
    state["out_dir"] = run_dir / "out"

    cases = _collect_cases(root, filters, limit, mode)
    state["cases"] = cases


def pytest_collection_modifyitems(config, items):
    state = config._rust_ui_state
    if state["enabled"]:
        return
    skip_marker = pytest.mark.skip(reason="Rust UI tests require --mode")
    for item in items:
        if item.get_closest_marker("rust_ui") is not None:
            item.add_marker(skip_marker)


def pytest_generate_tests(metafunc):
    if "rust_ui_case" not in metafunc.fixturenames:
        return
    state = metafunc.config._rust_ui_state
    if not state["enabled"]:
        metafunc.parametrize("rust_ui_case", [])
        return
    cases = state["cases"]
    ids = []
    for case in cases:
        ids.append(f"{case['mode']}::{case['rel_path']}")
    metafunc.parametrize("rust_ui_case", cases, ids=ids)


def pytest_sessionstart(session):
    state = session.config._rust_ui_state
    if not state["enabled"]:
        return

    if state["dry_run"]:
        files = _collect_files(state["root"], state["filters"], state["limit"])
        for path in files:
            print(path.as_posix())
        print(f"Matched {len(files)} test(s)")
        pytest.exit("dry-run", returncode=0)

    state["log_dir"].mkdir(parents=True, exist_ok=True)
    state["out_dir"].mkdir(parents=True, exist_ok=True)


def pytest_sessionfinish(session, exitstatus):
    state = session.config._rust_ui_state
    if not state["enabled"]:
        return
    if state["dry_run"]:
        return

    summary_file = state["run_dir"] / "summary.txt"
    summary = state["summary"]
    total = (
        len(summary["pass"])
        + len(summary["fail"])
        + len(summary["expected_fail"])
        + len(summary["allowed_pass"])
        + len(summary["skip"])
    )

    lines = []
    lines.append(f"mode: {state['mode']}")
    lines.append(f"root: {state['root']}")
    lines.append(f"output: {state['run_dir']}")
    lines.append(f"total: {total}")
    lines.append(f"passed: {len(summary['pass'])}")
    lines.append(f"failed: {len(summary['fail'])}")
    lines.append(f"skipped: {len(summary['skip'])}")
    lines.append(f"expected_fail: {len(summary['expected_fail'])}")
    lines.append(f"allowed_pass: {len(summary['allowed_pass'])}")
    lines.append("")

    _append_section(lines, "passed tests:", summary["pass"])
    _append_section(lines, "failed tests:", summary["fail"])
    _append_section(
        lines, "expected-fail tests (.stderr present):", summary["expected_fail"]
    )
    _append_section(
        lines, "allowed-pass tests (.stderr present):", summary["allowed_pass"]
    )
    _append_section(lines, "skipped tests:", summary["skip"])

    summary_file.parent.mkdir(parents=True, exist_ok=True)
    summary_file.write_text("\n".join(lines).rstrip() + "\n", encoding="utf-8")

    print(f"✅ Done. Summary: {summary_file}")
    print(
        "passed: {passed}  failed: {failed}  skipped: {skipped}  expected-fail: {expected}  allowed-pass: {allowed}  total: {total}".format(
            passed=len(summary["pass"]),
            failed=len(summary["fail"]),
            skipped=len(summary["skip"]),
            expected=len(summary["expected_fail"]),
            allowed=len(summary["allowed_pass"]),
            total=total,
        )
    )
    if len(summary["fail"]) > 0:
        print("Failures (see logs in {log_dir}):".format(log_dir=state["log_dir"]))
        for path in summary["fail"]:
            print(f"  {path}")


def _collect_files(root, filters, limit):
    files = []
    for path in root.rglob("*.rs"):
        if not path.is_file():
            continue
        files.append(path)
    files.sort(key=lambda p: p.as_posix())

    if len(filters) > 0:
        filtered = []
        for path in files:
            value = path.as_posix()
            matched = True
            for pattern in filters:
                if pattern not in value:
                    matched = False
                    break
            if matched:
                filtered.append(path)
        files = filtered

    if limit is not None:
        files = files[:limit]

    return files


def _collect_cases(root, filters, limit, mode):
    files = _collect_files(root, filters, limit)
    modes = []
    if mode == "both":
        modes = ["parse", "compile"]
    else:
        modes = [mode]

    cases = []
    for file_path in files:
        rel_path = file_path.as_posix()
        for case_mode in modes:
            cases.append(
                {
                    "path": file_path,
                    "mode": case_mode,
                    "rel_path": rel_path,
                }
            )
    return cases


def _append_section(lines, header, items):
    if len(items) == 0:
        return
    lines.append(header)
    for item in items:
        lines.append(f"  {item}")
    lines.append("")
