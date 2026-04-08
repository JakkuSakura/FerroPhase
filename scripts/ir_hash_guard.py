#!/usr/bin/env python3
import argparse
import hashlib
import json
import os
import sys
from datetime import datetime

DEFAULT_EXTS = [
    ".ast",
    ".ast-typed",
    ".ast-eval",
    ".hir",
    ".mir",
    ".lir",
]


def _normalize_bytes(data: bytes) -> bytes:
    return data.replace(b"\r\n", b"\n")


def _hash_file(path: str) -> str:
    with open(path, "rb") as f:
        data = _normalize_bytes(f.read())
    h = hashlib.sha256()
    h.update(data)
    return h.hexdigest()


def _collect_hashes(root: str, exts: list[str]) -> dict[str, str]:
    hashes: dict[str, str] = {}
    for dirpath, _, filenames in os.walk(root):
        for name in filenames:
            if not any(name.endswith(ext) for ext in exts):
                continue
            abs_path = os.path.join(dirpath, name)
            rel_path = os.path.relpath(abs_path, root)
            hashes[rel_path] = _hash_file(abs_path)
    return dict(sorted(hashes.items()))


def _load_baseline(path: str) -> dict:
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def _write_baseline(path: str, data: dict) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2, sort_keys=True, ensure_ascii=True)
        f.write("\n")


def _diff_hashes(expected: dict[str, str], actual: dict[str, str]) -> dict[str, list[str]]:
    missing = [k for k in expected.keys() if k not in actual]
    extra = [k for k in actual.keys() if k not in expected]
    changed = [k for k in expected.keys() if k in actual and expected[k] != actual[k]]
    return {
        "missing": missing,
        "extra": extra,
        "changed": changed,
    }


def _parse_backends(raw: str | None) -> list[str]:
    if not raw:
        return []
    items = [item.strip() for item in raw.split(",")]
    return [item for item in items if item]


def _collect_multi(root: str, backends: list[str], exts: list[str]) -> dict[str, dict[str, str]]:
    backend_hashes: dict[str, dict[str, str]] = {}
    for backend in backends:
        backend_dir = os.path.join(root, backend)
        if not os.path.isdir(backend_dir):
            print(f"[qa] ERROR: snapshots dir not found: {backend_dir}", file=sys.stderr)
            return {}
        hashes = _collect_hashes(backend_dir, exts)
        if not hashes:
            print(f"[qa] ERROR: no IR snapshots found under {backend_dir}", file=sys.stderr)
            return {}
        backend_hashes[backend] = hashes
    return backend_hashes


def _diff_multi(
    expected: dict[str, dict[str, str]],
    actual: dict[str, dict[str, str]],
) -> tuple[list[str], list[str], dict[str, dict[str, list[str]]]]:
    missing_backends = [b for b in expected.keys() if b not in actual]
    extra_backends = [b for b in actual.keys() if b not in expected]
    diffs: dict[str, dict[str, list[str]]] = {}
    for backend, expected_hashes in expected.items():
        actual_hashes = actual.get(backend)
        if actual_hashes is None:
            continue
        diffs[backend] = _diff_hashes(expected_hashes, actual_hashes)
    return missing_backends, extra_backends, diffs


def main() -> int:
    parser = argparse.ArgumentParser(description="IR snapshot hash guard")
    parser.add_argument("--snapshots-dir")
    parser.add_argument("--snapshots-root")
    parser.add_argument("--backends", help="comma-separated backend list")
    parser.add_argument("--baseline", required=True)
    parser.add_argument("--update", action="store_true", help="update baseline")
    parser.add_argument(
        "--ext",
        action="append",
        dest="exts",
        default=[],
        help="file extension to include (repeatable)",
    )
    args = parser.parse_args()

    snapshots_dir = os.path.abspath(args.snapshots_dir) if args.snapshots_dir else None
    snapshots_root = os.path.abspath(args.snapshots_root) if args.snapshots_root else None
    baseline_path = os.path.abspath(args.baseline)
    exts = args.exts or DEFAULT_EXTS

    if args.update and os.getenv("UPDATE_IR_HASHES") != "1":
        print("[qa] ERROR: UPDATE_IR_HASHES=1 is required to update baseline", file=sys.stderr)
        return 1

    if snapshots_root and snapshots_dir:
        print("[qa] ERROR: use either --snapshots-dir or --snapshots-root", file=sys.stderr)
        return 1

    if snapshots_root:
        backends = _parse_backends(args.backends)
        if not backends:
            print("[qa] ERROR: --backends is required with --snapshots-root", file=sys.stderr)
            return 1
        if not os.path.isdir(snapshots_root):
            print(f"[qa] ERROR: snapshots root not found: {snapshots_root}", file=sys.stderr)
            return 1
        backend_hashes = _collect_multi(snapshots_root, backends, exts)
        if not backend_hashes:
            return 1
        if args.update:
            payload = {
                "version": 2,
                "generated_at": datetime.utcnow().strftime("%Y-%m-%dT%H:%M:%SZ"),
                "snapshots_root": os.path.relpath(snapshots_root, os.getcwd()),
                "extensions": exts,
                "backends": {k: {"hashes": v} for k, v in backend_hashes.items()},
            }
            _write_baseline(baseline_path, payload)
            print(f"[qa] baseline written: {baseline_path}")
            return 0
        if not os.path.isfile(baseline_path):
            print(f"[qa] ERROR: baseline not found: {baseline_path}", file=sys.stderr)
            print("[qa] hint: set UPDATE_IR_HASHES=1 to create baseline", file=sys.stderr)
            return 1
        baseline = _load_baseline(baseline_path)
        expected_backends = baseline.get("backends")
        if not isinstance(expected_backends, dict):
            print("[qa] ERROR: baseline missing backends section", file=sys.stderr)
            return 1
        expected_hashes = {k: v.get("hashes", {}) for k, v in expected_backends.items()}
        missing_backends, extra_backends, diffs = _diff_multi(expected_hashes, backend_hashes)
        has_diff = False
        if missing_backends:
            has_diff = True
            print("[qa] missing backends:", file=sys.stderr)
            for backend in missing_backends:
                print(f"  - {backend}", file=sys.stderr)
        if extra_backends:
            has_diff = True
            print("[qa] extra backends:", file=sys.stderr)
            for backend in extra_backends:
                print(f"  - {backend}", file=sys.stderr)
        for backend, diff in diffs.items():
            if diff["missing"] or diff["extra"] or diff["changed"]:
                has_diff = True
                print(f"[qa] IR snapshot hash mismatch detected ({backend})", file=sys.stderr)
                if diff["missing"]:
                    print("[qa] missing:", file=sys.stderr)
                    for item in diff["missing"]:
                        print(f"  - {item}", file=sys.stderr)
                if diff["extra"]:
                    print("[qa] extra:", file=sys.stderr)
                    for item in diff["extra"]:
                        print(f"  - {item}", file=sys.stderr)
                if diff["changed"]:
                    print("[qa] changed:", file=sys.stderr)
                    for item in diff["changed"]:
                        print(f"  - {item}", file=sys.stderr)
        if has_diff:
            print("[qa] hint: set UPDATE_IR_HASHES=1 to refresh baseline", file=sys.stderr)
            return 1
        print("[qa] IR snapshot hashes OK")
        return 0

    if not snapshots_dir:
        print("[qa] ERROR: --snapshots-dir is required", file=sys.stderr)
        return 1

    if not os.path.isdir(snapshots_dir):
        print(f"[qa] ERROR: snapshots dir not found: {snapshots_dir}", file=sys.stderr)
        return 1

    hashes = _collect_hashes(snapshots_dir, exts)
    if not hashes:
        print(f"[qa] ERROR: no IR snapshots found under {snapshots_dir}", file=sys.stderr)
        return 1

    if args.update:
        payload = {
            "version": 1,
            "generated_at": datetime.utcnow().strftime("%Y-%m-%dT%H:%M:%SZ"),
            "snapshots_dir": os.path.relpath(snapshots_dir, os.getcwd()),
            "extensions": exts,
            "hashes": hashes,
        }
        _write_baseline(baseline_path, payload)
        print(f"[qa] baseline written: {baseline_path}")
        return 0

    if not os.path.isfile(baseline_path):
        print(f"[qa] ERROR: baseline not found: {baseline_path}", file=sys.stderr)
        print("[qa] hint: set UPDATE_IR_HASHES=1 to create baseline", file=sys.stderr)
        return 1

    baseline = _load_baseline(baseline_path)
    expected = baseline.get("hashes", {})
    diff = _diff_hashes(expected, hashes)

    if diff["missing"] or diff["extra"] or diff["changed"]:
        print("[qa] IR snapshot hash mismatch detected", file=sys.stderr)
        if diff["missing"]:
            print("[qa] missing:", file=sys.stderr)
            for item in diff["missing"]:
                print(f"  - {item}", file=sys.stderr)
        if diff["extra"]:
            print("[qa] extra:", file=sys.stderr)
            for item in diff["extra"]:
                print(f"  - {item}", file=sys.stderr)
        if diff["changed"]:
            print("[qa] changed:", file=sys.stderr)
            for item in diff["changed"]:
                print(f"  - {item}", file=sys.stderr)
        print("[qa] hint: set UPDATE_IR_HASHES=1 to refresh baseline", file=sys.stderr)
        return 1

    print("[qa] IR snapshot hashes OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
