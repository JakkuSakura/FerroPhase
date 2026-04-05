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


def main() -> int:
    parser = argparse.ArgumentParser(description="IR snapshot hash guard")
    parser.add_argument("--snapshots-dir", required=True)
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

    snapshots_dir = os.path.abspath(args.snapshots_dir)
    baseline_path = os.path.abspath(args.baseline)
    exts = args.exts or DEFAULT_EXTS

    if not os.path.isdir(snapshots_dir):
        print(f"[qa] ERROR: snapshots dir not found: {snapshots_dir}", file=sys.stderr)
        return 1

    hashes = _collect_hashes(snapshots_dir, exts)
    if not hashes:
        print(f"[qa] ERROR: no IR snapshots found under {snapshots_dir}", file=sys.stderr)
        return 1

    if args.update or not os.path.isfile(baseline_path):
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
