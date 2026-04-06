#!/usr/bin/env bash

set -euo pipefail

print_usage() {
    cat <<'USAGE'
Usage: scripts/run_rust_ui_tests.sh <mode> [options]

Modes:
  parse    Run `fp parse` on Rust UI tests
  compile  Run `fp compile` on Rust UI tests
  both     Run both parse and compile UI tests

Options:
  --root <dir>        Root test directory (default: tests/rust/ui)
  --filter <pattern>  Substring filter on path (repeatable)
  --limit <n>         Limit number of tests processed
  --output <dir>      Output root (default: target/fp-ui)
  --fp <path>         Path to fp binary (default: fp)
  --dry-run           Print matched files without running fp-cli
  --help|-h           Show this help

Examples:
  scripts/run_rust_ui_tests.sh parse --filter async --limit 50
  scripts/run_rust_ui_tests.sh compile --filter borrowck --output target/ui-compile
  scripts/run_rust_ui_tests.sh both --filter borrowck --limit 20
USAGE
}

MODE="${1:-}"
if [[ -z "$MODE" || "$MODE" == "--help" || "$MODE" == "-h" ]]; then
    print_usage
    exit 0
fi
shift

ROOT="tests/rust/ui"
OUTPUT="target/fp-ui"
FP_BIN="fp"
LIMIT=""
DRY_RUN="false"
FILTERS=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        --root)
            shift
            ROOT="${1:-}"
            ;;
        --output)
            shift
            OUTPUT="${1:-}"
            ;;
        --fp)
            shift
            FP_BIN="${1:-}"
            ;;
        --limit)
            shift
            LIMIT="${1:-}"
            ;;
        --filter)
            shift
            FILTERS+=("${1:-}")
            ;;
        --dry-run)
            DRY_RUN="true"
            ;;
        --help|-h)
            print_usage
            exit 0
            ;;
        *)
            echo "Unexpected argument: $1" >&2
            print_usage >&2
            exit 1
            ;;
    esac
    shift
done

if [[ "$MODE" != "parse" && "$MODE" != "compile" && "$MODE" != "both" ]]; then
    echo "Unknown mode: $MODE" >&2
    print_usage >&2
    exit 1
fi

if [[ ! -d "$ROOT" ]]; then
    echo "Test root not found: $ROOT" >&2
    exit 1
fi

if [[ "$DRY_RUN" != "true" ]]; then
    if [[ -x "$FP_BIN" ]]; then
        : # direct path is fine
    elif ! command -v "$FP_BIN" >/dev/null 2>&1; then
        echo "fp-cli not found: $FP_BIN" >&2
        exit 1
    fi
fi

if command -v uv >/dev/null 2>&1; then
    PYTEST_RUNNER=(uv run pytest)
elif command -v pytest >/dev/null 2>&1; then
    PYTEST_RUNNER=(pytest)
elif command -v python3 >/dev/null 2>&1; then
    PYTEST_RUNNER=(python3 -m pytest)
else
    echo "pytest runner not found (uv/pytest/python3)" >&2
    exit 1
fi

PYTEST_ARGS=(-m rust_ui "tests/test_rust_ui.py" --mode "$MODE" --root "$ROOT" --output "$OUTPUT" --fp "$FP_BIN")

if [[ ${#FILTERS[@]} -gt 0 ]]; then
    for pat in "${FILTERS[@]}"; do
        PYTEST_ARGS+=(--filter "$pat")
    done
fi

if [[ -n "$LIMIT" ]]; then
    if [[ "$LIMIT" =~ ^[0-9]+$ ]]; then
        PYTEST_ARGS+=(--limit "$LIMIT")
    else
        echo "--limit must be a number: $LIMIT" >&2
        exit 1
    fi
fi

if [[ "$DRY_RUN" == "true" ]]; then
    PYTEST_ARGS+=(--dry-run)
fi

"${PYTEST_RUNNER[@]}" "${PYTEST_ARGS[@]}"
