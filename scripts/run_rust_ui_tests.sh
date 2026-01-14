#!/usr/bin/env bash

set -euo pipefail

print_usage() {
    cat <<'USAGE'
Usage: scripts/run_rust_ui_tests.sh <mode> [options]

Modes:
  parse    Run `fp parse` on Rust UI tests
  compile  Run `fp compile` on Rust UI tests

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

if [[ "$MODE" != "parse" && "$MODE" != "compile" ]]; then
    echo "Unknown mode: $MODE" >&2
    print_usage >&2
    exit 1
fi

if [[ ! -d "$ROOT" ]]; then
    echo "Test root not found: $ROOT" >&2
    exit 1
fi

if ! command -v "$FP_BIN" >/dev/null 2>&1; then
    echo "fp-cli not found: $FP_BIN" >&2
    exit 1
fi

timestamp="$(date +%Y%m%d_%H%M%S)"
run_dir="${OUTPUT}/${MODE}_${timestamp}"
log_dir="${run_dir}/logs"
out_dir="${run_dir}/out"

mkdir -p "$log_dir" "$out_dir"

list_cmd=(rg --files -g '*.rs' "$ROOT")
mapfile -t files < <("${list_cmd[@]}")

if [[ ${#FILTERS[@]} -gt 0 ]]; then
    filtered=()
    for f in "${files[@]}"; do
        match="true"
        for pat in "${FILTERS[@]}"; do
            if [[ "$f" != *"$pat"* ]]; then
                match="false"
                break
            fi
        done
        if [[ "$match" == "true" ]]; then
            filtered+=("$f")
        fi
    done
    files=("${filtered[@]}")
fi

if [[ -n "$LIMIT" ]]; then
    if [[ "$LIMIT" =~ ^[0-9]+$ ]]; then
        files=("${files[@]:0:$LIMIT}")
    else
        echo "--limit must be a number: $LIMIT" >&2
        exit 1
    fi
fi

if [[ "$DRY_RUN" == "true" ]]; then
    printf "%s\n" "${files[@]}"
    echo "Matched ${#files[@]} test(s)"
    exit 0
fi

total=0
passed=0
failed=0
skipped=0

pass_list=()
fail_list=()
skip_list=()

for f in "${files[@]}"; do
    ((total+=1))
    rel_path="${f#./}"
    base_name="$(basename "$f")"
    log_file="${log_dir}/${base_name}.log"

    case "$MODE" in
        parse)
            # parse does not support import resolution; enforce no-resolve
            if "$FP_BIN" parse --no-resolve "$f" >"$log_file" 2>&1; then
                ((passed+=1))
                pass_list+=("$rel_path")
            else
                ((failed+=1))
                fail_list+=("$rel_path")
            fi
            ;;
        compile)
            out_path="${out_dir}/${base_name%.rs}.out"
            if "$FP_BIN" compile "$f" --output "$out_path" >"$log_file" 2>&1; then
                ((passed+=1))
                pass_list+=("$rel_path")
            else
                ((failed+=1))
                fail_list+=("$rel_path")
            fi
            ;;
        *)
            ((skipped+=1))
            skip_list+=("$rel_path")
            ;;
    esac
done

summary_file="${run_dir}/summary.txt"
{
    echo "mode: $MODE"
    echo "root: $ROOT"
    echo "output: $run_dir"
    echo "total: $total"
    echo "passed: $passed"
    echo "failed: $failed"
    echo "skipped: $skipped"
    echo
    if [[ ${#pass_list[@]} -gt 0 ]]; then
        echo "passed tests:"
        printf "  %s\n" "${pass_list[@]}"
        echo
    fi
    if [[ ${#fail_list[@]} -gt 0 ]]; then
        echo "failed tests:"
        printf "  %s\n" "${fail_list[@]}"
        echo
    fi
    if [[ ${#skip_list[@]} -gt 0 ]]; then
        echo "skipped tests:"
        printf "  %s\n" "${skip_list[@]}"
        echo
    fi
} >"$summary_file"

echo "✅ Done. Summary: $summary_file"
echo "passed: $passed  failed: $failed  skipped: $skipped  total: $total"
if [[ "$failed" -gt 0 ]]; then
    echo "Failures (see logs in $log_dir):"
    printf "  %s\n" "${fail_list[@]}"
    exit 1
fi
