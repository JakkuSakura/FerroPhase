#!/usr/bin/env bash

set -euo pipefail

print_usage() {
    cat <<'USAGE'
Usage: scripts/check_fp.sh <path-to-file.fp> [--prompt "custom prompt"]

Verifies a FerroPhase program by comparing interpreter and exec outputs,
generating Rust / C# / TypeScript projections, and optionally delegating a
semantic check to Codex.
USAGE
}

if [[ $# -lt 1 ]]; then
    print_usage >&2
    exit 1
fi

FP_FILE=""
CUSTOM_PROMPT=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --prompt)
            shift
            [[ $# -gt 0 ]] || { echo "--prompt requires an argument" >&2; exit 1; }
            CUSTOM_PROMPT="$1"
            ;;
        --help|-h)
            print_usage
            exit 0
            ;;
        *)
            if [[ -z "$FP_FILE" ]]; then
                FP_FILE="$1"
            else
                echo "Unexpected argument: $1" >&2
                exit 1
            fi
            ;;
    esac
    shift
done

if [[ ! -f "$FP_FILE" ]]; then
    echo "FerroPhase file not found: $FP_FILE" >&2
    exit 1
fi

if ! command -v fp >/dev/null 2>&1; then
    echo "The 'fp' CLI is required but not found in PATH." >&2
    exit 1
fi

if command -v codex >/dev/null 2>&1 && ! command -v jq >/dev/null 2>&1; then
    echo "jq is required for Codex JSON parsing but was not found in PATH." >&2
    exit 1
fi

WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

RUN_OUTPUT="$WORK_DIR/run.out"
EXEC_OUTPUT="$WORK_DIR/exec.out"

echo "→ Running interpreter (fp run)"
if ! fp run "$FP_FILE" >"$RUN_OUTPUT" 2>"$WORK_DIR/run.err"; then
    echo "fp run failed; see $WORK_DIR/run.err" >&2
    exit 1
fi

echo "→ Running exec path (fp exec --quiet --run --save-intermediates)"
if ! fp exec --quiet --run --save-intermediates "$FP_FILE" >"$EXEC_OUTPUT" 2>"$WORK_DIR/exec.err"; then
    echo "fp exec failed; see $WORK_DIR/exec.err" >&2
    exit 1
fi

if ! diff -u "$RUN_OUTPUT" "$EXEC_OUTPUT" >"$WORK_DIR/output.diff"; then
    echo "Interpreter and exec outputs differ:" >&2
    cat "$WORK_DIR/output.diff" >&2
    exit 1
fi
echo "✓ Interpreter and exec outputs match"

echo "→ Transpiling / compiling to Rust, C#, TypeScript"


RUST_OUT="$WORK_DIR/$(basename "$FP_FILE" .fp).rs"
CS_OUT="$WORK_DIR/$(basename "$FP_FILE" .fp).cs"
TS_OUT="$WORK_DIR/$(basename "$FP_FILE" .fp).ts"

if ! fp compile "$FP_FILE" --target rust --output "$RUST_OUT" >/"$WORK_DIR/rust.log" 2>&1; then
    echo "fp compile --target rust failed; see $WORK_DIR/rust.log" >&2
    exit 1
fi

if ! fp compile "$FP_FILE" --target csharp --output "$CS_OUT" >/"$WORK_DIR/csharp.log" 2>&1; then
    echo "fp compile --target csharp failed; see $WORK_DIR/csharp.log" >&2
    exit 1
fi

if ! fp compile "$FP_FILE" --target typescript --output "$TS_OUT" >/"$WORK_DIR/ts.log" 2>&1; then
    echo "fp compile --target typescript failed; see $WORK_DIR/ts.log" >&2
    exit 1
fi

echo "✓ Generated Rust:    $RUST_OUT"
echo "✓ Generated C#:      $CS_OUT"
echo "✓ Generated TypeScript: $TS_OUT"

if command -v codex >/dev/null 2>&1; then
    echo "→ Invoking Codex for semantic verification"
    SOURCE_SNIPPET="$(sed -e 's/^/    /' "$FP_FILE" | head -n 50)"
    RUN_SNIPPET="$(sed -e 's/^/    /' "$RUN_OUTPUT" | head -n 50)"
    RUST_SNIPPET="$(sed -e 's/^/    /' "$RUST_OUT" | head -n 50)"
    CS_SNIPPET="$(sed -e 's/^/    /' "$CS_OUT" | head -n 50)"
    TS_SNIPPET="$(sed -e 's/^/    /' "$TS_OUT" | head -n 50)"

    CODex_OUTPUT="$WORK_DIR/codex.json"

    if [[ -z "$CUSTOM_PROMPT" ]]; then
        CUSTOM_PROMPT="Assess whether the FerroPhase program and derived artifacts are semantically consistent.\nRespond strictly as JSON with the following schema:\n{\n  \"meaningConsistent\": boolean,\n  \"notes\": string,\n  \"issues\": [\n    {\"type\": \"error\" | \"warning\", \"message\": string, \"artifact\": \"source\" | \"output\" | \"rust\" | \"csharp\" | \"typescript\"}\n  ]\n}\nFor each problem, add an entry to issues and prefix every error message with [error]. If there are no issues, return an empty array. Do not include any non-JSON text.\n\nContext:\nSource (.fp):\n$SOURCE_SNIPPET\n\nfp run output:\n$RUN_SNIPPET\n\nGenerated Rust (.rs):\n$RUST_SNIPPET\n\nGenerated C# (.cs):\n$CS_SNIPPET\n\nGenerated TypeScript (.ts):\n$TS_SNIPPET"
    fi

    if ! codex exec "$CUSTOM_PROMPT" >"$CODex_OUTPUT" 2>"$WORK_DIR/codex.err"; then
        echo "codex exec reported an error; see $WORK_DIR/codex.err" >&2
        exit 1
    fi

    if ! jq '.' "$CODex_OUTPUT" >"$WORK_DIR/codex.pretty" 2>"$WORK_DIR/codex_jq.err"; then
        echo "Failed to parse Codex JSON output; see $WORK_DIR/codex_jq.err" >&2
        cat "$CODex_OUTPUT" >&2
        exit 1
    fi

    if grep -q '\[error\]' "$CODex_OUTPUT"; then
        echo "❌ Codex reported error markers:" >&2
        grep '\[error\]' "$CODex_OUTPUT" >&2
        exit 1
    fi

    ERROR_COUNT=$(jq '[.issues[] | select(.type == "error")] | length' "$CODex_OUTPUT")
    if [[ "$ERROR_COUNT" -gt 0 ]]; then
        echo "❌ Codex semantic check found $ERROR_COUNT error(s)." >&2
        jq '.issues[] | select(.type == "error")' "$CODex_OUTPUT" >&2
        exit 1
    fi

    CONSISTENT=$(jq -r '.meaningConsistent' "$CODex_OUTPUT")
    if [[ "$CONSISTENT" != "true" ]]; then
        echo "❌ Codex marked the artifacts as not meaning-consistent." >&2
        jq '.' "$CODex_OUTPUT" >&2
        exit 1
    fi

    echo "✓ Codex semantic verification passed"
else
    echo "⚠️  'codex' CLI not available; skipping semantic verification." >&2
fi

echo "✅ All checks completed successfully"
