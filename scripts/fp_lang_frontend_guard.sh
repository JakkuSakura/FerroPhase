#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/fp_lang_frontend_guard.sh [--strict]

Prints occurrences of "legacy" entrypoints/patterns that should disappear as the
fp-lang frontend is refactored to the canonical pipeline:
  source -> (token/trivia) -> CST -> (filtered tokens) -> AST

In default mode, this script always exits 0 and is safe to run at any time.
With --strict, it exits non-zero if any forbidden patterns are found.

TODO: 如果希望直接 `scripts/fp_lang_frontend_guard.sh` 可执行（而不是 `bash scripts/...`），
请在仓库里为该脚本设置可执行位并在 CI 中复用。
USAGE
}

STRICT=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --strict) STRICT=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

if ! command -v rg >/dev/null 2>&1; then
  echo "Error: ripgrep (rg) is required." >&2
  exit 127
fi

declare -a CHECKS=(
  "crates/fp-lang/src/lib.rs|\\bparse_expr_ast\\("
  "crates/fp-lang/src/lib.rs|\\bparse_items_ast\\("
  "crates/fp-lang/src/lib.rs|\\brewrite_to_rust\\("
  "crates/fp-lang/src/lib.rs|\\bcst_to_source\\("
  "crates/fp-lang/src/parser/mod.rs|\\bcst::parse\\("
  "crates/fp-lang/src/parser/expr.rs|match_keyword\\(input,\\s*Keyword::Emit\\)"
  "crates/fp-lang/src/parser/cst.rs|CstNode::splice\\(vec!\\["
)

found_any=0
echo "fp-lang frontend guard (strict=$STRICT)"
for entry in "${CHECKS[@]}"; do
  file="${entry%%|*}"
  pat="${entry#*|}"

  # Print matches, but do not fail the script unless --strict is passed.
  if rg -n "$pat" "$file" >/dev/null 2>&1; then
    found_any=1
    echo
    echo "==> $file"
    echo "pattern: $pat"
    rg -n "$pat" "$file" || true
  fi
done

echo
if [[ $found_any -eq 0 ]]; then
  echo "OK: no guarded patterns found."
else
  echo "NOTE: guarded patterns found."
  if [[ $STRICT -eq 1 ]]; then
    exit 1
  fi
fi
