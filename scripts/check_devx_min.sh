#!/usr/bin/env bash
set -euo pipefail

# Minimal DX checks for Rust projects.
# Optional environment variables to skip checks:
#   SKIP_RA=1          skip rust-analyzer --version check
#   SKIP_CARGO_CHECK=1 skip cargo check -q
#   SKIP_CARGO_FMT=1   skip cargo fmt -- --check
#   SKIP_CARGO_CLIPPY=1 skip cargo clippy -q -D warnings

print_skip() {
  echo "[skip] $1"
}

print_warn() {
  echo "[warn] $1"
}

if [[ "${SKIP_RA:-}" == "1" ]]; then
  print_skip "rust-analyzer --version"
else
  if command -v rust-analyzer >/dev/null 2>&1; then
    rust-analyzer --version
  else
    print_warn "rust-analyzer not found; skipping version check"
  fi
fi

if [[ "${SKIP_CARGO_CHECK:-}" == "1" ]]; then
  print_skip "cargo check -q"
else
  if command -v cargo >/dev/null 2>&1; then
    cargo check -q
  else
    print_warn "cargo not found; skipping cargo check"
  fi
fi

if [[ "${SKIP_CARGO_FMT:-}" == "1" ]]; then
  print_skip "cargo fmt -- --check"
else
  if command -v cargo >/dev/null 2>&1; then
    cargo fmt -- --check
  else
    print_warn "cargo not found; skipping cargo fmt"
  fi
fi

if [[ "${SKIP_CARGO_CLIPPY:-}" == "1" ]]; then
  print_skip "cargo clippy -q -D warnings"
else
  if command -v cargo >/dev/null 2>&1; then
    cargo clippy -q -D warnings
  else
    print_warn "cargo not found; skipping cargo clippy"
  fi
fi
