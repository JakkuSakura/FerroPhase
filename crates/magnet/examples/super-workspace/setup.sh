#!/bin/bash
# setup.sh
# Setup script for the magnet super-workspace example

set -e # Exit on any error

# Text formatting
BOLD="\033[1m"
BLUE="\033[34m"
GREEN="\033[32m"
YELLOW="\033[33m"
RESET="\033[0m"

# Helper function to print section headers
header() {
  echo -e "\n${BOLD}${BLUE}==== $1 ====${RESET}\n"
}

# Helper function to explain what's happening
explain() {
  echo -e "${YELLOW}$1${RESET}"
}

# Helper function to show commands as they're run
run_cmd() {
  echo -e "${BOLD}${GREEN}$ $1${RESET}"
  eval "$1"
}

# Determine the path to the magnet CLI tool
MAGNET_PATH="$(which magnet 2>/dev/null || echo "")"
if [ -z "$MAGNET_PATH" ]; then
  if [ -f "../../../target/debug/magnet" ]; then
    MAGNET_PATH="../../../target/debug/magnet"
  elif [ -f "../../../target/release/magnet" ]; then
    MAGNET_PATH="../../../target/release/magnet"
  else
    MAGNET_PATH="cargo run --bin magnet --"
  fi
fi

header "Super-Workspace Example Setup"
explain "This script demonstrates the magnet super-workspace concept"
explain "We'll use magnet to manage dependencies across three separate projects:"
explain "  - shared-lib: Common library with types and utilities"
explain "  - project1: A project that uses the shared library"
explain "  - project2: Another project that uses the shared library"

header "Step 1: Generating Cargo.toml files for shared-lib"
explain "First, let's generate the Cargo.toml for the shared library workspace"
run_cmd "cd shared-lib && $MAGNET_PATH generate"

header "Step 2: Generating Cargo.toml files for project1"
explain "Now, let's generate the Cargo.toml for project1 with path dependencies to shared-lib"
run_cmd "cd project1 && $MAGNET_PATH generate"

header "Step 3: Generating Cargo.toml files for project2"
explain "Finally, let's generate the Cargo.toml for project2 with path dependencies to shared-lib"
run_cmd "cd project2 && $MAGNET_PATH generate"

header "Step 4: Examining the generated files"
explain "Let's look at some of the generated Cargo.toml files to see path dependencies in action"

explain "\nProject 1 API Cargo.toml:"
run_cmd "cat project1/crates/api/Cargo.toml"

explain "\nProject 2 Service Cargo.toml:"
run_cmd "cat project2/crates/service/Cargo.toml"

header "Super-Workspace in Action"
explain "Notice how magnet has automatically set up path dependencies to the shared-lib crates."
explain "This demonstrates the key benefit of the super-workspace concept:"
explain "  - You can have multiple separate projects"
explain "  - They can share code through path dependencies"
explain "  - Dependencies are managed automatically by magnet"
explain "  - Changes to the shared code are immediately available to all projects"

header "Further Exploration"
explain "Try making changes and regenerating the Cargo.toml files:"
explain "1. Add a new dependency to shared-lib/Magnet.toml"
explain "2. Run: cd shared-lib && $MAGNET_PATH generate"
explain "3. Add the same dependency to project1/Magnet.toml and project2/Magnet.toml"
explain "4. Run: cd project1 && $MAGNET_PATH generate"
explain "5. Run: cd project2 && $MAGNET_PATH generate"
explain "6. See how the dependency is now consistently applied across projects"

explain "\nTry checking configurations for consistency:"
explain "Run: cd project1 && $MAGNET_PATH check"
explain "Run: cd project2 && $MAGNET_PATH check"

explain "\nTry listing all crates in a workspace:"
explain "Run: cd project1 && $MAGNET_PATH list"