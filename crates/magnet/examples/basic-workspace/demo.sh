#!/bin/bash
# demo.sh
# Demonstration script for the magnet basic workspace example

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

header "Basic Workspace Example Demo"
explain "This script demonstrates magnet for managing a single Cargo workspace"
explain "We'll use magnet to generate consistent Cargo.toml files from Magnet.toml"

header "Step 1: Examining the project structure"
explain "The project has three crates:"
explain "  - core: A core library with basic functionality"
explain "  - utils: A utility library that depends on core"
explain "  - app: An application that depends on both core and utils"

run_cmd "ls -la crates/"

header "Step 2: Looking at Magnet.toml"
explain "Let's examine the Magnet.toml configuration file that defines our workspace:"
run_cmd "cat Magnet.toml"

header "Step 3: Generating Cargo.toml files"
explain "Now we'll use magnet to generate all Cargo.toml files from the Magnet.toml configuration:"
run_cmd "$MAGNET_PATH generate"

header "Step 4: Examining generated files"
explain "\nRoot Cargo.toml (workspace definition):"
run_cmd "cat Cargo.toml"

explain "\nCore crate Cargo.toml:"
run_cmd "cat crates/core/Cargo.toml"

explain "\nUtils crate Cargo.toml:"
run_cmd "cat crates/utils/Cargo.toml"

explain "\nApp crate Cargo.toml:"
run_cmd "cat crates/app/Cargo.toml"

header "Step 5: Checking workspace consistency"
explain "Let's check if our workspace is configured consistently:"
run_cmd "$MAGNET_PATH check"

header "Step 6: Listing all crates in the workspace"
explain "Let's list all crates that magnet found in our workspace:"
run_cmd "$MAGNET_PATH list"

header "Demo Complete"
explain "The magnet tool simplifies Cargo workspace management by:"
explain "  - Centralizing dependency definitions in one file (Magnet.toml)"
explain "  - Automatically generating consistent Cargo.toml files"
explain "  - Ensuring dependencies are synchronized across crates"
explain "  - Providing tools to check and verify workspace configuration"

explain "\nTry experimenting with it yourself:"
explain "1. Edit Magnet.toml to add or change dependencies"
explain "2. Run '$MAGNET_PATH generate' to update all Cargo.toml files"
explain "3. Run '$MAGNET_PATH check' to verify consistency"