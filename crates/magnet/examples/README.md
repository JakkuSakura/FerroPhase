# Magnet CLI Tool Examples

This directory contains examples demonstrating how to use the `magnet` CLI tool for managing dependencies in Rust projects, with a focus on the super-workspace concept and automatic path resolution.

## Key Features Demonstrated

### 1. Automatic Path Resolution with `nexus = true`

The `magnet` tool can automatically detect and resolve path dependencies for crates across workspaces using the `nexus = true` configuration:

```toml
# Example of automatic path resolution
[dependencies]
shared-types = { nexus = true }
```

When `nexus = true` is specified, `magnet` will:
- Search for the specified crate in all workspaces
- Automatically set up the correct relative path dependency
- Update all Cargo.toml files accordingly

### 2. Per-Crate Configuration with Nested Magnet.toml

Each crate within a workspace can have its own `Magnet.toml` configuration:

```
workspace/
├── Magnet.toml             # Workspace-level configuration
├── crates/
│   ├── core/
│   │   ├── Magnet.toml     # Crate-specific configuration
│   │   └── ...
│   └── utils/
│       ├── Magnet.toml     # Crate-specific configuration
│       └── ...
```

This allows for:
- Crate-specific dependency overrides
- Different version requirements per crate
- Granular control of features

### 3. Workspace Dependencies

Crates can reference workspace-defined dependencies using the `workspace = true` attribute:

```toml
# In a crate's Magnet.toml
[dependencies]
serde = { workspace = true } # Use the workspace-defined version
```

## Examples Overview

### Basic Workspace Example

The `basic-workspace/` example shows how to use `magnet` to manage a single workspace with multiple crates:

```
basic-workspace/
├── Magnet.toml           # Root workspace configuration
├── crates/
│   ├── core/             # Core library
│   ├── utils/            # Utility library (with its own Magnet.toml)
│   └── app/              # Example application
```

Key features demonstrated:
- Automatic path resolution between workspace crates
- Per-crate Magnet.toml configuration
- Workspace-level dependency management

### Super Workspace Example

The `super-workspace/` example demonstrates how to manage dependencies across multiple separate projects:

```
super-workspace/
├── shared-lib/           # Shared library project
│   ├── Magnet.toml
│   └── crates/
│       ├── shared-types/
│       └── shared-utils/
├── project1/             # Project that uses shared libraries
│   ├── Magnet.toml
│   └── crates/
│       └── api/
└── project2/             # Another project using shared libraries
    ├── Magnet.toml
    └── crates/
        └── service/
```

Key features demonstrated:
- Auto-detection of crates across projects with `nexus = true`
- Creating a super-workspace spanning multiple projects
- Consistent dependency management across separate codebases

## Using the Examples

### 1. Generating Cargo.toml Files

From any project directory with a `Magnet.toml` file, run:

```bash
# Using cargo run (if magnet is not installed)
cargo run --bin magnet -- generate

# Or using the installed magnet binary
magnet generate
```

This will:
- Read the configuration from `Magnet.toml`
- Automatically detect crates marked with `nexus = true`
- Generate or update all Cargo.toml files with proper path dependencies

### 2. Checking Workspace Consistency

To check for configuration inconsistencies:

```bash
magnet check
```

### 3. Listing Workspace Crates

To list all crates in the workspace:

```bash
magnet list
```

## Key Concepts

### Super-Workspace Pattern

A super-workspace spans multiple separate projects, allowing them to share code while maintaining independent project structures:

1. Each project has its own `Magnet.toml`
2. Projects reference shared crates with `nexus = true`
3. `magnet` automatically sets up path dependencies between projects

### Automatic Path Resolution

When `nexus = true` is specified for a dependency, `magnet` automatically:

1. Searches for the crate in all workspaces it can find
2. Calculates the correct relative path between crates
3. Updates the dependency in Cargo.toml to use this path

### Multi-Level Configuration

`magnet` supports a hierarchical configuration approach:

1. **Workspace-level** (`/Magnet.toml`): Defines shared dependencies
2. **Crate-level** (`/crates/my-crate/Magnet.toml`): Overrides or extends workspace config

## Try It Yourself

1. Examine the `Magnet.toml` files in each example
2. Run `magnet generate` in different directories to see how path dependencies are resolved
3. Add new dependencies and see how they propagate
4. Create your own crate and use `nexus = true` to automatically link it