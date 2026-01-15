# Magnet CLI Tool

Magnet is a CLI tool for managing Magnet.toml project configuration files, designed to enhance the Cargo workspace experience with a super-workspace concept.

## Features

- **Super-workspace management**: Define workspace members and dependencies in a Magnet.toml file
- **Path-based dependency resolution**: Automatically replace references to crates in the super-workspace with path-based dependencies
- **Cargo.toml generation**: Generate Cargo.toml files from Magnet.toml configurations
- **Workspace validation**: Check for inconsistencies between Magnet.toml and Cargo.toml files

## Installation

```bash
cargo install --path ./crates/magnet
```

## Usage

### Initialize a new Magnet.toml

```bash
magnet init [path]
```

### Initialize Magnet.toml from Cargo.toml

```bash
magnet init --from-cargo [path]
```

### Generate Cargo.toml from Magnet.toml

```bash
magnet generate [--config Magnet.toml]
```

### Check Magnet.toml for issues

```bash
magnet check [--config Magnet.toml]
```

### List all crates in the super-workspace

```bash
magnet list [--config Magnet.toml]
```

### Run a FerroPhase package via fp-cli

```bash
magnet run [path] [--package name] [--entry src/main.fp] [--mode compile|interpret] [--resolver ferrophase] [--example name] [--release] [--profile name] [--build-option key=value]
```

### Run tests

```bash
magnet test [path] [--package name] [--release] [--profile name]
```

## Example Magnet.toml

```toml
[project]
name = "my-project"
version = "0.1.0"
description = "My awesome Rust project"
authors = ["Your Name <your.email@example.com>"]

[workspace]
members = ["crates/*"]
exclude = []
resolver = "2"

# Dependencies shared across the workspace
[dependencies]
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1.0", features = ["full"] }

# Development dependencies shared across the workspace
[dev-dependencies]
criterion = "0.5"

[features]
feature_a = []
feature_b = ["feature_a"]

[build]
features = ["feature_a", "feature_b"]

[build.options]
opt_level = "2"
```

## License

MIT License
