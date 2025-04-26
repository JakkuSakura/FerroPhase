# Basic Workspace Example

This example demonstrates how to use the `magnet` CLI tool to manage dependencies within a single Cargo workspace, showing automatic path resolution and multi-level configuration.

## Project Structure

```
basic-workspace/
├── Magnet.toml           # Workspace-level configuration
├── crates/
    ├── core/             # Core library crate
    ├── utils/            # Utility library crate
    │   └── Magnet.toml   # Crate-specific configuration
    └── app/              # Application crate
```

## Key Features Demonstrated

### 1. Workspace-Level Auto-Resolution

The workspace Magnet.toml uses `auto = true` to automatically manage path dependencies between crates in the workspace:

```toml
# From basic-workspace/Magnet.toml
[dependencies]
core-crate = { auto = true }
utils-crate = { auto = true }
app-crate = { auto = true }
```

### 2. Crate-Level Configuration

The utils crate has its own Magnet.toml, demonstrating how to:
- Override workspace settings
- Use workspace-defined dependencies
- Add crate-specific dependencies

```toml
# From crates/utils/Magnet.toml
[dependencies]
# Automatically detect and use path dependency for core-crate
core-crate = { auto = true }
# Shared dependencies defined in the workspace
log = { workspace = true }
# Crate-specific dependency configuration
serde_json = { version = "1.0", features = ["preserve_order"] }
```

## Using This Example

### Step 1: Generate Cargo.toml Files

To generate all Cargo.toml files from the Magnet.toml configurations:

```bash
cd basic-workspace
magnet generate
```

This will:
- Read the workspace Magnet.toml and any crate-level Magnet.toml files
- Generate appropriate Cargo.toml files with path dependencies
- Merge workspace and crate-level dependency configurations

### Step 2: Examine the Generated Files

Look at the generated Cargo.toml files to see how the configurations were applied:

```bash
# Check the workspace Cargo.toml
cat Cargo.toml

# Check a crate's Cargo.toml with path dependencies
cat crates/utils/Cargo.toml
```

### Step 3: Try Modifying Dependencies

Experiment with how changes propagate:

1. Add a new dependency to the workspace Magnet.toml
2. Run `magnet generate` again
3. See how all crates now have access to the new dependency

### Step 4: Check Workspace Configuration

Verify that your configuration is consistent:

```bash
magnet check
```

## Benefits of This Approach

- **Centralized Dependency Management**: Define common dependencies once in the workspace Magnet.toml
- **Automatic Path Resolution**: Let magnet handle path dependencies automatically with `auto = true`
- **Crate-Specific Overrides**: Override dependencies at the crate level when needed
- **Inherited Workspace Settings**: Use `workspace = true` to inherit workspace dependency configurations
- **Flexible Structure**: Easily add or remove crates without manual path management