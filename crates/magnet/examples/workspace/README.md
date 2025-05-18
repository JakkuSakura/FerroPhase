# Super-Workspace Example

This example demonstrates how to use the `magnet` CLI tool to manage dependencies across multiple separate projects, creating a "super-workspace" that spans project boundaries.

## Project Structure

```
super-workspace/
├── shared-lib/           # Shared library project
│   ├── Magnet.toml       # Workspace-level config
│   └── crates/
│       ├── shared-types/
│       │   └── Magnet.toml   # Crate-specific config
│       └── shared-utils/
├── project1/             # Project that uses shared libraries
│   ├── Magnet.toml       # Uses auto=true to find shared crates
│   └── crates/
│       └── api/
└── project2/             # Another project using shared libraries
    ├── Magnet.toml       # Uses auto=true to find shared crates
    └── crates/
        └── service/
```

## Key Features Demonstrated

### 1. Automatic Path Resolution with `nexus = true`

In the `project1/Magnet.toml` and `project2/Magnet.toml` files, shared dependencies are specified with `nexus = true`:

```toml
[dependencies]
shared-types = { nexus = true }
shared-utils = { nexus = true }
```

This tells `magnet` to:
- Search for these crates across all accessible workspaces
- Calculate the correct relative paths between projects
- Set up path dependencies automatically

### 2. Multi-Level Configuration

This example shows how configuration can be managed at multiple levels:

1. **Workspace level** (`shared-lib/Magnet.toml`):
   - Defines common dependencies for all crates in the workspace
   - Sets workspace-wide options

2. **Crate level** (`shared-lib/crates/shared-types/Magnet.toml`):
   - Overrides or extends workspace configuration
   - Sets crate-specific dependency options
   - Can reference workspace dependencies with `workspace = true`

## Using This Example

### Step 1: Explore the Configuration Files

Examine the Magnet.toml files in each project to understand how dependencies are defined:

- `shared-lib/Magnet.toml`: Base workspace with common dependencies
- `project1/Magnet.toml`: Project using `nexus = true` for shared crates
- `shared-lib/crates/shared-types/Magnet.toml`: Crate-specific configuration

### Step 2: Generate Cargo.toml Files

Run the following commands to see how magnet resolves dependencies automatically:

```bash
# Generate Cargo.toml files for shared-lib
cd shared-lib
magnet generate

# Generate Cargo.toml files for project1
cd ../project1
magnet generate

# Generate Cargo.toml files for project2
cd ../project2
magnet generate
```

### Step 3: Examine the Results

Look at the generated Cargo.toml files to see how path dependencies have been set up:

```bash
# Check how path dependencies are resolved in project1
cat project1/crates/api/Cargo.toml

# Check how path dependencies are resolved in project2
cat project2/crates/service/Cargo.toml
```

### Step 4: Try Modifying Dependencies

To see how changes propagate:

1. Add a new dependency to `shared-lib/Magnet.toml`
2. Run `magnet generate` in the shared-lib directory
3. Run `magnet generate` in project1 and project2 directories
4. See how the changes affect all projects

## Benefits of the Super-Workspace Pattern

- **Decoupled Projects**: Each project maintains its independence
- **Shared Code**: Projects can share code through automatic path dependencies
- **Consistent Dependencies**: Common dependencies are defined and versioned once
- **Automatic Path Resolution**: No need to manually manage paths between projects
- **Hierarchical Configuration**: Control dependencies at both workspace and crate levels

## Advanced Usage

### Customizing Path Detection

You can explicitly define search paths in your Magnet.toml:

```toml
[workspace.search_paths]
shared = "../shared-lib"
```

### Mixing Auto and Explicit Paths

You can mix automatic and explicit path configurations:

```toml
[dependencies]
# Automatically detected
shared-types = { nexus = true }

# Explicitly defined path
special-crate = { path = "../other-project/crates/special" }
```