# FerroPhase: Meta-Compilation Framework with Advanced Const Evaluation

A meta-compilation framework that extends Rust with powerful compile-time computation and metaprogramming capabilities.

## Overview

FerroPhase is a **meta-compilation framework** that enhances Rust with:
- **Rich compile-time introspection** - `sizeof!()`, `hasfield!()`, `field_count!()`
- **Dynamic type generation** - declarative `type T = { ... }` syntax
- **Structural metaprogramming** - conditional fields and type inheritance
- **Multiple compilation targets** - Rust transpiler, interpreter, future LLVM support

## Quick Start

### Installation
```bash
git clone https://github.com/your-org/FerroPhase
cd FerroPhase
cargo build --release
export PATH="$PATH:$(pwd)/target/release"
```

### Create Your First Project
```bash
fp init my-project --template basic
cd my-project
fp compile src/main.fp --target rust --run
```

### Example: Advanced Type Generation
```rust
#!/usr/bin/env fp run

fn main() {
    // Future FerroPhase syntax - parametric type creation
    const fn create_api_handler<const METHODS: &'static [&'static str]>() -> Type {
        type Handler = {
            base_url: String,
            timeout: u64,
            
            // Generate methods based on parameter
            for method in METHODS {
                field!(format!("{}_endpoint", method)): String,
                
                fn method!(method)(&self, path: &str) -> Result<Response, Error> {
                    generate_http_method!(method, self.base_url, path)
                }
            }
            
            // Conditional optimization fields
            if METHODS.len() > 5 {
                connection_pool: ConnectionPool,
                
                fn batch_request(&self, requests: Vec<Request>) -> Vec<Response> {
                    generate_batch_handler!(METHODS)
                }
            }
        };
        Handler
    }
    
    // Generate specialized API handlers
    type RestApi = create_api_handler<&["GET", "POST", "PUT", "DELETE"]>();
    type GraphqlApi = create_api_handler<&["QUERY", "MUTATION", "SUBSCRIPTION"]>();
    
    // Compile-time validation and analysis
    const REST_SIZE: usize = sizeof!(RestApi);
    const GRAPHQL_SIZE: usize = sizeof!(GraphqlApi);
    const REST_HAS_BATCH: bool = hasmethod!(RestApi, "batch_request");
    
    if REST_SIZE > 1024 {
        compile_warning!("REST API handler is getting large");
    }
    
    println!("REST API: {} bytes, batch: {}", REST_SIZE, REST_HAS_BATCH);
    println!("GraphQL API: {} bytes", GRAPHQL_SIZE);
}
```

## Key Features

### Compile-time Introspection
```rust
const SIZE: usize = sizeof!(MyStruct);
const FIELDS: usize = field_count!(MyStruct);
const HAS_FIELD: bool = hasfield!(MyStruct, "name");
```

### Parametric Type Generation
```rust
const fn create_entity<const FEATURES: EntityFeatures>() -> Type {
    type Entity = {
        id: u64,
        created_at: u64,
        
        if FEATURES.has_metadata {
            metadata: HashMap<String, Value>,
        }
        
        if FEATURES.has_permissions {
            permissions: Vec<Permission>,
            
            fn can_access(&self, resource: &str) -> bool {
                generate_permission_check!(resource)
            }
        }
        
        if FEATURES.is_auditable {
            audit_log: Vec<AuditEntry>,
            
            fn log_action(&mut self, action: Action) {
                generate_audit_logger!(action)
            }
        }
    };
    Entity
}

// Generate different entity types
type User = create_entity<EntityFeatures { 
    has_metadata: true, 
    has_permissions: true, 
    is_auditable: false 
}>();

type AdminUser = create_entity<EntityFeatures { 
    has_metadata: true, 
    has_permissions: true, 
    is_auditable: true 
}>();
```

### Advanced Type Composition
```rust
// Type inheritance with conditional extensions
type Employee = {
    ...User,  // Inherit all User fields and methods
    department: String,
    salary: u64,
    
    // Conditional mixins based on department
    if department == "Engineering" {
        ...DeveloperMixin,  // Git access, code review permissions
        github_username: String,
    }
    
    if department == "Management" {
        ...ManagerMixin,    // Team management, budget access
        direct_reports: Vec<UserId>,
        budget_limit: u64,
    }
    
    // Generate department-specific methods
    fn get_role_permissions(&self) -> Vec<Permission> {
        match_department!(self.department, {
            "Engineering" => generate_dev_permissions!(),
            "Management" => generate_mgmt_permissions!(),
            _ => generate_default_permissions!()
        })
    }
};

// Automatic validation
const EMPLOYEE_SIZE: usize = sizeof!(Employee);
const MAX_ROLES: usize = count_conditional_fields!(Employee);

if MAX_ROLES > 10 {
    compile_error!("Too many role variants - consider refactoring");
}
```

## CLI Commands

| Command | Description |
|---------|-------------|
| `fp init <name>` | Create new project |
| `fp compile <file>` | Compile FerroPhase code |
| `fp eval <expr>` | Evaluate expressions |
| `fp check <path>` | Validate code |
| `fp format <files>` | Format code |

### Project Templates
- `basic` - Simple project with examples
- `library` - Library with advanced features
- `binary` - CLI application template

## Project Structure

```
FerroPhase/
├── crates/
│   ├── fp-core/           # Core AST and operations
│   ├── fp-optimize/       # Optimization passes
│   ├── fp-rust-lang/      # Rust frontend
│   └── fp-cli/            # Command-line interface
├── examples/              # Example programs
└── docs/                  # Documentation
```

## Implementation Status

### ✅ Current Features
- Core AST infrastructure
- Const evaluation with 15+ intrinsics
- Rust transpiler
- Professional CLI with project templates
- Declarative type syntax examples

### 🚧 In Progress
- Enhanced parser for macro syntax
- Side effect tracking system
- 3-phase const evaluation

### 📋 Planned
- LLVM backend
- WebAssembly output
- Language server support

## Meta-Compilation Philosophy

**FerroPhase** acts as a meta-compilation layer that:
- **Complements Rust** rather than replacing it
- **Generates clean, idiomatic Rust code** from enhanced syntax
- **Provides compile-time computation** beyond Rust's const capabilities
- **Enables structural metaprogramming** for code generation

## Examples

See `examples/` directory for advanced FerroPhase capabilities:

### 1. Parametric Struct Generation (`05_parametric_structs.fp`)
```rust
// Generate vector types with dimension-based methods
const fn create_vector_type<const DIM: usize, T>() -> Type {
    type Vector = {
        match DIM {
            2 => { x: T, y: T },
            3 => { x: T, y: T, z: T },
            _ => {
                for i in 0..DIM {
                    field!(format!("dim_{}", i)): T,
                }
            }
        }
        
        if DIM >= 2 {
            fn dot(&self, other: &Self) -> T {
                generate_dot_product!(DIM, T)
            }
        }
        
        if DIM == 3 {
            fn cross(&self, other: &Self) -> Self {
                generate_cross_product!(T)
            }
        }
    };
    Vector
}

type Vector3D = create_vector_type<3, f64>();
type Vector8D = create_vector_type<8, f32>();
```

### 2. Generic Specialization (`08_generic_specialization.fp`)
```rust
// Automatic container optimization based on type properties
const fn create_container<T>() -> Type {
    type Container = {
        data: Vec<T>,
        
        if is_copy!(T) && sizeof!(T) <= 8 {
            inline_buffer: [T; 16],  // Small type optimization
        }
        
        if implements!(T, Hash) {
            hash_cache: u64,
            
            fn hash_all(&self) -> u64 {
                generate_hash_aggregator!(T)
            }
        }
        
        if is_numeric!(T) && sizeof!(T) == 4 {
            fn sum_simd(&self) -> T {
                generate_simd_sum!(T)  // SIMD acceleration
            }
        }
    };
    Container
}
```

### 3. Database ORM Generation (`10_metaprogramming_patterns.fp`)
```rust
// Generate complete ORM from schema definition
const SCHEMA = {
    table_name: "users",
    fields: [
        ("id", "u64", "PRIMARY KEY"),
        ("name", "String", "NOT NULL"),
        ("email", "String", "UNIQUE"),
    ]
};

type User = {
    id: u64,
    name: String,
    email: String,
    
    // Auto-generated CRUD methods
    fn save(&self) -> Result<(), DbError> {
        execute_sql!("INSERT INTO users (name, email) VALUES (?, ?)", 
                    self.name, self.email)
    }
    
    fn find_by_id(id: u64) -> Result<User, DbError> {
        query_one!("SELECT * FROM users WHERE id = ?", id)
    }
};
```

Run examples:
```bash
# Direct execution with shebang
./examples/05_parametric_structs.fp
./examples/08_generic_specialization.fp
./examples/10_metaprogramming_patterns.fp

# Or via CLI
fp run examples/05_parametric_structs.fp
fp compile examples/08_generic_specialization.fp --target rust
```

## Contributing

FerroPhase welcomes contributions in:
- Language design and syntax
- Optimization passes
- Type system features
- Tooling and IDE support

## License

[Insert License Here]