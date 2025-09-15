# FerroPhase: Meta-Compilation Framework with Multi-Language Comptime Superpowers

**TL;DR**: FerroPhase = "Rust++ with multi-language comptime superpowers and flexible compilation targets"

A unified compilation infrastructure that extends Rust's capabilities while supporting multi-language interoperability and advanced compile-time computation.

## 🎯 Primary Vision

FerroPhase is a **meta-compilation framework** designed for performance-critical domains where you need:
- **Rust's performance and safety** as the foundation
- **Rich expressiveness** from multiple languages (Python algorithms, JS DSLs)
- **Advanced compile-time computation** for code generation and optimization
- **Flexible deployment options** across different targets and use cases

## 🚀 Key Design Goals

### 1. Multi-Language Frontend
- **Rust-with-extensions** as the primary language with enhanced type system
- **Additional language parsers** (Python, JavaScript, others) for polyglot development
- **Unified AST representation** enabling cross-language optimization and analysis
- **Seamless language interoperability** within the same project

```rust
// FerroPhase: Mix languages for their strengths
fn optimize_trading_strategy() {
    // Use Python for algorithm expressiveness
    let strategy = python! {
        import numpy as np
        def calculate_signals(prices):
            return np.gradient(prices) > threshold
    };
    
    // Use Rust for performance-critical execution
    let execution_engine = rust! {
        struct HighFrequencyEngine {
            latency_target: Duration::from_nanos(100),
        }
    };
    
    // Compile-time fusion and optimization
    @optimize_for_latency(strategy, execution_engine)
}
```

### 2. Enhanced Comptime System
- **Rich compile-time evaluation** beyond Rust's const capabilities
- **Cross-language comptime computation** (Python code executing at Rust compile-time)
- **Advanced metaprogramming** with structural type manipulation
- **Dynamic code generation** based on compile-time analysis

```rust
// Compile-time algorithm analysis and specialization
const ALGORITHM_PROFILE = comptime python! {
    # Analyze algorithm complexity at compile time
    import ast
    def analyze_complexity(code):
        # Return optimization hints
        return {"vectorizable": True, "memory_pattern": "sequential"}
};

// Generate specialized implementations
type OptimizedImpl = comptime {
    if ALGORITHM_PROFILE.vectorizable {
        @generate_simd_version()
    } else {
        @generate_scalar_version()
    }
};
```

### 3. Flexible Backend Targets
- **Rust transpilation** for seamless integration with existing Rust ecosystems
- **Direct interpretation** for REPL-style development and rapid prototyping
- **LLVM compilation** for performance-critical applications and custom targets
- **WebAssembly output** for web and edge deployment

### 4. High-Level Cross-Language Optimization
- **Cross-language dead code elimination** removing unused code across language boundaries
- **Compile-time constant propagation** across different source languages
- **Inlining and specialization** across language barriers
- **Unified optimization pipeline** treating all code as a single compilation unit

## 🎮 Target Use Cases

### Performance-Critical Domains
**Crypto Trading Platforms**
```rust
// Express trading logic in Python, compile to microsecond-latency Rust
comptime python! {
    def define_strategy():
        # Complex mathematical models
        return SignalGenerator(rsi_threshold=70, macd_crossover=True)
}

// Auto-generated high-frequency trading engine
@ultra_low_latency  // <100ns execution time
struct TradingEngine = comptime @optimize_for_latency(define_strategy());
```

**Scientific Computing**
```rust
// NumPy-style expressiveness, Rust performance
let simulation = comptime {
    let python_model = python! {
        # Define complex physics simulation
        import numpy as np
        def molecular_dynamics(particles, timestep):
            # Complex scientific computation
    };
    
    // Compile to vectorized Rust with SIMD
    @vectorize_with_simd(python_model)
};
```

**Systems Programming with High-Level APIs**
```rust
// Express complex networking logic, compile to efficient async Rust
comptime {
    let protocol = javascript! {
        // Use JS for DSL expressiveness
        const protocol = {
            handshake: ["HELLO", "ACK", "READY"],
            messages: { broadcast: "all", unicast: "one" }
        };
    };
    
    @generate_async_rust_implementation(protocol)
}
```

## 🏗️ Architecture Overview

### Compilation Pipeline
```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────────┐
│ Multi-Language  │    │ Unified AST &    │    │ Flexible Backend    │
│ Frontend        │───▶│ Cross-Language   │───▶│ Targets             │
│                 │    │ Optimization     │    │                     │
│ • Rust++        │    │                  │    │ • Rust Transpiler   │
│ • Python        │    │ • Dead Code      │    │ • LLVM Compiler     │
│ • JavaScript    │    │ • Const Prop     │    │ • Interpreter       │
│ • Custom DSLs   │    │ • Specialization │    │ • WebAssembly       │
└─────────────────┘    └──────────────────┘    └─────────────────────┘
```

### Enhanced Type System (TypeScript-inspired)
```rust
// Structural typing with rich compile-time computation
type ApiEndpoints = struct {
    "/users": { method: "GET" | "POST", auth: Required },
    "/data": { method: "GET", auth: Optional },
};

// Type-level computation
type HttpClient<T> = comptime {
    let mut client_type = struct {};
    
    for (path, config) in @reflect_fields(T) {
        if config.auth == Required {
            @addfield(client_type, "auth_token", String);
        }
        
        let method_name = @snake_case(path);
        @addmethod(client_type, method_name, @generate_http_method(config));
    }
    
    client_type
};

// Generates fully typed, optimized HTTP client at compile time
type MyClient = HttpClient<ApiEndpoints>;
```

### Comptime Intrinsics for Metaprogramming
```rust
// Rich set of compile-time intrinsics
@sizeof(Type)              // Get type size
@reflect_fields(Struct)    // Inspect struct fields
@create_struct(name)       // Dynamic struct creation
@addfield(struct, name, type)  // Add fields dynamically
@addmethod(struct, name, impl) // Add methods dynamically
@optimize_for_latency()    // Latency-optimized codegen
@vectorize_with_simd()     // SIMD vectorization
@generate_async_version()  // Async/await transformation
```

## 🛠️ Name Origin & Philosophy

**FerroPhase** combines:
- **Ferro-**: From Latin "ferrum" (iron), connecting to Rust's foundation
- **-Phase**: Representing the staged compilation process that transforms high-level abstractions

This reflects our philosophy: **complement Rust rather than replace it**. FerroPhase acts as a sophisticated preprocessing phase that generates clean, idiomatic Rust code while providing much richer expressiveness and multi-language capabilities.

## 📁 Project Structure

```
FerroPhase/
├── crates/
│   ├── fp-core/              # Core AST and language infrastructure
│   │   ├── src/ast/          # Abstract syntax tree definitions
│   │   ├── src/ops/          # Built-in operations and intrinsics
│   │   └── src/context.rs    # Evaluation context management
│   ├── fp-optimize/          # Optimization passes and interpreter
│   │   ├── src/pass/         # Optimization pass implementations
│   │   ├── src/interpreter.rs # Expression interpreter
│   │   └── tests/            # Comprehensive optimization tests
│   ├── fp-rust-lang/         # Rust language frontend
│   │   ├── src/parser/       # Rust syntax parsing
│   │   └── src/printer/      # Rust code generation
│   ├── fp-cli/               # Command-line interface ⭐ NEW!
│   │   ├── src/bin/fp.rs     # Main CLI binary
│   │   ├── src/commands/     # Command implementations
│   │   ├── src/project.rs    # Project management
│   │   └── tests/            # CLI integration tests
│   ├── fp-javascript/        # JavaScript/TypeScript frontend
│   ├── fp-mips/              # MIPS backend (example)
│   └── fp-rust-macro/        # Rust macro utilities
├── examples/                 # Example FerroPhase programs
├── docs/                     # Documentation and design documents
└── README.md                 # This file
```

## 🔧 Current Implementation Status

### ✅ Completed (Phase 1: Foundation)
- **🎯 Complete CLI Interface**: Professional `fp` binary with all major commands
- **🏗️ Core AST Infrastructure**: Unified representation for multi-language constructs
- **⚡ Const Evaluation Engine**: Rich compile-time computation with 15+ intrinsics
- **🔄 Optimization Pipeline**: Specialization, inlining, and const evaluation passes
- **📝 Rust Frontend**: Enhanced Rust parser with type extensions
- **🎨 Project Templates**: 4 project templates (basic, library, binary, multi-lang)
- **⚙️ Configuration System**: Comprehensive project and CLI configuration
- **📊 Progress Tracking**: Rich terminal output with progress bars and colors

#### Implemented Const Evaluation Intrinsics
```rust
@sizeof(Type)              // Get type size in bytes
@create_struct(name)       // Dynamic struct creation
@addfield(struct, name, type) // Add fields to structs
@reflect_fields(Type)      // Inspect struct fields  
@hasfield(struct, name)    // Check field existence
@field_count(struct)       // Get number of fields
@field_type(struct, name)  // Get field type
@struct_size(struct)       // Get total struct size
@clone_struct(type)        // Clone struct definition
@type_name(Type)           // Get type name as string
@compile_error(msg)        // Compile-time errors
@compile_warning(msg)      // Compile-time warnings
@generate_method(...)      // Method generation
@hasmethod(struct, name)   // Method existence checking
// + more intrinsics for metaprogramming
```

#### CLI Commands Available
```bash
fp init <name>             # Create new projects with templates
fp compile <file>          # Compile with multiple targets & optimization
fp eval <expr>             # Interactive expression evaluation
fp check <path>            # Code validation and linting
fp repl                    # Interactive REPL (framework ready)
fp optimize <file>         # Run optimization passes
fp format <files>          # Code formatting
fp lsp                     # Language server (framework ready)
fp info                    # System and build information
fp completions <shell>     # Shell completion generation
```

### 🚧 In Progress (Phase 2: Multi-Language & Polish)
- **🐍 Python Integration**: Compile-time Python execution for algorithms
- **📜 JavaScript Integration**: Configuration DSLs and compile-time JS
- **🔍 @ Symbol Parsing**: Enhanced parser for intrinsic syntax (`@sizeof`)
- **🎭 Side Effect Tracking**: Comprehensive code generation system
- **📚 Language Server**: IDE integration with IntelliSense
- **🏃 Interactive REPL**: Full-featured read-eval-print loop

### 📋 Planned (Phase 3: Advanced Features)
- **🔗 Advanced Type System**: Structural typing, unions, intersections
- **⚡ LLVM Backend**: Direct compilation for maximum performance  
- **🌐 WebAssembly Output**: Web and edge deployment targets
- **📦 Package System**: Multi-language dependency management
- **🏢 Enterprise Features**: Large-scale project support with incremental compilation

## 🚀 Getting Started

### Installation

#### From Source
```bash
git clone https://github.com/your-org/FerroPhase
cd FerroPhase
cargo build --release

# The fp binary will be available at:
# ./target/release/fp
```

#### Add to PATH (recommended)
```bash
# Add to your shell profile (.bashrc, .zshrc, etc.)
export PATH="$PATH:/path/to/FerroPhase/target/release"
```

### Quick Start

#### 1. Create Your First Project
```bash
# Initialize a new FerroPhase project
fp init my-project --template basic
cd my-project

# See what was created
ls -la
# Output:
# src/main.fp          # Main source file
# Ferrophase.toml      # Project configuration  
# README.md           # Project documentation
```

#### 2. Explore the Generated Code
```rust
// src/main.fp - Generated by FerroPhase CLI
fn main() {
    println!("Hello from FerroPhase!");
    println!("Project: {}", PROJECT_NAME);
    
    // Demonstrate const evaluation
    const GREETING = comptime {
        let hour = 14; // Would be actual time in real implementation
        if hour < 12 {
            "Good morning"
        } else if hour < 18 {
            "Good afternoon"
        } else {
            "Good evening"
        }
    };
    
    println!("{}, FerroPhase!", GREETING);
}

const PROJECT_NAME: &str = "my-project";
```

#### 3. Compile and Run
```bash
# Compile to Rust and run immediately
fp compile src/main.fp --target rust --run
# Output: 
# Hello from FerroPhase!
# Project: my-project
# Good afternoon, FerroPhase!

# Or compile with optimization and watch for changes
fp compile src/main.fp --target rust --opt-level 3 --watch
```

#### 4. Interactive Development
```bash
# Evaluate expressions directly
fp eval --expr "1 + 2 * 3"
# Output: Result: 7

# Evaluate from file with AST inspection  
fp eval --file src/main.fp --print-ast

# Start interactive REPL (coming soon)
fp repl
```

### Advanced Examples

#### Multi-Language Project Template
```bash
# Create a project with Python and JavaScript integration
fp init trading-system --template multi-lang
cd trading-system
```

This generates a sophisticated project showcasing FerroPhase's multi-language capabilities:

```rust
// src/main.fp - Multi-language demonstration
fn main() {
    // Python-powered compile-time computation
    const ANALYSIS_RESULT = comptime python! {
        import math
        def analyze_data():
            # Complex analysis that would be tedious in Rust
            data = [1, 2, 3, 4, 5]
            mean = sum(data) / len(data)
            std_dev = math.sqrt(sum((x - mean) ** 2 for x in data) / len(data))
            return {
                "mean": mean,
                "std_dev": std_dev,
                "optimization_hint": "vectorize" if len(data) > 100 else "scalar"
            }
        analyze_data()
    };
    
    // JavaScript-powered configuration DSL
    const API_CONFIG = comptime javascript! {
        const config = {
            endpoints: {
                "/users": { method: "GET", auth: true },
                "/posts": { method: ["GET", "POST"], auth: false },
                "/admin": { method: "*", auth: true, admin: true }
            },
            middleware: ["cors", "logging", "auth"]
        };
        
        // Validate configuration at compile time
        Object.keys(config.endpoints).forEach(path => {
            if (!path.startsWith("/")) {
                throw new Error(`Invalid endpoint path: ${path}`);
            }
        });
        
        config;
    };
    
    // Generate optimized code based on compile-time analysis
    let processor = comptime {
        if ANALYSIS_RESULT.optimization_hint == "vectorize" {
            @generate_simd_version()
        } else {
            @generate_scalar_version()
        }
    };
    
    println!("Analysis result: mean = {}", ANALYSIS_RESULT.mean);
    println!("API endpoints: {}", API_CONFIG.endpoints.len());
    processor.run();
}
```

#### Library Project Template
```bash
# Create a library with advanced const evaluation features
fp init math-utils --template library
cd math-utils
```

This creates a library demonstrating compile-time type manipulation:

```rust
// src/lib.fp - Advanced type-level computation
pub mod core {
    /// Compile-time type generation based on input characteristics
    pub type ComputedType<T> = comptime {
        let mut result_type = struct {};
        
        // Add fields based on type characteristics
        @addfield(result_type, "data", T);
        @addfield(result_type, "metadata", TypeMetadata<T>);
        
        // Conditional fields based on type properties
        if @sizeof(T) <= 8 {
            @addfield(result_type, "inline_buffer", [T; 4]);
        }
        
        result_type
    };
    
    /// Type metadata computed at compile time
    pub type TypeMetadata<T> = struct {
        name: &'static str,
        size: usize,
        alignment: usize,
    };
    
    /// Create instances with rich compile-time metadata
    pub fn create_with_metadata<T>(value: T) -> ComputedType<T> {
        ComputedType {
            data: value,
            metadata: TypeMetadata {
                name: @type_name(T),
                size: @sizeof(T),
                alignment: @alignof(T),
            },
            // inline_buffer populated if T is small enough
        }
    }
}
```

### CLI Command Reference

| Command | Description | Example |
|---------|-------------|---------|
| `fp init <name>` | Create new project | `fp init my-app --template basic` |
| `fp compile <file>` | Compile FerroPhase code | `fp compile src/main.fp --target rust --run` |
| `fp eval <expr>` | Evaluate expressions | `fp eval --expr "1 + 2 * 3"` |
| `fp check <path>` | Validate code | `fp check src/ --include "**/*.fp"` |
| `fp repl` | Interactive REPL | `fp repl --multiline` |
| `fp format <files>` | Format code | `fp format src/**/*.fp --in-place` |
| `fp info` | System information | `fp info --all` |

#### Project Templates

| Template | Description | Use Case |
|----------|-------------|----------|
| `basic` | Simple project with main.fp | Learning, small utilities |
| `library` | Library with tests and docs | Reusable components |
| `binary` | Binary with configuration | CLI tools, applications |
| `multi-lang` | Python/JS integration | Complex algorithms, DSLs |

### Development Workflow

#### Watch Mode Development
```bash
# Automatically recompile on file changes
fp compile src/ --watch --target rust --run

# Check code continuously
fp check src/ --watch
```

#### Optimization and Analysis
```bash
# Compile with maximum optimization
fp compile src/main.fp --opt-level 3 --target llvm

# Run specific optimization passes
fp optimize src/main.fp --passes specialize,inline,const_eval --stats

# View optimization statistics
fp compile src/main.fp --target rust --debug --stats
```

#### Configuration Management
```bash
# Use custom configuration
fp compile src/main.fp --config ./custom.toml

# Generate shell completions
fp completions bash > ~/.bash_completions.d/fp
fp completions zsh > ~/.oh-my-zsh/completions/_fp
```

## 🎭 Feature Showcase

### Rich CLI Experience
The `fp` command provides a modern, professional CLI experience:

```bash
# Beautiful progress indicators and colored output
$ fp compile src/main.fp --target rust --opt-level 3 --run
🔨 Compiling src/main.fp...
⚡ Applying optimization passes (level 3)...
✓ Generated optimized Rust code: src/main.rs
🚀 Running compiled code...
Hello from FerroPhase!
```

### Comprehensive Project Templates

#### Basic Template - Get Started Quickly
```bash
$ fp init hello-world --template basic
✓ Successfully created FerroPhase project 'hello-world'

Next steps:
  $ cd hello-world
  $ fp compile src/main.fp --target rust --run
```

#### Multi-Language Template - Advanced Integration  
```bash
$ fp init ai-system --template multi-lang
✓ Created project with Python and JavaScript integration
✓ Generated configuration DSL examples
✓ Set up cross-language const evaluation
```

### Interactive Development
```bash
# Immediate expression evaluation
$ fp eval --expr "comptime { 1 + 2 * 3 }"
Result: 7

# File evaluation with AST inspection
$ fp eval --file examples/const_demo.fp --print-ast
AST: ExprBlock {
    stmts: [
        Stmt::ConstDecl { name: "RESULT", expr: BinaryOp... }
    ]
}
Result: 42

# Watch mode for rapid iteration
$ fp compile src/ --watch --target rust
👀 Watching for changes...
🔄 File changes detected, recompiling...
✓ Recompilation successful
```

### Advanced Const Evaluation

FerroPhase's const evaluation goes far beyond traditional compile-time computation:

```rust
// Complex compile-time struct generation
type DynamicAPI = comptime {
    let mut api = @create_struct("APIRouter");
    
    // Parse OpenAPI spec at compile time
    let spec = parse_openapi_file("api.yaml");
    
    for endpoint in spec.endpoints {
        let method_name = @snake_case(endpoint.path);
        @addfield(api, method_name, EndpointHandler);
        
        if endpoint.requires_auth {
            @addfield(api, format!("{}_auth", method_name), AuthValidator);
        }
    }
    
    // Generate validation at compile time
    if @field_count(api) > 50 {
        @compile_warning("Large API detected, consider splitting");
    }
    
    api
};
```

### Multi-Target Compilation
```bash
# Rust transpilation (default)
$ fp compile app.fp --target rust --output app.rs

# LLVM compilation for maximum performance  
$ fp compile app.fp --target llvm --output app.ll --opt-level 3

# WebAssembly for web deployment
$ fp compile app.fp --target wasm --output app.wasm

# Interpretation for rapid development
$ fp compile app.fp --target interpret
```

### Configuration-Driven Development
```toml
# Ferrophase.toml - Rich project configuration
[project]
name = "my-app"
template = "multi-lang"

[compilation]
target = "rust"
opt_level = 2
optimization_passes = ["specialize", "inline", "const_eval"]

[languages.python]
enabled = true
version = "3.8+"
packages = ["numpy", "scipy"]

[languages.javascript]  
enabled = true
runtime = "node"
packages = ["lodash", "moment"]

[build]
parallel = true
script = "custom_build.sh"
```

### Error Reporting and Diagnostics
FerroPhase provides beautiful, helpful error messages:

```bash
$ fp compile broken.fp
Error: Syntax error in FerroPhase code
  ┌─ src/broken.fp:5:12
  │
5 │     let x = @unknown_intrinsic();
  │            ^^^^^^^^^^^^^^^^^^^ Unknown compile-time intrinsic
  │
  = help: Available intrinsics: @sizeof, @create_struct, @addfield, ...
  = note: Check the FerroPhase intrinsics reference for details
```

## 🤝 Contributing

FerroPhase is designed for the Rust community and beyond. We welcome contributions in:

- **Language Design**: Syntax and semantics for multi-language integration
- **Compiler Engineering**: Optimization passes and code generation  
- **Type System**: Advanced type features and inference
- **Tooling**: IDE support, debugging, profiling
- **Documentation**: Tutorials, examples, best practices

Join us in building the future of high-performance, expressive programming!

## 🔥 Try It Now

### Quick 5-Minute Demo

```bash
# 1. Clone and build FerroPhase
git clone https://github.com/your-org/FerroPhase
cd FerroPhase
cargo build --release

# 2. Add to your PATH
export PATH="$PATH:$(pwd)/target/release"

# 3. Create your first project
fp init hello-ferrophase --template basic
cd hello-ferrophase

# 4. Explore the generated code
cat src/main.fp
# Shows: Advanced const evaluation example

# 5. Compile and run
fp compile src/main.fp --target rust --run
# Output: 
# Hello from FerroPhase!
# Project: hello-ferrophase  
# Good afternoon, FerroPhase!

# 6. Try interactive evaluation
fp eval --expr "1 + 2 * 3"
# Output: Result: 7

# 7. Create an advanced multi-language project
fp init ai-demo --template multi-lang
cd ai-demo
fp compile src/main.fp --target rust --run
# Shows: Cross-language const evaluation demo
```

### Real-World Example

Here's what you can build with FerroPhase today:

```rust
// trading-bot/src/main.fp
fn main() {
    // Compile-time risk analysis using Python
    const RISK_MODEL = comptime python! {
        import numpy as np
        def calculate_var(prices, confidence=0.95):
            returns = np.diff(np.log(prices))
            return np.percentile(returns, (1-confidence)*100)
        
        # Sample data for demo
        sample_prices = [100, 102, 98, 105, 97, 103, 99]
        var_95 = calculate_var(sample_prices)
        
        {
            "var_95": var_95,
            "position_limit": 1000000 if var_95 < 0.02 else 500000,
            "rebalance_freq": "daily" if var_95 < 0.01 else "hourly"
        }
    };
    
    // JavaScript config DSL for trading rules
    const TRADING_RULES = comptime javascript! {
        const rules = {
            pairs: ["BTC/USD", "ETH/USD", "ADA/USD"],
            strategies: {
                momentum: { enabled: true, lookback: 20 },
                mean_reversion: { enabled: false, threshold: 2.0 },
                arbitrage: { enabled: true, min_spread: 0.001 }
            },
            risk_limits: {
                max_position: RISK_MODEL.position_limit,
                max_drawdown: 0.05,
                stop_loss: 0.02
            }
        };
        
        // Validate configuration
        if (rules.risk_limits.max_position > 2000000) {
            throw new Error("Position limit too high for current VaR");
        }
        
        rules;
    };
    
    // Generate optimized trading engine based on analysis
    let engine = comptime {
        let mut trading_engine = @create_struct("TradingEngine");
        
        // Add fields based on enabled strategies
        for (name, config) in @reflect_fields(TRADING_RULES.strategies) {
            if config.enabled {
                @addfield(trading_engine, format!("{}_handler", name), StrategyHandler);
            }
        }
        
        // Add risk management based on VaR analysis
        if RISK_MODEL.var_95 > 0.015 {
            @addfield(trading_engine, "risk_monitor", HighFreqRiskMonitor);
        } else {
            @addfield(trading_engine, "risk_monitor", StandardRiskMonitor);
        }
        
        trading_engine
    };
    
    println!("🤖 Trading Bot Initialized");
    println!("📊 VaR (95%): {:.4}", RISK_MODEL.var_95);
    println!("💰 Position Limit: ${}", TRADING_RULES.risk_limits.max_position);
    println!("⚡ Rebalance: {}", RISK_MODEL.rebalance_freq);
    println!("🎯 Active Strategies: {}", count_enabled_strategies());
    
    // Start the engine (would connect to real exchanges in production)
    engine.run();
}
```

Run this example:
```bash
fp init trading-bot --template multi-lang
# Replace src/main.fp with the above code
fp compile src/main.fp --target rust --run
```

## 🔗 References & Inspiration

- **Zig's Comptime**: Advanced compile-time computation patterns
- **TypeScript**: Structural type system and gradual typing
- **Terra**: Multi-stage programming and LLVM integration  
- **Rust**: Safety, performance, and ecosystem foundation
- **Python**: Expressiveness and scientific computing ecosystem

---

**FerroPhase**: Where multiple languages unite for uncompromising performance and expressiveness.