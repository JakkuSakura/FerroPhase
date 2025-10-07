# fp-clang Integration Guide

## Overview

The `fp-clang` crate provides C/C++ language support for FerroPhase by leveraging clang to compile C/C++ code to LLVM IR, which can then be integrated with the FerroPhase LLVM backend.

## Architecture

```
C/C++ Source → clang → LLVM IR → fp-llvm → LIR → LLVM Backend
                                     ↓
                          FerroPhase LIR Program
```

## Use Cases

### 1. Foreign Function Interface (FFI)

Parse C headers to generate bindings:

```rust
use fp_clang::{ClangCodegen, CompileOptions};

let codegen = ClangCodegen::new()?;
let signatures = codegen.extract_declarations(
    Path::new("mylib.h"),
    &CompileOptions::default()
)?;

// Generate FerroPhase FFI declarations
for sig in signatures {
    println!("extern \"C\" {}", sig.to_declaration());
}
```

### 2. Native Library Integration

Compile C/C++ libraries and link with FerroPhase:

```rust
use fp_clang::{ClangCodegen, CompileOptions, Standard};

let codegen = ClangCodegen::new()?;
let mut options = CompileOptions::default();
options.standard = Some(Standard::C11);
options.optimization = Some("2".to_string());

// Compile C library
let c_module = codegen.compile_file(
    Path::new("mylib.c"),
    &options
)?;

// Link with FerroPhase LIR
let linked = codegen.link_with_lir(
    &[Path::new("mylib.c")],
    &lir_program,
    &options
)?;
```

### 3. Performance-Critical Code

Write performance-critical sections in C/C++ and integrate with FerroPhase:

```rust
// math_ops.c - optimized C implementation
int fast_multiply(int* arr, int len) {
    int result = 1;
    for (int i = 0; i < len; i++) {
        result *= arr[i];
    }
    return result;
}

// Compile and use from FerroPhase
let module = parser.parse_to_llvm_ir(
    Path::new("math_ops.c"),
    &options
)?;
```

## Supported Features

### C/C++ Standards

- **C**: C89, C99, C11, C17, C23
- **C++**: C++98, C++03, C++11, C++14, C++17, C++20, C++23

### Compilation Options

```rust
let mut options = CompileOptions::default();
options.standard = Some(Standard::C11);
options.optimization = Some("2".to_string()); // -O2
options.debug = true; // Include debug info
options.include_dirs.push("/usr/local/include".to_string());
options.flags.push("-Wall".to_string());
```

### Features

- ✅ Function compilation
- ✅ Variadic functions
- ✅ Struct/Union types
- ✅ Typedefs
- ✅ Header parsing
- ✅ C++ compilation (with name mangling)
- ✅ Optimization levels (0-3, s, z)
- ✅ Debug information
- ✅ Custom include paths
- ✅ Compiler flags

## Integration with fp-llvm

The fp-clang crate produces `llvm_ir::Module` objects that are compatible with fp-llvm:

```rust
// Compile C code
let c_module = clang_parser.parse_to_llvm_ir(c_file, &options)?;

// Use with fp-llvm backend
let llvm_codegen = fp_llvm::LlvmCodegen::new()?;
llvm_codegen.generate_object_file(&c_module, output_file)?;
```

## Future Enhancements

- [ ] Automatic binding generation for C structs
- [ ] C++ template support
- [ ] Objective-C support
- [ ] Module linking and optimization
- [ ] Cross-compilation support
- [ ] Static library integration
- [ ] Shared library loading

## Requirements

- clang (version 16-19)
- LLVM development libraries
- Standard C/C++ headers

## Error Handling

The crate provides detailed error types:

```rust
match parser.parse_to_llvm_ir(file, &options) {
    Ok(module) => // Success
    Err(ClangError::ClangNotFound(msg)) => // clang not installed
    Err(ClangError::CompilationFailed(stderr)) => // Compilation error
    Err(ClangError::LlvmIrError(msg)) => // LLVM IR parsing error
    Err(e) => // Other error
}
```

## Testing

Run the test suite:

```bash
cargo test -p fp-clang
```

Run examples:

```bash
cargo run -p fp-clang --example parse_c_file
cargo run -p fp-clang --example extract_headers
```
