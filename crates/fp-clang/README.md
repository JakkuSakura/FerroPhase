# fp-clang

C/C++ frontend for the FerroPhase compiler using clang.

## Overview

`fp-clang` owns the Clang parser, declaration AST, and direct LLVM integration
for C and C++ source files. The public `fp-c` and `fp-cpp` crates build on this
parser for the compiler-facing frontends: they lower C/C++ declarations into
the shared FerroPhase AST so the normal HIR, typing, MIR, and LIR pipeline can
consume them.

## Features

- Parse C and C++ source files using clang
- Generate LLVM IR from C/C++ code for the direct LLVM integration path
- Extract function declarations from header files
- Support for various C/C++ standards (C89-C23, C++98-C++23)
- Integration with fp-llvm backend
- Configurable compilation options (optimization, debug info, etc.)

## Usage

### Direct LLVM Compilation

```rust
use fp_clang::{ClangParser, CompileOptions, Standard};
use std::path::Path;

// Create parser
let parser = ClangParser::new()?;

// Set up options
let mut options = CompileOptions::default();
options.standard = Some(Standard::C11);
options.optimization = Some("2".to_string());

// Parse C file to LLVM IR (inkwell-backed module)
let source = Path::new("example.c");
let module = parser.parse_to_llvm_ir(source, &options)?;
```

### Extracting Declarations

```rust
use fp_clang::ClangCodegen;

let codegen = ClangCodegen::new()?;
let header = Path::new("mylib.h");
let options = CompileOptions::default();

// Extract function signatures
let signatures = codegen.extract_declarations(header, &options)?;

for sig in signatures {
    println!("{}", sig.to_declaration());
}
```

### Linking with FerroPhase LLVM

```rust
use fp_clang::ClangCodegen;

let codegen = ClangCodegen::new()?;

// Compile C files
let c_files = vec![
    Path::new("native.c"),
    Path::new("bindings.c"),
];

// Link with LIR program
let llvm_module = codegen.link_with_lir(&c_files, &lir_program, &options)?;
```

## Supported Standards

- **C Standards**: C89, C99, C11, C17, C23
- **C++ Standards**: C++98, C++03, C++11, C++14, C++17, C++20, C++23

## Requirements

- clang installed and available in PATH (versions 16-19 supported)
- LLVM IR library with LLVM 19 support

## Integration with FerroPhase

The `fp-clang`, `fp-c`, and `fp-cpp` crates cover two related integration
paths:

1. **Shared-AST frontend**: use `fp-c::CFrontend` or `fp-cpp::CppParser` when
   declarations must enter the common compiler pipeline.
2. **Direct LLVM integration**: use `fp-clang::ClangCodegen` when C/C++ source
   should be compiled directly to LLVM IR.

The shared-AST path is the compiler default for generated libc bindings. The
`scripts/codegen_libc.sh` script invokes the normal `fp compile` pipeline and
emits the top-level `::libc` package.

The direct integration path supplements the LLVM backend by:

1. **FFI Support**: Parse C headers to generate foreign function interface bindings
2. **Native Libraries**: Compile C/C++ libraries to link with FerroPhase code
3. **Interop**: Enable seamless interoperation between FerroPhase and C/C++
4. **Performance**: Use optimized C/C++ libraries in FerroPhase projects

## Examples

See the `examples/` directory for complete examples of:
- Calling C functions from FerroPhase
- Using C libraries
- Parsing C headers for FFI
- Mixed language compilation
