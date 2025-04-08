# FerroPhase: High-Level Extension Language for Rust

This language explores higher-level abstractions for Rust through staged compilation and rich type manipulation.

Goal: Expressiveness and safety while leveraging Rust's performance

It adds advanced type capabilities and meta-programming to the Rust ecosystem.

See rust-lang/examples/main_02.rs for example.

## Name Origin

The name "FerroPhase" is derived from two key concepts that define this language:

1. **Ferro-**: From the Latin word "ferrum" meaning iron, establishing a connection to Rust (which is iron oxide). This acknowledges FerroPhase's foundation in Rust syntax and semantics.

2. **-Phase**: Represents the staged compilation process that transforms high-level abstractions into valid Rust code. This highlights the language's role as a phase in the development process that feeds into the Rust compiler.

Together, "FerroPhase" captures both the language's Rust heritage and its role as a complementary layer that enhances Rust development.

## Vision: Complementing Rust

FerroPhase aims to be a complement to Rust rather than a replacement. While Rust excels at performance and safety, it can be verbose and lacks certain high-level abstractions. FerroPhase addresses this gap by:

- Providing advanced type system features inspired by TypeScript
- Enabling powerful meta-programming capabilities
- Simplifying complex patterns through higher-level abstractions
- Transpiling to clean, idiomatic Rust code

When it comes to abstraction, even Rust and C++ claim they provide zero-cost abstraction, but they also have their
problems.

- Interpreted languages like Python is slow
- template in C++ and Rust is fastest, but with weird syntax and incomplete features compared to the language itself
- Generics(in Java) doesn't provide much runtime performance benefit. Even with help of JIT it's hard to optimize at
  higher level than method
- OOP polymorphism involves calling dynamic dispatched methods, which involves a runtime cost
- codegen requires an external tool, and writing such tool involves much work
- macros(in Rust/Scala/C) is hard to debug, and limited to decorate a function/struct(or text replacement in C)
- lisp is powerful in metaprogramming, but it's an interpreter language

The ideal solution is the FerroPhase language, which provides high-level abstractions that transpile to Rust:

- Write declarative/high level code, and let Rust handle the performance aspects
- Leverage Rust's ecosystem and optimization capabilities

To sum up: make the language simple yet expressive, and produces clean, idiomatic Rust code

## Syntax

## Frontend

Same as rust with extensions for advanced type operations and meta-programming

## Optimization

Then AST gets passed through multiple transformation phases, maintaining the same semantics while generating optimal Rust code.

Current transformation phases:

- [x] Specialization and inlining
- [ ] Flow analysis
- [ ] Dead code elimination

### Specialization

### Flow analysis

### Dead code elimination

## Backend

AST gets transpiled into Rust as the primary target language. This approach leverages:

- Full interoperability with existing Rust code
- The entire Rust ecosystem
- Rust's sophisticated optimizations
- Zero runtime overhead compared to hand-written Rust
- Easy integration into existing Rust projects

Scala remains as an experimental alternative target.

The language aims to experiment with simple syntax and expressive capabilities.
Compile time is not a concern, as we maintain the same semantics between phases.

### Type System

FerroPhase brings TypeScript-inspired type features to the Rust ecosystem:

```rust
// structural typing, solid type
type Foo = struct {
    a: i32,
    b: i32,
}
// named typing
struct Foo {
    a: i32,
    b: i32,
}
// impl trait
impl Read
// addition of traits
impl Read + impl Write
impl Read + Write
// generic
Vec<T>
// subtraction of traits
impl Read + impl Write - impl Write
// union type
type RW = impl Write | impl Write
// intersection type
type RW = impl Read + impl Write
type RW = impl Read & impl Write

// building a struct
type Foo2 = Type<{
    type mut T = struct {}
    T.a = bool;
    T.b = i32;
    T
}>

// type function
type NestedVec<T, const n: usize> = Type<{
    if n == 0 {
        T
    } else {
        Vec < NestedVec < T, n - 1 >>
    }
}>
// typeof
type Foo = typeof(1)
// literal types
type Foo = 1

// type alias
type Foo = i32

// if const
fn foo() {
    if const { true } {
        println!("true")
    } else {
        println!("false")
    }
}
// type dict
type Foo = struct {
    "/api": EndpointApi,
    "/user": EndpointUser,
}
Foo["/api"]

// fn LinkedList(comptime T: type) type {
//   return struct {
//     pub const Node = struct {
//     prev: ? * Node = null,
//     next: ? * Node = null,
//     data: T,
//     };
//     
//     first: ? * Node = null,
//     last: ? * Node = null,
//     len: usize = 0,
//   };
// }

type LinkedList<T> = struct {
    type Node = struct {
    prev: ? * Node = null,
    next: ? * Node = null,
    data: T,
    };
    first: ? * Node = null,
    last: ? * Node = null,
    len: usize = 0,
}
```

## Use Cases

FerroPhase is particularly valuable for:

1. **API Design**: Create expressive, type-safe APIs with minimal boilerplate
2. **Domain-Specific Languages**: Build custom DSLs that compile to efficient Rust
3. **Complex Data Processing**: Express algorithms at a higher level
4. **Type-Heavy Systems**: Work with sophisticated type relationships more easily
5. **Code Generation**: Generate specialized implementations from high-level descriptions

## References

Struct is (partly) similar to GoLang and Zig
Similar to Zig's comptime system, but aims to do more than Zig with less and simpler
code. https://kristoff.it/blog/what-is-zig-comptime/
Interesting article describes biformity which FerroPhase aims to solve:
https://hirrolot.github.io/posts/why-static-languages-suffer-from-complexity.html

## TODO

- [ ] Use miette for error handling
- [ ] Refer to rustc's demand-driven compilation. i.e. use trait instead of passes where possible. this is already WIP
- [ ] Use tree-sitter for CST
- [ ] Implement checks using passes, similar to mutations
- [ ] Implement TypeScript-inspired type system
- [ ] Create seamless Rust integration
- [ ] Build language server for IDE support

## Development Roadmap

### Phase 1: Core Language Foundation (Short-term)
1. **Enhanced Type System**
   - Implement structural typing
   - Add union and intersection types
   - Develop type inference engine
   - Support type-level computation

2. **Metaprogramming Foundation**
   - Implement compile-time evaluation
   - Create AST manipulation utilities
   - Support code generation from types

3. **Rust Integration**
   - Seamless import of Rust types and functions
   - Clean Rust code generation
   - Type-preserving translation

4. **Developer Experience**
   - Implement helpful error messages
   - Create basic IDE support
   - Build documentation tools

### Phase 2: Advanced Features (Medium-term)
1. **Extended Type System**
   - Add effect tracking
   - Implement advanced type inference
   - Support higher-kinded types
   - Create standard type utilities

2. **Pattern Libraries**
   - Functional programming patterns
   - Common design patterns
   - Domain-specific abstractions

3. **Tooling Ecosystem**
   - Full-featured language server
   - Integration with popular IDEs
   - Build system integration

4. **Standard Library**
   - High-level data structures
   - Functional utilities
   - Common abstractions

### Phase 3: Ecosystem Growth (Long-term)
1. **Community Building**
   - Documentation and tutorials
   - Example projects
   - Community packages

2. **Integration with Rust Ecosystem**
   - Crates.io compatibility
   - Cargo plugin
   - Seamless workflow with Rust projects

3. **Enterprise Features**
   - Large project support
   - Performance profiling
   - Migration tools from pure Rust

### Contributors Guide

1. **Getting Started**
   - Set up the development environment
   - Understand the compiler architecture
   - Pick beginner-friendly issues

2. **Areas Needing Help**
   - Parser improvements and syntax design
   - Type system implementation
   - Optimization algorithms
   - Backend code generation
   - Testing infrastructure
   - Documentation

3. **Development Process**
   - Fork the repository
   - Make changes in a feature branch
   - Submit pull requests with tests
   - Participate in code reviews

By focusing on complementing Rust rather than replacing it, FerroPhase can carve out a valuable niche in the programming language ecosystem, similar to how TypeScript extends JavaScript or how Kotlin enhances Java.
`