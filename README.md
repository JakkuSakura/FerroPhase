# FerroPhase: Staged High Level Language

This language explores staging in Rust, but also leave space for transpilation.

Goal: Readability without compromising performance

It adds high-level generics and codegen to Rust.

See rust-lang/examples/main_02.rs for example.

## Name Origin

The name "FerroPhase" is derived from two key concepts that define this language:

1. **Ferro-**: From the Latin word "ferrum" meaning iron, establishing a connection to Rust (which is iron oxide). This acknowledges FerroPhase's foundation in Rust syntax and semantics, while suggesting the strength and stability of iron.

2. **-Phase**: Represents the staged compilation process that transforms high-level code through multiple optimization phases. This highlights the language's key innovation: the ability to express code at a high level and systematically transform it into highly optimized low-level code.

Together, "FerroPhase" captures both the language's Rust heritage and its distinctive staged transformation approach that sets it apart from traditional compiled languages.

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

The ideal solution is the FerroPhase language, which specialize code into low level languages:

- Write declarative/high level code, and then the optimizer will optimize it into imperative code.

To sum up: make the language simple yet expressive, and produces fast low-level code

## Syntax

## Frontend

Same as rust

## Optimization

Then AST gets passed through multiple optimization phrases, while maintaining the same semantics.

Current optimization phrases:

- [x] Specialization and inlining
- [ ] Flow analysis
- [ ] Dead code elimination

### Specialization

- Constant evaluation
  Unless the function is too big, inlining does not perform

- Loop unrolling


- [ ] Zig's comptime, like rust macros but supports comptime inspection and is to replace templates
-

### Flow analysis

Graph-based(and effects and contexts):

- [x] build a graph of data flow
- [x] build a graph of control flow
- [ ] Falliblity: The effect of a section of code failing to complete and evaluate to its expected value (in Rust, think
  Result)
- [ ] Multiplicity: The effect of a section of code being evaluated multiple times, yielding many values or operating
  over many values (in Rust, think Iterator)
- [ ] Asynchrony: The effect of a section of code yielding control when it cannot immediately progress, to allow other
  sections of code to progress instead (in Rust, think Future)
- [ ] Pureness: The effect of a function having no side effects
- [ ] Safeness(sorry to toss you in): The effect of a section of code being unsafe, to use `unsafe { }` to suppress. And
  many other types of safeness
- [ ] Deprecation
- [ ] Some Rust ideas: Ref, MutRef

Readings
https://boats.gitlab.io/blog/post/the-problem-of-effects/
https://internals.rust-lang.org/t/can-we-make-a-rusty-effect-system/11697

### Dead code elimination

- [ ] use graph-based data flow to eliminate dead code
- [ ] combine data flow and control flow to eliminate dead code

## Backend

Then AST gets transpiled into a low level language, which is either Rust for performance or natively scala. FFI is not
an issue as we compile at source code level

The language aims to experiment simple syntax, maximum runtime performance.
Compile time is not a concern, as we maintain the same semantics between phrases, some optimization can be disabled for
fast compilation, or even use interpretation mode.

### Type System

Hope to bring some of typescript's type system to FerroPhase. It verifies and infer types at compile time, and then generate
solid types in Rust.
It should also be available as a cli tool and standalone library.

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

## References

Struct is (partly) similar to GoLang and Zig
Similar to Zig's comptime system, but aims to do more than Zig with less and simpler
code. https://kristoff.it/blog/what-is-zig-comptime/
Interesting article describes biformity which FerroPhase aims to solve:
https://hirrolot.github.io/posts/why-static-languages-suffer-from-complexity.html

## TODO

- [ ] Use miette for error handling
- [ ] Refer to rustc's demand-driven compilation. i.e. use trait instead of passes where possible. this it already WIP
- [ ] Use tree-sitter for CST
- [ ] TypeScript's type system