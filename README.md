# SHLL: Simple High Level Language

The language is a statically typed language, compiled LISP variant. It's intended to be used a platform for high level optimization and transpilation.
The syntax is deliberately simple yet expressive: everything is an application, either of type or of value. 
This way, it's easy to generate and parse, and versatile enough to maintain all high level information
```text
(block
   (def fun foo (lp (p a int)) (int)
     a
   )
   (foo 1)
)
```
This program defines a function `foo` that takes `a` of integer and returns `a`, then apply 1 to `foo`.

The language is designed to be compatible with multiple frontend, ideally Scala 3, and multiple backend, ideally Rust and C.

With this language, you can write logic the comfortable way, and then the optimizer will rewrite it into a performant low level code.

Here's a list of techniques you can use in ordinary languages:

- interpreter is slow
- template is fastest, but with weird syntax
- generics(in Java) doesn't provide much runtime performance benefit
- polymorphism involves calling dynamic dispatched methods, which involves a runtime cost
- codegen requires an external tool, and writing such tool involves much work
- macros(in Rust/Scala/C) is hard to debug, and limited to decorate a function/struct(or text replacement in C)
- lisp is powerful in metaprogramming, but it's an interpreter language


The ideal solution is the SHLL language, which specialize code into low level languages:

- Write declarative code, and then the optimizer will optimize it into a imperative code. 

To sum up: make the language simple yet expressive, and produces fast low-level code

## Syntax

### Frontend
The frontend is designed to be simple, then it looks like LISP

1. Literal values: `1`, `1.0`, `foo`, `true`, `false`, `"string"`, `'c'`
2. Everything in value level is an application, argument can be positional or named `(func 1 2 3 foo=bar)`, where `func` is the name of applicant
3. Basic types: `int`, `float`, `bool`, `char`, `string`, `list`, `struct`, `fun`
4. Type (templated or not) are also an application
5. Writing an parser is easy. You even don't have to write a full interpreter, just write a simple parser


Concrete Syntax Tree is built with antlr4, then converted to Abstract Syntax Tree. 
The simple syntax design makes CST and AST almost equivalent.

### Language features
To make the language practical, we need to add some features to it:
- [x] Literals
- [ ] static types
- [ ] Def operator
```shll
(def type name args) => (let name [type args])
(def fun def (lp (: type ident) (: name (listof any) (: args (listof any) flatten=true)) (unit)
  (let name (type name=name args=args) parent_scope=true)
)
```
- [ ] Sum types
```shll
(enum
  (v foo int)
  (v bar int)
)
```
- [ ] Product types
```shll
(struct
  (f foo int)
  (f bar int)
)
```
- [ ] functions
```shll
(fun
  (lp (p a int) (p b int))
  int
  body?
)
```
- [ ] kinds
```shll
(def fun listof (lp (p value type)) (inferred)
  (struct
    (f arr (arrayof value)) 
    (f len int) 
    name=list
  )
)
```
- [ ] traits and generics
```shll
(def trait num 
  (def fun add (lp (p a self) (p b self)) self)
  (def fun sub (lp (p a self) (p b self)) self)
  (def fun mul (lp (p a self) (p b self)) self)
  (def fun div (lp (p a self) (p b self)) self)
  (def fun neg (lp (p a self)) self)
)

(// "templates")
(def fun sum (lp (p t type)) (funof (lp (p l (listof t)) t
  (fun (lp (p l (listof t) flatten=true) t
    (block
      (let s 0)
      (for i l
        (assign s (add s i))
      )
      s
    )
  )
)
(// "can it be more convenient with type inference?")
((sum int) (list 1 2 3))
(// "with just trait itself")
(def fun sum (lp (p l (listof num) flatten=true) num
  (block
    (def val s num 0)
    (for i l
      (assign s (add s i))
    )
    s
  )
)
(sum 1 2 3)
(// "should it be allowed?")
(def trait point
  (def field x int)
  (def field y int)
)
```
## Optimization
Then AST gets passed through multiple optimization phrases, while maintaining the same semantics.

Current optimization phrases:
- [x] Specialization and inlining
- [x] Flow analysis
- [x] Dead code elimination
- [ ] Avoid boxing in low-level(rust) backend
### Specialization

- Constant evaluation
```shll
(block
    (def fun foo (lp (: a int)) int
      a + 2
    )
    (print (foo 1) + (foo (input)))   
)
```

gives
```shll
(block
  (def fun foo (lp (: a int)) int
    a + 2
  )
  (print 2 + (foo (input)))
)
```

Unless the function is too big, inlining does not perform

- Loop unrolling
```shll
(for i (range 0 3)
  (print i)
)
```
```shll
(block
  (print 0)
  (print 1)
  (print 2)
)
```


### Flow analysis

Graph-based(and effects and contexts):

- [x] build a graph of data flow
- [x] build a graph of control flow
- [ ] Falliblity: The effect of a section of code failing to complete and evaluate to its expected value (in Rust, think Result)
- [ ] Multiplicity: The effect of a section of code being evaluated multiple times, yielding many values or operating over many values (in Rust, think Iterator)
- [ ] Asynchrony: The effect of a section of code yielding control when it cannot immediately progress, to allow other sections of code to progress instead (in Rust, think Future)
- [ ] Pureness: The effect of a function having no side effects
- [ ] Safeness(sorry to toss you in): The effect of a section of code being unsafe, to use `unsafe { }` to suppress. And many other types of safeness
- [ ] Deprecation
- [ ] Some Rust ideas: Ref, MutRef

Readings
https://boats.gitlab.io/blog/post/the-problem-of-effects/
https://internals.rust-lang.org/t/can-we-make-a-rusty-effect-system/11697


### Dead code elimination

- [x] use graph-based data flow to eliminate dead code
- [x] combine data flow and control flow to eliminate dead code


## Backend
Then AST gets transpiled into a low level language, which is either Rust for performance or natively scala. FFI is not an issue as we compile at source code level

The language aims to experiment simple syntax, maximum runtime performance.
Compile time is not a concern, as we maintain the same semantics between phrases, some optimization can be disabled for fast compilation, or even use interpretation mode.

The ideal usage is to build a high frequency trading system and write strategies for it.


## TODO
- [ ] use scalameta to parse/generate scala code
