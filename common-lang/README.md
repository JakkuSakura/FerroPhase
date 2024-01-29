# common-lang

common-lang contains mainly AST of SHLL

## Boxing
According to oxc and syn

1.
```rust
   enum Expression {
       Variant(Box<Struct>)
   }
   struct Struct {
       expression: Expression
   }
```
2.
```rust
   enum Expression {
       Variant(Struct)
   }
   struct Struct {
       expression: Box<Expression>
   }
```

1. has better performance and egonomics

So the rules are:
1. Box will only appear in enum
2. Use enum directly without considering Box
3. Make sure enum is thin enough to pass around

## TODO
1. [ ] support unsafe and pointers via virtual machine
2. [ ] evaluate kinda complex rust std types
3. [x] new code structure:
    ```text
    id/{path, id, hash} -> refers to other expr/value/ty nodes
    expr/{if, for, etc} -> AST nodes that generates an typed value, living in lifted code universe
    expr/ty -> special AST node that generates type
    value/{struct, enum, etc} -> value living in memory universe
    value/ty -> type in memory
    ty/{int, float, etc} -> type living in type universe
    ```
4. [ ] use ids, instead of `Box<T>`
5. [ ] query-driven system like rustc

