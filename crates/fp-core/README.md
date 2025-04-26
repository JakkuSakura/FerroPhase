# fp-core

fp-core contains mainly AST of SHLL

## Boxing

Boxing/interning inside AST is a trade-off between performance and ergonomics.

```rust
enum Expression {
    Variant(Struct)
}

struct Struct {
    expression: Box<Expression> //, or just AExpr
}
```

1. has better performance and egonomics

So the rules are:

1. Box will only appear in enum
2. Use enum directly without considering Box
3. Make sure enum is thin enough to pass around

## Ids

For other IRs, we use ids to refer to other seconds for re-use

## TODO

1. [ ] support unsafe and pointers via virtual machine
2. [ ] evaluate kinda complex rust std types
3. [ ] query-driven system like rustc

