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


