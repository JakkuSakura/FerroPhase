# common-lang

common-lang contains mainly AST of SHLL

## TODO

- [ ] Refactor boxing method: from oxc project

After experimenting with two types of boxed enum variants:

1.

```
enum Expression {
    Variant(Box<Struct>)
}
struct Struct {
    expression: Expression
}
```

2.

```
enum Expression {
    Variant(Struct)
}
struct Struct {
    expression: Box<Expression>
}
```

have concluded that the first options is more performant and more ergonomic to use.
he following test make sure all enum variants are boxed, resulting 16 bytes for each enum.
ead `https://nnethercote.github.io/perf-book/type-sizes.html` for more details
