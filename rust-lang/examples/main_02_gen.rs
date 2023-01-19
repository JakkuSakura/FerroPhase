use std::ops::Add;
fn do_op(a: i64, b: i64, c: f64, d: f64, op: fn(i64, i64) -> i64) -> () {
    print(op(a, b));
    print(op(c, d));
}
fn add<T: Add>(a: T, b: T) -> T {
    a + b
}
fn print(i: i64) -> () {
    println!("{}", i)
}
fn main() -> () {
    do_op_2();
}
fn do_op_2() -> () {
    print(add_0());
    print(add_1());
}
fn add_0() -> T {
    1i64 + 2i64
}
fn add_1() -> T {
    3f64 + 4f64
}

// stdout: 3i64
// stdout: 7f64
// result: RawUse { raw: ItemUse { attrs: [], vis: Inherited, use_token: Use, leading_colon: None, tree: Path(UsePath { ident: Ident(std), colon2_token: Colon2, tree: Path(UsePath { ident: Ident(ops), colon2_token: Colon2, tree: Name(UseName { ident: Ident(Add) }) }) }), semi_token: Semi } }
