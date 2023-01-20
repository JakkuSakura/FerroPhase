use std::ops::Add;
fn print(i: i64) -> () {
    println!("{}", i)
}
fn main() -> () {
    do_op_2();
}
fn add_0() -> T {
    1i64 + 2i64
}
fn do_op_2() -> () {
    print(add_0());
    print(add_1());
}
fn add_1() -> T {
    3f64 + 4f64
}

// stdout: 3i64
// stdout: 7f64
// result: RawUse { raw: ItemUse { attrs: [], vis: Inherited, use_token: Use, leading_colon: None, tree: Path(UsePath { ident: Ident(std), colon2_token: Colon2, tree: Path(UsePath { ident: Ident(ops), colon2_token: Colon2, tree: Name(UseName { ident: Ident(Add) }) }) }), semi_token: Semi } }
