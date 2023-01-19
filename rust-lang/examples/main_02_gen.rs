fn do_op(a: i64, b: i64, op: fn(i64, i64) -> i64) -> i64 {
    op(a, b)
}
fn add(a: i64, b: i64) -> i64 {
    a + b
}
fn print(i: i64) -> () {
    println!("{}", i)
}
fn main() -> () {
    print(do_op_1());
    print(do_op_3());
}
fn add_2() -> i64 {
    10i64 + 20i64
}
fn add_0() -> i64 {
    1i64 + 2i64
}
fn do_op_3() -> i64 {
    add_2()
}
fn do_op_1() -> i64 {
    add_0()
}

// stdout: 3i64
// stdout: 30i64
// result: Unit
