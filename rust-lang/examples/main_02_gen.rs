use std::fmt::Display;
use std::ops::Add;
fn do_op(a: i64, b: i64, c: f64, d: f64, op: fn(i64, i64) -> i64) -> () {
    print(op(a, b));
    print(op(c, d));
}
fn add<T: Add>(a: T, b: T) -> T {
    a + b
}
fn print(i: impl Display) -> () {
    println!("{}", i)
}
fn main() -> () {
    do_op_2();
}
pub fn do_op_2() -> () {
    print(fun_0());
    print(fun_1());
}
pub fn fun_0() -> T {
    3i64
}
pub fn fun_1() -> T {
    7f64
}

// stdout: 3i64
// stdout: 7f64
// result: ()
