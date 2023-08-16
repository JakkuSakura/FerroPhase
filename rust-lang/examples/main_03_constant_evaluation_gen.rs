use std::fmt::Display;
use std::ops::Add;
fn do_op(a: i64, b: i64, c: i64, d: i64, op: fn(i64, i64) -> i64) {
    if a > 0i64 {
        print(op(a, b));
    } else {
        print(op(c, d));
    }
}
fn add<T: Add>(a: T, b: T) -> T {
    a + b
}
fn print(i: impl Display) {
    println!("{}", i)
}
fn main() {
    {
        print(3i64);
    };
}
pub fn add_0() -> i64 {
    3i64
}
pub fn do_op_1() {
    print(3i64);
}

// stdout: 3i64
// result: ()
