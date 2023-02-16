use std::fmt::Display;
use std::ops::Add;

fn do_op(a: i64, b: i64, c: i64, d: i64, op: fn(i64, i64) -> i64) {
    if a > 0 {
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
    do_op(1, 2, 3, 4, add);
}
