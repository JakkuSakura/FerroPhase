use std::fmt::Display;
use std::ops::Add;

fn do_op<T: Add>(a: i64, b: i64, c: f64, d: f64, op: fn(T, T) -> T) {
    print2(op(a, b));
    print2(op(c, d));
}
fn add<T: Add>(a: T, b: T) -> T {
    a + b
}
fn print(i: impl Display) {
    println!("{}", i)
}
fn print2(i: impl Display) {
    print(i);
}
fn main() {
    do_op(1, 2, 3.0, 4.0, add);
}
