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
    {
        print(1i64 + 2i64);
    };
    {
        print(3f64 + 4f64);
    };
}

// stdout: 3i64
// stdout: 7f64
// result: ()
