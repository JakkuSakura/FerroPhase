use std::fmt::Display;
use std::ops::Add;
fn add<T: Add>(a: T, b: T) -> T {
    a + b
}
fn print(i: impl Display) {
    println!("{}", i)
}
fn main() {
    let c = 3i64;
    let a = 1i64;
    let d = 4i64;
    let b = 2i64;
    let op = add;
    if a > 0i64 {
        print(op(a, b));
    } else {
        print(op(c, d));
    };
}
pub fn do_op_0() {
    let c = 3i64;
    let a = 1i64;
    let d = 4i64;
    let b = 2i64;
    let op = add;
    if a > 0i64 {
        print(op(a, b));
    } else {
        print(op(c, d));
    }
}

// stdout: 3i64
// result: ()
