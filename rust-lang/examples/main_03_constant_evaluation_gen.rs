use std::fmt::Display;
use std::ops::Add;
fn print(i: impl Display) {
    println!("{}", i)
}
fn main() {
    print(1i64 + 2i64);
}
pub fn add_0() -> i64 {
    1i64 + 2i64
}
pub fn do_op_1() {
    print(1i64 + 2i64);
}

// stdout: 3i64
// result: ()
