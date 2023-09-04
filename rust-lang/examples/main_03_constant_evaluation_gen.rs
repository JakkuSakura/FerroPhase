use std::fmt::Display;
use std::ops::Add;
fn print(i: impl Display) {
    println!("{}", i)
}
fn main() {
    print(1 + 2);
}
pub fn add_0() -> i64 {
    1 + 2
}
pub fn do_op_1() {
    print(1 + 2);
}

// stdout: 3
// result: ()
