use std::fmt::Display;
use std::ops::Add;
fn print(i: impl Display) -> () {
    println!("{}", i)
}
fn main() -> () {
    (do_op_2());
}
pub fn add_1() -> f64 {
    (3f64 + 4f64)
}
pub fn add_0() -> i64 {
    (1i64 + 2i64)
}
pub fn do_op_2() -> () {
    (print((add_0())));
    (print((add_1())));
}

// stdout: 3i64
// stdout: 7f64
// result: ()
