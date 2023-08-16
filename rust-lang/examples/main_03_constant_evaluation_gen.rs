use std::fmt::Display;
use std::ops::Add;
fn print(i: impl Display) {
    println!("{}", i)
}
fn main() {
    do_op_1_3();
}
pub fn add_0() -> i64 {
    3i64
}
pub fn add_0_2() -> i64 {
    3i64
}
pub fn do_op_1() {
    print(add_0());
}
pub fn do_op_1_3() {
    print(add_0_2());
}

// stdout: 3i64
// result: ()
