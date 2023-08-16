use std::fmt::Display;
use std::ops::Add;
fn print(i: impl Display) {
    println!("{}", i)
}
fn main() {
    do_op_2_5();
}
pub fn add_0() -> i64 {
    3i64
}
pub fn add_0_3() -> i64 {
    3i64
}
pub fn add_1() -> f64 {
    7f64
}
pub fn add_1_4() -> f64 {
    7f64
}
pub fn do_op_2() {
    print(add_0());
    print(add_1());
}
pub fn do_op_2_5() {
    print(add_0_3());
    print(add_1_4());
}

// stdout: 3i64
// stdout: 7f64
// result: ()
