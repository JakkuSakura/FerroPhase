use std::fmt::Display;
use std::ops::Add;
fn print(i: impl Display) {
    println!("{}", i)
}
fn main() {
    {
        print(3i64);
        print(7f64);
    };
}
pub fn add_0() -> i64 {
    3i64
}
pub fn add_1() -> f64 {
    7f64
}
pub fn do_op_2() {
    print(3i64);
    print(7f64);
}

// stdout: 3i64
// stdout: 7f64
// result: ()
