use std::fmt::Display;
use std::ops::Add;
fn print(i: impl Display) -> () {
    println!("{}", i)
}
fn main() -> () {
    do_op_1();
}
pub fn add_0() -> i64 {
    3i64 + 4i64
}
pub fn do_op_1() -> () {
    {
        print(add_0());
    }
}

// stdout: 7i64
// result: ()
