use std::fmt::Display;
use std::ops::Add;
fn print(i: impl Display) {
    println!("{}", i)
}
fn main() {
    {
        print(1i64 + 2i64);
    };
    {
        print(3f64 + 4f64);
    };
}
pub fn add_0() -> i64 {
    1i64 + 2i64
}
pub fn add_2() -> f64 {
    3f64 + 4f64
}
pub fn do_op_4() {
    {
        print(1i64 + 2i64);
    };
    {
        print(3f64 + 4f64);
    };
}
pub fn print2_1() {
    print(1i64 + 2i64);
}
pub fn print2_3() {
    print(3f64 + 4f64);
}

// stdout: 3i64
// stdout: 7f64
// result: ()
