use std::fmt::Display;
use std::ops::Add;
fn print(i: impl Display) {
    println!("{}", i)
}
fn main() {
    {
        print(1 + 2);
    };
    {
        print(3 + 4);
    };
}
pub fn add_0() -> i64 {
    1 + 2
}
pub fn add_2() -> f64 {
    3 + 4
}
pub fn do_op_4() {
    {
        print(1 + 2);
    };
    {
        print(3 + 4);
    };
}
pub fn print2_1() {
    print(1 + 2);
}
pub fn print2_3() {
    print(3 + 4);
}

// stdout: 3
// stdout: 7
// result: ()
