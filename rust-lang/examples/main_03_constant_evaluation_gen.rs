use std::fmt::Display;
use std::ops::Add;
fn print(i: impl Display) {
    println!("{}", i)
}
fn main() {
    do_op(1, 2, 3, 4, add);
}

// result: ()
