pub use fp_rust::t;
pub use std::fmt::Display;
pub fn print(i: impl Display) -> () {
    println!("{}", i)
}
pub type Int = i64;
pub struct FooUnnamedStruct {
    pub a: Int,
    pub b: Int,
}
pub struct BarNamedStruct {
    pub c: Int,
    pub d: Int,
}
pub struct FooPlusBar {
    pub a: Int,
    pub b: Int,
    pub c: Int,
    pub d: Int,
}
pub fn main() -> () {
    FooPlusBar {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
    };
}
