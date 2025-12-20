pub use fp_rust::t;
pub use std::fmt::Display;
pub fn print(i: impl Display) -> () {
    println!("{}", i)
}
pub type Int = t! {
    i64
};
pub type FooUnnamedStruct = t! {
    struct{
        pub a : Int , pub b : Int
    }
};
pub struct BarNamedStruct {
    pub c: Int,
    pub d: Int,
}
pub type FooPlusBar = t! {
    (FooUnnamedStruct + BarNamedStruct)
};
pub fn main() -> () {
    FooPlusBar {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
    };
}
