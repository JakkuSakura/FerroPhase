use rust_lang::t;
use std::fmt::Display;
fn print(i: impl Display) {
    println!("{}", i)
}
type Int = t! { i64 };
type FooUnnamedStruct = t! { struct { pub a : Int , pub b : Int } };
type BarNamedStruct = t! { struct BarNamedStruct { pub c : Int , pub d : Int } };
type FooPlusBar = t! { FooUnnamedStruct + BarNamedStruct };
fn main() {
    FooPlusBar {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
    };
}

