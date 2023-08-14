use rust_lang::t;
use std::fmt::Display;
fn print(i: impl Display) {
    println!("{}", i)
}
type Int = i64;
type FooUnnamedStruct = t! { struct { pub a : Int , pub b : Int } };
type BarNamedStruct = t! { struct BarNamedStruct { pub c : Int , pub d : Int } };
type FooPlusBar = FooUnnamedStruct + BarNamedStruct;
fn main() {
    FooPlusBar {
        a: 1i64,
        b: 2i64,
        c: 3i64,
        d: 4i64,
    };
}

