#!/usr/bin/env fp run
//! Type arithmetic example demonstrating struct composition.
//! Shows how to combine types using the + operator to merge struct fields.

use fp_rust::t;
use std::fmt::Display;

fn print(i: impl Display) {
    println!("{}", i)
}

type Int = i64;

type FooUnnamedStruct = t! {
    struct {
        a: Int,
        b: Int,
    }
};

struct BarNamedStruct {
    c: Int,
    d: Int,
}

type FooPlusBar = t! {
     FooUnnamedStruct + BarNamedStruct
};

fn main() {
    FooPlusBar {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
    };
}
