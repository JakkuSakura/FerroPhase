#!/usr/bin/env fp run
//! Function specialization via generic monomorphization

use std::fmt::Display;
use std::ops::Add;

fn add<T: Add<Output = T> + Copy>(a: T, b: T) -> T {
    a + b
}

fn double<T: Add<Output = T> + Copy>(x: T) -> T {
    x + x
}

fn pipeline<T: Add<Output = T> + Copy + Display>(a: T, b: T) -> T {
    let result = double(add(a, b));
    println!("specialized result: {}", result);
    result
}

fn main() {
    // Each concrete type generates a specialized version of the generic functions.
    let _ = pipeline(10i64, 20i64);
    let _ = pipeline(1.5f64, 2.5f64);
}
