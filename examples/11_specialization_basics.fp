#!/usr/bin/env fp run
//! Function specialization via generic monomorphization

use std::fmt::Display;
use std::ops::Add;

struct Pair<T> {
    left: T,
    right: T,
}

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

fn sum_pair<T: Add<Output = T> + Copy>(pair: Pair<T>) -> T {
    pair.left + pair.right
}

fn max<T: PartialOrd + Copy + Display>(a: T, b: T) -> T {
    let result = if a > b { a } else { b };
    println!("max({}, {}) = {}", a, b, result);
    result
}

fn main() {
    println!("📘 Tutorial: 11_specialization_basics.fp");
    println!("🧭 Focus: Function specialization via generic monomorphization");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    // Each concrete type generates a specialized version of the generic functions.
    let _ = pipeline(10i64, 20i64);
    let _ = pipeline(1.5f64, 2.5f64);

    let pair_i = Pair { left: 3i64, right: 7i64 };
    let pair_f = Pair { left: 1.25f64, right: 2.75f64 };
    println!("sum_pair i64 = {}", sum_pair(pair_i));
    println!("sum_pair f64 = {}", sum_pair(pair_f));

    let _ = max(10i64, 3i64);
    let _ = max(2.5f64, 9.0f64);
}
