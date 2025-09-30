#!/usr/bin/env fp run
//! Higher-order functions: passing functions as arguments and closures

use std::fmt::Display;
use std::ops::Add;

fn apply<T: Add<Output = T> + Display>(a: T, b: T, op: fn(T, T) -> T) {
    println!("{}", op(a, b));
}

fn add<T: Add<Output = T>>(a: T, b: T) -> T {
    a + b
}

fn apply_if(cond: bool, a: i64, b: i64, op: fn(i64, i64) -> i64) -> i64 {
    if cond {
        op(a, b)
    } else {
        0
    }
}

fn make_adder(n: i64) -> impl Fn(i64) -> i64 {
    move |x| x + n
}

fn main() {
    // Pass function as argument
    println!("Generic operations:");
    apply(10i64, 20i64, add);
    apply(1.5f64, 2.5f64, add);

    // Conditional with function
    println!("\nConditional:");
    println!("{}", apply_if(true, 5, 3, add));
    println!("{}", apply_if(false, 5, 3, add));

    // Return function (closure)
    println!("\nClosure factory:");
    let add_10 = make_adder(10);
    println!("add_10(5) = {}", add_10(5));

    // Lambda
    let double = |x| x * 2;
    println!("double(7) = {}", double(7));
}