#!/usr/bin/env fp interpret
//! Higher-order functions: passing functions as arguments and closures

fn apply_i64(a: i64, b: i64, op: fn(i64, i64) -> i64) {
    println!("apply({}, {}) = {}", a, b, op(a, b));
}

fn add_i64(a: i64, b: i64) -> i64 {
    a + b
}

fn apply_f64(a: f64, b: f64, op: fn(f64, f64) -> f64) {
    println!("apply({}, {}) = {}", a, b, op(a, b));
}

fn add_f64(a: f64, b: f64) -> f64 {
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
    move |x: i64| -> i64 x + n
}

fn main() {
    println!("📘 Tutorial: 09_higher_order_functions.fp");
    println!("🧭 Focus: Higher-order functions: passing functions as arguments and closures");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    // Pass function as argument
    println!("Generic operations:");
    apply_i64(10, 20, add_i64);
    apply_f64(1.5, 2.5, add_f64);

    // Conditional with function
    println!("\nConditional:");
    println!("apply_if(true, 5, 3) = {}", apply_if(true, 5, 3, add_i64));
    println!("apply_if(false, 5, 3) = {}", apply_if(false, 5, 3, add_i64));

    println!("\nClosure factory:");
    let add_10 = make_adder(10);
    let added = add_10(5);
    println!("add_10(5) = {}", added);

    let double = |x: i64| -> i64 x * 2;
    let doubled = double(7);
    println!("double(7) = {}", doubled);
}
