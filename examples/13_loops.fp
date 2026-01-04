#!/usr/bin/env fp run
//! Loop constructs: while, for, and loop.
//! Demonstrates iteration, break, continue, and loop expressions.

fn factorial(n: i64) -> i64 {
    let mut result = 1;
    let mut i = 1;
    while i <= n {
        result *= i;
        i += 1;
    }
    result
}

const fn const_factorial(n: i64) -> i64 {
    if n <= 1 {
        1
    } else {
        n * const_factorial(n - 1)
    }
}

fn sum_range(start: i64, end: i64) -> i64 {
    let mut sum = 0;
    for i in start..end {
        sum += i;
    }
    sum
}

fn find_first_divisor(n: i64) -> i64 {
    let mut i = 2;
    loop {
        if i * i > n {
            break n; // n is prime
        }
        if n % i == 0 {
            break i; // found divisor
        }
        i += 1;
    }
}

fn sum_even_numbers(limit: i64) -> i64 {
    let mut sum = 0;
    let mut i = 0;
    while i < limit {
        i += 1;
        if i % 2 != 0 {
            continue; // skip odd numbers
        }
        sum += i;
    }
    sum
}

fn main() {
    println!("📘 Tutorial: 13_loops.fp");
    println!("🧭 Focus: Loop constructs: while, for, and loop.");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    println!("=== Loop Constructs ===\n");

    // While loops
    println!("1. While loop - factorial:");
    println!("  5! = {}", factorial(5));
    println!("  7! = {}", factorial(7));

    // For loops
    println!("\n2. For loop - sum range:");
    println!("  sum(1..10) = {}", sum_range(1, 10));
    println!("  sum(5..15) = {}", sum_range(5, 15));

    // Infinite loop with break
    println!("\n3. Loop with break expression:");
    println!("  First divisor of 24: {}", find_first_divisor(24));
    println!("  First divisor of 17: {}", find_first_divisor(17));

    // Continue statement
    println!("\n4. Loop with continue:");
    println!("  Sum of even numbers < 10: {}", sum_even_numbers(10));

    // Nested loops
    println!("\n5. Nested loops:");
    let mut count = 0;
    for i in 1..4 {
        for j in 1..4 {
            count += 1;
            if i == j {
                print!("[{}] ", i);
            }
        }
    }
    println!("\n  Iterations: {}", count);

    // Const evaluation with recursion
    println!("\n6. Compile-time recursion:");
    const FACTORIAL_CONST: i64 = const_factorial(5);
    println!("  const_factorial(5) = {}", FACTORIAL_CONST);

    println!("\n✓ Loop constructs demonstrated!");
}
