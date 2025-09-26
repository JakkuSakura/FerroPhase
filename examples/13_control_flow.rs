#!/usr/bin/env fp run
//! Control flow demonstration with loops, break, and continue

fn main() {
    // Simple while loop with counter
    const MAX_COUNT: i64 = 5;
    let mut counter: i64 = 0;
    
    println!("=== While Loop ===");
    while counter < MAX_COUNT {
        println!("Counter: {}", counter);
        counter = counter + 1;
    }
    
    println!("Final counter: {}", counter);
    
    // Loop with break
    println!("\n=== Infinite Loop with Break ===");
    let mut i: i64 = 0;
    loop {
        if i >= 3 {
            break;
        }
        println!("Loop iteration: {}", i);
        i = i + 1;
    }
    
    // Loop with continue
    println!("\n=== Loop with Continue ===");
    let mut j: i64 = 0;
    while j < 6 {
        j = j + 1;
        if j == 2 || j == 4 {
            continue;
        }
        println!("Not 2 or 4: {}", j);
    }
    
    // Nested loops
    println!("\n=== Nested Loops ===");
    let mut x: i64 = 0;
    while x < 3 {
        let mut y: i64 = 0;
        while y < 2 {
            println!("x={}, y={}", x, y);
            y = y + 1;
        }
        x = x + 1;
    }
    
    println!("\nControl flow demo completed!");
}