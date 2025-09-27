#!/usr/bin/env fp run
//! Demonstrates that multiple diagnostics can be collected without aborting evaluation

// This import doesn't exist - should generate error but continue
use nonexistent::module::Function;

// Another bad import - should generate another error
use another::bad::import;  

// This should still work despite the errors above
fn main() {
    let x = 42;
    println!("Hello, world! x = {}", x);
}

// Valid struct definition - should work
struct Point {
    x: i64,
    y: i64,
}

// Function using invalid import - should generate error but continue  
fn use_bad_import() {
    let f = Function::new(); // This references the nonexistent import
}

// Valid function - should work
fn valid_function() {
    let p = Point { x: 1, y: 2 };
    println!("Point: ({}, {})", p.x, p.y);
}
