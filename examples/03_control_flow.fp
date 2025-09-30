#!/usr/bin/env fp run
//! Control flow: if/else expressions with const and runtime evaluation
//!
//! Demonstrates:
//! - Const if/else with compile-time evaluation
//! - Nested if/else chains
//! - Boolean logic in conditions
//! - Runtime if/else expressions
//!
//! Note: Loop/break/continue/return statements are parsed but have MIR generation bugs

fn main() {
    // Const if/else chains
    const TEMP: i64 = 25;
    const WEATHER: &str = if TEMP > 30 {
        "hot"
    } else if TEMP > 20 {
        "warm"
    } else {
        "cold"
    };
    println!("{}°C is {}", TEMP, WEATHER);

    // Boolean logic
    const IS_SUNNY: bool = true;
    const IS_WARM: bool = TEMP > 20;
    const ACTIVITY: &str = if IS_SUNNY && IS_WARM {
        "outdoor"
    } else {
        "indoor"
    };
    println!("Suggested: {}", ACTIVITY);

    // Nested conditions
    const SCORE: i64 = 85;
    const GRADE: &str = if SCORE >= 90 {
        "A"
    } else if SCORE >= 80 {
        "B"
    } else if SCORE >= 70 {
        "C"
    } else {
        "F"
    };
    println!("Score {} = grade {}", SCORE, GRADE);

    // Runtime if/else
    let value = 42;
    let category = if value > 50 {
        "high"
    } else if value > 25 {
        "medium"
    } else {
        "low"
    };
    println!("Value {} is {}", value, category);

}
