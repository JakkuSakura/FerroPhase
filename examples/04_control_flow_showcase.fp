#!/usr/bin/env fp run
//! Control flow showcase with const evaluation

fn main() {
    // Basic if expressions (working in const evaluation)
    println!("=== If Expression Showcase ===");
    
    const TEST_VALUE: i64 = 42;
    const RESULT: String = if TEST_VALUE > 30 { "high" } else { "low" };
    println!("Value {} is {}", TEST_VALUE, RESULT);
    
    // Nested if expressions
    const SCORE: i64 = 88;
    const GRADE: String = if SCORE >= 90 {
        "A"
    } else if SCORE >= 80 {
        "B"
    } else if SCORE >= 70 {
        "C" 
    } else if SCORE >= 60 {
        "D"
    } else {
        "F"
    };
    println!("Score {} gets grade {}", SCORE, GRADE);
    
    // Boolean logic in conditions
    const MIN_PASS: i64 = 60;
    const HONOR_THRESHOLD: i64 = 85;
    const IS_PASSING: bool = SCORE >= MIN_PASS;
    const IS_HONOR_STUDENT: bool = SCORE >= HONOR_THRESHOLD;
    
    const ACADEMIC_STATUS: String = if IS_PASSING && IS_HONOR_STUDENT {
        "Honor Roll"
    } else if IS_PASSING {
        "Regular Pass"
    } else {
        "Needs Improvement"
    };
    println!("Academic status: {}", ACADEMIC_STATUS);
    
    // Const evaluation with mathematical conditions
    const X: i64 = 15;
    const Y: i64 = 25;
    const RELATIONSHIP: String = if X > Y {
        "X is greater"
    } else if X < Y {
        "Y is greater" 
    } else {
        "X equals Y"
    };
    println!("{} and {}: {}", X, Y, RELATIONSHIP);
    
    // String-based conditional logic
    const NAME: String = "FerroPhase";
    const HAS_FERRO: bool = NAME.contains("Ferro");
    const HAS_PHASE: bool = NAME.contains("Phase");
    
    const ANALYSIS: String = if HAS_FERRO && HAS_PHASE {
        "Contains both Ferro and Phase"
    } else if HAS_FERRO {
        "Contains Ferro only"
    } else if HAS_PHASE {
        "Contains Phase only"
    } else {
        "Contains neither"
    };
    println!("Name '{}' analysis: {}", NAME, ANALYSIS);
    
    println!("Control flow showcase completed!");
}