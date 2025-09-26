#!/usr/bin/env fp run
//! Complete control flow demonstration

fn main() {
    println!("=== FerroPhase Control Flow Features ===");
    
    // 1. Basic if expressions (const evaluation)
    println!("\n1. Const Evaluation If Expressions:");
    const TEMPERATURE: i64 = 25;
    const WEATHER: String = if TEMPERATURE > 30 {
        "hot"
    } else if TEMPERATURE > 20 {
        "warm"
    } else if TEMPERATURE > 10 {
        "cool"
    } else {
        "cold"
    };
    println!("Temperature {}°C is {}", TEMPERATURE, WEATHER);
    
    // 2. Boolean logic in conditions
    println!("\n2. Boolean Logic:");
    const IS_SUNNY: bool = true;
    const IS_WARM: bool = TEMPERATURE > 20;
    const OUTDOOR_ACTIVITY: String = if IS_SUNNY && IS_WARM {
        "Perfect for outdoor activities"
    } else if IS_SUNNY {
        "Sunny but might be cool"
    } else if IS_WARM {
        "Warm but cloudy"
    } else {
        "Stay indoors"
    };
    println!("Weather verdict: {}", OUTDOOR_ACTIVITY);
    
    // 3. String operations in conditions
    println!("\n3. String-based Conditions:");
    const PROJECT_NAME: String = "FerroPhase";
    const VERSION: String = "0.1.0";
    const BUILD_TYPE: String = if VERSION.contains("0.") {
        "Development Build"
    } else {
        "Production Build"
    };
    println!("{} {} - {}", PROJECT_NAME, VERSION, BUILD_TYPE);
    
    // 4. Mathematical conditions
    println!("\n4. Mathematical Conditions:");
    const A: i64 = 8;
    const B: i64 = 12;
    const C: i64 = A + B;
    const TRIANGLE_TYPE: String = if A == B && B == C {
        "Equilateral"
    } else if A == B || B == C || A == C {
        "Isosceles" 
    } else {
        "Scalene"
    };
    println!("Triangle with sides {}, {}, {}: {}", A, B, C, TRIANGLE_TYPE);
    
    // 5. Nested conditions with complex logic
    println!("\n5. Complex Nested Conditions:");
    const SCORE_1: i64 = 85;
    const SCORE_2: i64 = 92;
    const SCORE_3: i64 = 78;
    const AVERAGE: i64 = (SCORE_1 + SCORE_2 + SCORE_3) / 3;
    
    const PERFORMANCE: String = if AVERAGE >= 90 {
        if SCORE_1 >= 85 && SCORE_2 >= 85 && SCORE_3 >= 85 {
            "Excellent - Consistent High Performance"
        } else {
            "Excellent - High Average"
        }
    } else if AVERAGE >= 80 {
        "Good Performance"
    } else if AVERAGE >= 70 {
        "Satisfactory Performance"
    } else {
        "Needs Improvement"
    };
    
    println!("Scores: {}, {}, {} (Average: {})", SCORE_1, SCORE_2, SCORE_3, AVERAGE);
    println!("Assessment: {}", PERFORMANCE);
    
    // 6. Runtime if expressions (let bindings)
    println!("\n6. Runtime If Expressions:");
    let runtime_value: i64 = 42;
    if runtime_value > 50 {
        println!("Runtime value {} is high", runtime_value);
    } else if runtime_value > 25 {
        println!("Runtime value {} is medium", runtime_value);
    } else {
        println!("Runtime value {} is low", runtime_value);
    }
    
    println!("\n=== Control Flow Feature Summary ===");
    println!("✓ Basic if-else expressions");
    println!("✓ Nested if-else chains");  
    println!("✓ Boolean logic operators (&&, ||)");
    println!("✓ String method conditions (.contains())");
    println!("✓ Mathematical comparisons");
    println!("✓ Complex nested conditions");
    println!("✓ Const evaluation support");
    println!("✓ Runtime evaluation support");
    println!("✓ AST→HIR→THIR→MIR→LIR→LLVM compilation");
    
    println!("\nFerroPhase control flow system operational! 🚀");
}