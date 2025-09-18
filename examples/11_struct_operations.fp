#!/usr/bin/env fp run

// Example demonstrating struct operations and introspection
use fp_core::intrinsic;

// Define a Point struct
struct Point {
    x: i64,
    y: i64,
}

fn main() {
    // Create a Point instance
    let p1 = Point { x: 10, y: 20 };
    
    // Test struct field access
    println!("p1.x = {}", p1.x);
    println!("p1.y = {}", p1.y);
    
    // Test struct introspection
    const POINT_SIZE: i64 = sizeof!(Point);
    const FIELD_COUNT: i64 = field_count!(Point);
    const HAS_X: bool = hasfield!(Point, "x");
    const HAS_Z: bool = hasfield!(Point, "z");
    
    println!("Point size: {} bytes", POINT_SIZE);
    println!("Point field count: {}", FIELD_COUNT);
    println!("Point has field 'x': {}", HAS_X);
    println!("Point has field 'z': {}", HAS_Z);
    
    // Create another Point
    let p2 = Point { x: 5, y: 15 };
    
    // Calculate distance (simple version)
    let dx = p1.x - p2.x;
    let dy = p1.y - p2.y;
    let distance_squared = dx * dx + dy * dy;
    
    println!("Distance squared between points: {}", distance_squared);
}