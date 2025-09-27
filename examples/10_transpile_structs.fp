#!/usr/bin/env fp run
//! Example for transpilation testing

fn main() {
    // Test struct introspection and transpilation
    struct Point {
        x: f64,
        y: f64,
    }
    
    struct Color {
        r: u8,
        g: u8,
        b: u8,
    }
    
    // Const evaluation
    const POINT_SIZE: usize = sizeof!(Point);
    const COLOR_SIZE: usize = sizeof!(Color);
    const TOTAL_SIZE: usize = POINT_SIZE + COLOR_SIZE;
    
    // Runtime values
    let origin = Point { x: 0.0, y: 0.0 };
    let red = Color { r: 255, g: 0, b: 0 };
    
    println!("Transpilation Example");
    println!("Point size: {} bytes", POINT_SIZE);
    println!("Color size: {} bytes", COLOR_SIZE);
    println!("Total size: {} bytes", TOTAL_SIZE);
    println!("Origin: ({}, {})", origin.x, origin.y);
    println!("Red: rgb({}, {}, {})", red.r, red.g, red.b);
}