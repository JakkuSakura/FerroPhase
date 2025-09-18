#!/usr/bin/env fp run

// Example demonstrating actual impl block methods
use fp_core::intrinsic;

// Define a Rectangle struct
struct Rectangle {
    width: i64,
    height: i64,
}

// Implement methods for Rectangle
impl Rectangle {
    // Constructor method (associated function)
    fn new(width: i64, height: i64) -> Rectangle {
        Rectangle { width, height }
    }
    
    // Calculate area (takes ownership)
    fn area(self) -> i64 {
        self.width * self.height
    }
    
    // Calculate perimeter (borrows self)
    fn perimeter(&self) -> i64 {
        2 * (self.width + self.height)
    }
    
    // Check if it's a square (borrows self)
    fn is_square(&self) -> bool {
        self.width == self.height
    }
}

fn main() {
    // Create a rectangle manually (constructor calls not supported yet)
    let rect = Rectangle { width: 10, height: 5 };
    
    println!("Initial rectangle: {}x{}", rect.width, rect.height);
    
    // Test custom methods
    let area = rect.area();
    println!("Area: {}", area);
    
    let perimeter = rect.perimeter();
    println!("Perimeter: {}", perimeter);
    
    let is_square = rect.is_square();
    println!("Is square: {}", is_square);
    
    // Create a square and test
    let square = Rectangle { width: 7, height: 7 };
    println!("Square: {}x{}", square.width, square.height);
    
    let square_area = square.area();
    let square_is_square = square.is_square();
    
    println!("Square area: {}", square_area);
    println!("Is square: {}", square_is_square);
    
    // Test introspection
    const RECT_SIZE: i64 = sizeof!(Rectangle);
    const FIELD_COUNT: i64 = field_count!(Rectangle);
    
    println!("Rectangle size: {} bytes", RECT_SIZE);
    println!("Rectangle field count: {}", FIELD_COUNT);
}