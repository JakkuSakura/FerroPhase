#!/usr/bin/env fp run
//! Traits: defining shared behavior with default methods

trait Shape {
    fn area(&self) -> f64;

    fn describe(&self) {
        println!("area={:.2}", self.area());
    }
}

struct Circle {
    radius: f64,
}

impl Shape for Circle {
    fn area(&self) -> f64 {
        3.14159 * self.radius * self.radius
    }
}

struct Rectangle {
    width: f64,
    height: f64,
}

impl Shape for Rectangle {
    fn area(&self) -> f64 {
        self.width * self.height
    }
}

fn print_area<T: Shape>(shape: &T) {
    println!("{:.2}", shape.area());
}

fn main() {
    let circle = Circle { radius: 5.0 };
    let rect = Rectangle { width: 4.0, height: 6.0 };

    // Trait methods
    circle.describe();
    rect.describe();

    // Generic with trait bound
    print_area(&circle);
    print_area(&rect);
}