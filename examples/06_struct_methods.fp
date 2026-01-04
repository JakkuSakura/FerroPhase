#!/usr/bin/env fp run
//! Struct methods and field access

pub struct Point {
    x: i64,
    y: i64,
}

impl Point {
    pub fn new(x: i64, y: i64) -> Self {
        Self { x, y }
    }

    pub fn translate(&mut self, dx: i64, dy: i64) {
        self.x += dx;
        self.y += dy;
    }

    pub fn distance2(&self, other: &Self) -> i64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        dx * dx + dy * dy
    }
}

pub struct Rectangle {
    width: i64,
    height: i64,
}

impl Rectangle {
    pub fn new(width: i64, height: i64) -> Self {
        Self { width, height }
    }

    pub fn area(&self) -> i64 {
        self.width * self.height
    }

    pub fn perimeter(&self) -> i64 {
        2 * (self.width + self.height)
    }

    pub fn is_square(&self) -> bool {
        self.width == self.height
    }
}

fn main() {
    println!("📘 Tutorial: 06_struct_methods.fp");
    println!("🧭 Focus: Struct methods and field access");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    println!("=== Struct Operations ===");

    let mut p1 = Point::new(10, 20);
    let p2 = Point::new(5, 15);
    println!("p1 = ({}, {})", p1.x, p1.y);
    println!("p2 = ({}, {})", p2.x, p2.y);

    p1.translate(3, -4);
    println!("p1 after translate = ({}, {})", p1.x, p1.y);
    println!("Distance²(p1, p2) = {}", p1.distance2(&p2));

    let rect = Rectangle::new(10, 5);
    println!("Rectangle: {}×{}", rect.width, rect.height);
    println!("  area = {}", rect.area());
    println!("  perimeter = {}", rect.perimeter());
    println!("  is_square = {}", rect.is_square());
}
