#!/usr:bin/env fp run
//! Enum variants: unit, tuple, struct variants and discriminants

enum Shape {
    Point,
    Circle(i64),
    Rectangle { w: i64, h: i64 },
}

impl Shape {
    fn describe(&self) -> &'static str {
        match self {
            Shape::Point => "point",
            Shape::Circle(_) => "circle",
            Shape::Rectangle { .. } => "rectangle",
        }
    }
}

enum Value {
    A = 1,
    B = 2,
    C = 5,
}

enum Option<T> {
    Some(T),
    None,
}

impl<T> Option<T> {
    fn unwrap_or(self, default: T) -> T {
        match self {
            Option::Some(v) => v,
            Option::None => default,
        }
    }
}

fn main() {
    println!("📘 Tutorial: 15_enums.fp");
    println!("🧭 Focus: Enum variants: unit, tuple, struct variants and discriminants");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    // Unit, tuple, struct variants
    let point = Shape::Point;
    let circle = Shape::Circle(10);
    let rect = Shape::Rectangle { w: 5, h: 3 };

    println!("shape point -> {}", point.describe());
    println!("shape circle -> {}", circle.describe());
    println!("shape rectangle -> {}", rect.describe());

    // Discriminants
    let val = Value::C;
    println!("discriminant: {}", val as i32);

    // Generic enum
    let some: Option<i64> = Option::Some(42);
    let none: Option<i64> = Option::None;
    println!("unwrap_or(Some(42), 0) = {}", some.unwrap_or(0));
    println!("unwrap_or(None, 99) = {}", none.unwrap_or(99));

    // Const discriminant
    const CODE: i32 = Value::B as i32;
    println!("const: {}", CODE);
}
