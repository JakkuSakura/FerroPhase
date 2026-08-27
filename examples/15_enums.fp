#!/usr/bin/env fp interpret
//! Enum variants: unit, tuple, struct variants and discriminants

enum Shape {
    Point,
    Circle(i64),
    Rectangle { w: i64, h: i64 },
}

impl Shape {
    fn describe(&self) -> &str {
        match *self {
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

fn value_code(value: Value) -> i64 {
    match value {
        Value::A => 1,
        Value::B => 2,
        Value::C => 5,
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
    println!("discriminant: {}", value_code(val));

    // Const discriminant
    const CODE: i64 = 2;
    println!("const: {}", CODE);
}
