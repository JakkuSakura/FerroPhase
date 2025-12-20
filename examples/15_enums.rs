pub enum Shape {
    Point,
    Circle(i64),
    Rectangle { w: i64, h: i64 },
}
impl Shape {
    pub fn describe(&self) -> &'static str {
        match self {
            Shape::Point => "point",
            Shape::Circle(_) => "circle",
            Shape::Rectangle { .. } => "rectangle",
        }
    }
}
pub enum Value {
    A = 1,
    B = 2,
    C = 5,
}
pub enum Option<T> {
    Some(T),
    None,
}
impl<T> Option<T> {
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Option::Some(v) => v,
            Option::None => default,
        }
    }
}
pub fn main() -> () {
    let point = Shape::Point;
    let circle = Shape::Circle(10);
    let rect = Shape::Rectangle { w: 5, h: 3 };
    println!("{}", point.describe());
    println!("{}", circle.describe());
    println!("{}", rect.describe());
    let val = Value::C;
    println!("discriminant: {}", val as i32);
    let some: Option<i64> = Option::Some(42);
    let none: Option<i64> = Option::None;
    println!("{}", some.unwrap_or(0));
    println!("{}", none.unwrap_or(99));
    const CODE: i32 = 2;
    println!("const: {}", CODE);
}
