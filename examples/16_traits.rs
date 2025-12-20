pub trait Shape {
    fn area(&self) -> f64;
    fn describe(&self) -> () {
        println!("area={.2}", self.area());
    }
}
pub struct Circle {
    pub radius: f64,
}
impl Shape for Circle {
    pub fn area(&self) -> f64 {
        3.14159 * self.radius * self.radius
    }
}
pub struct Rectangle {
    pub width: f64,
    pub height: f64,
}
impl Shape for Rectangle {
    pub fn area(&self) -> f64 {
        self.width * self.height
    }
}
pub fn main() -> () {
    let circle = Circle { radius: 5 };
    let rect = Rectangle {
        width: 4,
        height: 6,
    };
    circle.describe();
    rect.describe();
    print_area__spec0(&circle);
    print_area__spec1(&rect);
}
pub fn print_area__spec0(shape: &Circle) -> () {
    println!("{.2}", shape.area());
}
pub fn print_area__spec1(shape: &Rectangle) -> () {
    println!("{.2}", shape.area());
}
