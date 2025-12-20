pub struct Pair<T, U> {
    pub first: T,
    pub second: U,
}
impl<T, U> Pair<T, U> {
    pub fn new(first: T, second: U) -> Self {
        Self {
            first: first,
            second: second,
        }
    }
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
    let pair = Pair::new(42, "hello".to_string());
    println!("({}, {})", pair.first, pair.second);
    println!("max(10, 20) = {}", max__spec0(10, 20));
    println!("max(3.5, 2.1) = {}", max__spec1(3.5, 2.1));
    let some: Option<i64> = Option::Some(100);
    let none: Option<i64> = Option::None;
    println!("{}", some.unwrap_or(0));
    println!("{}", none.unwrap_or(99));
}
pub fn max__spec0(a: i64, b: i64) -> i64 {
    if a > b {
        a
    } else {
        b
    }
}
pub fn max__spec1(a: f64, b: f64) -> f64 {
    if a > b {
        a
    } else {
        b
    }
}
