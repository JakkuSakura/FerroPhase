pub fn main() -> () {
    pub struct Data {
        pub a: i64,
        pub b: i64,
        pub c: [u8; 16],
    }
    pub const SIZE: usize = 24;
    pub const FIELDS: usize = 3;
    pub const HAS_A: bool = true;
    pub const HAS_X: bool = false;
    println!("sizeof={}, fields={}", SIZE, FIELDS);
    println!("has_a={}, has_x={}", HAS_A, HAS_X);
    pub const MAX_SIZE: usize = 64;
    pub const SIZE_OK: bool = true;
    pub const IS_ALIGNED: bool = true;
    println!("size_ok={}, aligned={}", SIZE_OK, IS_ALIGNED);
    pub const MODE: &str = "optimized".to_string();
    println!("mode: {}", MODE);
}
