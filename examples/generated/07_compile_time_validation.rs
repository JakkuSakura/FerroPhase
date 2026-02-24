pub fn main() -> () {
    struct Data {
        pub a: i64,
        pub b: i64,
        pub c: [u8; 16],
    }
    const SIZE: usize = 24;
    const FIELDS: usize = 3;
    const HAS_A: bool = true;
    const HAS_X: bool = false;
    println!("sizeof={}, fields={}", SIZE, FIELDS);
    println!("has_a={}, has_x={}", HAS_A, HAS_X);
    const MAX_SIZE: usize = 64;
    const SIZE_OK: bool = true;
    const IS_ALIGNED: bool = true;
    println!("size_ok={}, aligned={}", SIZE_OK, IS_ALIGNED);
    const MODE: &str = "optimized";
    println!("mode: {}", MODE);
}
