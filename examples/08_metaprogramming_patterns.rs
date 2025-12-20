pub fn main() -> () {
    pub const FIELD_COUNT: usize = 3;
    pub const TYPE_NAME: &str = "Point3D".to_string();
    pub struct Point3D {
        pub x: i64,
        pub y: i64,
        pub z: i64,
    }
    impl Point3D {
        pub fn type_name() -> &str {
            TYPE_NAME
        }
        pub fn field_count() -> usize {
            FIELD_COUNT
        }
    }
    println!(
        "{} has {} fields",
        Point3D::type_name(),
        Point3D::field_count()
    );
    pub const VARIANT_A: u8 = 1;
    pub const VARIANT_B: u8 = 2;
    pub enum Tag {
        A = VARIANT_A as isize,
        B = VARIANT_B as isize,
    }
    let tag = Tag::A;
    println!("tag discriminant: {}", tag as u8);
}
