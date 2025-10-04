#!/usr/bin/env fp run
//! Metaprogramming: using const metadata to drive code generation

fn main() {
    // Const metadata
    const FIELD_COUNT: usize = 3;
    const TYPE_NAME: &str = "Point3D";

    struct Point3D {
        x: i64,
        y: i64,
        z: i64,
    }

    impl Point3D {
        fn type_name() -> &'static str {
            TYPE_NAME
        }

        fn field_count() -> usize {
            FIELD_COUNT
        }
    }

    println!("{} has {} fields",
             Point3D::type_name(),
             Point3D::field_count());

    // Enum discriminants with const
    const VARIANT_A: u8 = 1;
    const VARIANT_B: u8 = 2;

    enum Tag {
        A = VARIANT_A as isize,
        B = VARIANT_B as isize,
    }

    let tag = Tag::A;
    println!("tag discriminant: {}", tag as u8);
}
