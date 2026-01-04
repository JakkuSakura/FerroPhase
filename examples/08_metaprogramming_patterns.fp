#!/usr/bin/env fp run
//! Metaprogramming: using const metadata to drive code generation

struct Point3D {
    x: i64,
    y: i64,
    z: i64,
}

// Build a derived type by cloning and adding fields.
type LabeledPoint = const {
    let mut t = clone_struct!(Point3D);
    addfield!(t, "label", &'static str);
    t
};

// Emit a compile-time method stub for illustrative codegen.
const _GENERATED: () = const {
    generate_method!("describe", quote<item> {
        fn describe(&self) -> &'static str {
            "generated"
        }
    });
};

fn main() {
    println!("📘 Tutorial: 08_metaprogramming_patterns.fp");
    println!("🧭 Focus: Metaprogramming: using const metadata to drive code generation");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    // Const metadata
    const FIELD_COUNT: i64 = field_count!(Point3D);
    const POINT_NAME: &str = type_name!(Point3D);
    const SIZE: i64 = struct_size!(Point3D);
    const FIELDS = reflect_fields!(Point3D);
    const X_TYPE: &str = type_name!(field_type!(Point3D, "x"));

    println!("{} has {} fields (size={})", POINT_NAME, FIELD_COUNT, SIZE);
    println!("x type: {}", X_TYPE);
    println!("fields:");
    for field in FIELDS.iter() {
        println!("  {}: {}", field.name, field.type_name);
    }

    let p = Point3D { x: 1, y: 2, z: 3 };
    println!("point=({}, {}, {})", p.x, p.y, p.z);

    let lp = LabeledPoint {
        x: 4,
        y: 5,
        z: 6,
        label: "origin",
    };
    println!(
        "labeled=({}, {}, {}, {})",
        lp.x, lp.y, lp.z, lp.label
    );
}
