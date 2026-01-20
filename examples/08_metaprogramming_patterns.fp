#!/usr/bin/env fp run
//! Metaprogramming: const metadata + quote/splice execution

use std::meta::TypeBuilder;

struct Point3D {
    x: i64,
    y: i64,
    z: i64,
}

// Build a derived type by cloning and adding fields.
type LabeledPoint = const {
    TypeBuilder::from(Point3D)
        .with_field("label", &'static str)
        .build()
};

const SCORE_EXPR: expr = quote<expr> { (2 + 3) * 4 };

const FN_GROUP: [item] = quote<[item]> {
    fn alpha() {}
    fn beta(x: i64) -> i64 { x + 1 }
};

const FN_COUNT: i64 = FN_GROUP.len();

const STEP_STMT: stmt = quote<stmt> {
    let step = 7 * 3;
    println!("stmt splice => step={}", step);
};

const BANNER_ITEM: item = quote<item> {
    struct Banner {
        title: &'static str,
        rank: i64,
    }
};

fn main() {
    println!("📘 Tutorial: 08_metaprogramming_patterns.fp");
    println!("🧭 Focus: Metaprogramming: const metadata + quote/splice execution");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    println!("=== Part 1: Const Metadata ===");
    const FIELD_COUNT: i64 = type(Point3D).fields.len();
    const POINT_NAME: &str = type(Point3D).name;
    const SIZE: i64 = type(Point3D).size;
    const FIELDS = type(Point3D).fields;
    const X_TYPE: &str = type(Point3D).field_type("x").name;

    println!("{} has {} fields (size={})", POINT_NAME, FIELD_COUNT, SIZE);
    println!("x type: {}", X_TYPE);
    println!("fields:");
    for field in FIELDS.iter() {
        println!("  {}: {}", field.name, field.ty.name);
    }

    let p = Point3D { x: 1, y: 2, z: 3 };
    println!("point=({}, {}, {})", p.x, p.y, p.z);

    let lp = LabeledPoint {
        x: 4,
        y: 5,
        z: 6,
        label: "origin",
    };
    println!("labeled=({}, {}, {}, {})", lp.x, lp.y, lp.z, lp.label);

    println!("");
    println!("=== Part 2: Execute Quoted Code ===");
    let score = splice(SCORE_EXPR);
    println!("expr splice => {}", score);

    println!("quote<[item]> count => {}", FN_COUNT);

    splice(STEP_STMT);

    splice(BANNER_ITEM);
    let banner = Banner { title: "metaprogramming", rank: score };
    println!("item splice => {} #{}", banner.title, banner.rank);
}
