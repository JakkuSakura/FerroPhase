#!/usr/bin/env fp run
//! AoS -> SoA conversion using const-eval type generation and intrinsics.

struct Point {
    x: i64,
    y: i64,
}

const fn build_soa(source: type, name: &str) -> type {
    let mut t = create_struct!(name);
    let count = field_count!(source) as i64;
    let mut idx = 0;
    while idx < count {
        let field_name = field_name_at!(source, idx);
        let field_ty = field_type!(source, field_name);
        t = addfield!(t, field_name, Vec<field_ty>);
        idx = idx + 1;
    }
    t
}

type PointSoA = const { build_soa(Point, "PointSoA") };

const POINT_FIELDS = reflect_fields!(Point);
fn aos_to_soa(points: Vec<Point>) -> PointSoA {
    let mut x = Vec::new();
    let mut y = Vec::new();
    let mut idx = 0;
    while idx < points.len() {
        let p = points[idx];
        x.push(p.x);
        y.push(p.y);
        idx = idx + 1;
    }
    PointSoA { x, y }
}

fn push_point(soa: PointSoA, x: i64, y: i64) -> PointSoA {
    let mut xs = soa.x;
    let mut ys = soa.y;
    xs.push(x);
    ys.push(y);
    PointSoA { x: xs, y: ys }
}

fn main() {
    printf("📘 Tutorial: 32_aos_to_soa.fp\n");
    printf("🧭 Focus: AoS -> SoA conversion with const-eval types\n");
    printf("🧪 What to look for: intrinsic metadata and filled SoA buffers\n");
    printf("✅ Expectation: SoA lengths match input + appended points\n");
    printf("\n");

    const SOA_FIELDS: usize = field_count!(PointSoA);
    const SOA_SIZE: i64 = struct_size!(PointSoA);
    const SOA_NAME: &str = type_name!(PointSoA);
    printf("SoA type: {} fields={} size={}\n", SOA_NAME, SOA_FIELDS, SOA_SIZE);
    printf("Point fields:\n");
    let mut meta_idx = 0;
    while meta_idx < POINT_FIELDS.len() {
        let field = POINT_FIELDS[meta_idx];
        printf("  {}: {}\n", field.name, field.type_name);
        meta_idx = meta_idx + 1;
    }

    let points = Vec::from([
        Point { x: 1, y: 2 },
        Point { x: 3, y: 4 },
        Point { x: 5, y: 6 },
    ]);

    let mut soa = aos_to_soa(points);
    printf("converted: x.len={} y.len={}\n", soa.x.len(), soa.y.len());
    printf("x[0]={} y[0]={}\n", soa.x[0], soa.y[0]);

    soa = push_point(soa, 7, 8);
    soa = push_point(soa, 9, 10);
    printf("after append: x.len={} y.len={}\n", soa.x.len(), soa.y.len());
    printf("x[4]={} y[4]={}\n", soa.x[4], soa.y[4]);
}
