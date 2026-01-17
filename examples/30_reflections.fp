#!/usr/bin/env fp run
fn count_fields_plus_n(s: &Any, i: i32) -> usize {
    let ty = type_info!(s);
    count_fields!(ty) + i as usize
}
fn count_fields_plus_1(s: &Any) -> usize {
    const N: i32 = 1;
    count_fields_plus_n(s, N) // it will be specialized automatically
}
const fn const_count_fields_plus_1(s: Any) -> usize {
    const N: i32 = 1;
    // it will be specialized automatically
    // this time, it's automatically a const fn
    count_fields_plus_n(s, N)
}

fn main() {
    struct S { a: i32, b: f64 }
    let s = S { a: 10, b: 20.0 };
    let r1 = count_fields_plus_n(&s, 3);
    let r2 = count_fields_plus_1(&s);
    let r3 = const_count_fields_plus_1(s);
    assert!(r1 == 5);
    assert!(r2 == 3);
    assert!(r3 == 3);

}