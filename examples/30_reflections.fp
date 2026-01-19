#!/usr/bin/env fp run
struct S { a: i32, b: f64 }

fn count_fields_plus_n(_s: &S, i: i32) -> usize {
    field_count!(S) + i as usize
}
fn count_fields_plus_1(s: &S) -> usize {
    const N: i32 = 1;
    count_fields_plus_n(s, N) // it will be specialized automatically
}
const fn const_count_fields_plus_1() -> usize {
    const N: i32 = 1;
    // it will be specialized automatically
    // this time, it's automatically a const fn
    field_count!(S) + N as usize
}

fn main() {
    let s = S { a: 10, b: 20.0 };
    let r1 = count_fields_plus_n(&s, 3);
    let r2 = count_fields_plus_1(&s);
    let r3 = const_count_fields_plus_1();
    assert!(r1 == 5);
    assert!(r2 == 3);
    assert!(r3 == 3);

}
