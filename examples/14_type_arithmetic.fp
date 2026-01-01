#!/usr/bin/env fp run
//! Type arithmetic examples: combining and relating types with operators.
//! These operators are type-level, not value-level.

use fp_rust::t;

type Int = i64;

// "type T = struct { ... }" is written through t! so the compiler treats it as a type expression.
type Foo = t! {
    struct {
        a: Int,
        common: Int,
        foo: Int,
    }
};

type Bar = t! {
    struct {
        common: Int,
        bar: Int,
    }
};

// A + B: combine/merge the structure of A and B.
// For structs, this is field composition: the resulting type has fields from both sides.
type FooPlusBar = t! { Foo + Bar };

// A | B: A or B. The value is one of the two shapes, so you usually pattern-match.
type FooOrBar = t! { Foo | Bar };

// A - B: A without the parts contributed by B.
// For structs, this means removing fields that exist in B from A.
type FooMinusBar = t! { Foo - Bar };

// Another inline struct type alias.
type InlineRecord = t! {
    struct {
        tag: Int,
        value: Int,
    }
};

// Use a type-level "A or B" value by accepting FooOrBar and matching it.
fn describe_union(value: FooOrBar) -> Int {
    match value {
        Foo { a, common, foo } => a + common + foo,
        Bar { common, bar } => common + bar,
    }
}

fn main() {
    FooPlusBar {
        a: 1,
        common: 2,
        foo: 3,
        bar: 4,
    };

    FooMinusBar {
        a: 10,
        foo: 20,
    };

    InlineRecord { tag: 7, value: 42 };

    let left: FooOrBar = Foo { a: 1, common: 2, foo: 3 };
    let right: FooOrBar = Bar { common: 4, bar: 5 };
    println!("{}", describe_union(left));
    println!("{}", describe_union(right));
}
