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

// A & B: A and B at the same time (the value must satisfy both types).
// For structs, this is the overlap: only fields common to both sides.
// For traits, it is often the most meaningful: it expresses "implements both traits".
type FooAndBar = t! { Foo & Bar };

// Another inline struct type alias.
type InlineRecord = t! {
    struct {
        tag: Int,
        value: Int,
    }
};

// Value-in-type example: array lengths are values embedded in types.
type Int4 = t! { [Int; 4] };

// Use a type-level "A or B" value by accepting FooOrBar and matching it.
fn describe_union(value: FooOrBar) -> Int {
    match value {
        Foo { a, common, foo } => a + common + foo,
        Bar { common, bar } => common + bar,
    }
}

// Function type that consumes a FooOrBar.
type FooOrBarFn = fn(FooOrBar) -> Int;

type LiteralInt = t! { 42i64 };
type LiteralBool = t! { true };
type LiteralStr = t! { "hello" };
type LiteralUnit = t! { () };
type LiteralNull = t! { null };
type LiteralStrEnum = t! { "red" | "green" | "blue" };
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
    let handler: FooOrBarFn = describe_union;
    println!("{}", handler(left));
    println!("{}", handler(right));

    let _ints: Int4 = [1, 2, 3, 4];
}

// Proposed operators (not yet implemented in the language; kept as code for syntax reference).
// This block will not compile until the parser and type checker support them.
fn proposed_syntax_examples(existing_foo: Foo, value: FooOrBar) {
    type FooMaybe = Foo?;


    fn print_display<T: Display - Clone>(value: T) {
        println!("{}", value);
    }

    let _ = value;
}
