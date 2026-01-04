#!/usr/bin/env fp run
//! Type arithmetic examples: combining and relating types with operators.
//! These operators are type-level, not value-level.

use fp_rust::t;
use std::fmt::Display;

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

// Optionality suffix: T? is sugar for Option<T>.
type FooMaybe = Foo?;

// Negative bounds: require Display but explicitly forbid Clone.
fn print_display<T: Display + !Clone>(value: T) {
    println!("display: {}", value);
}

fn describe_optional(value: FooMaybe) -> Int {
    match value {
        Foo { a, common, foo } => a + common + foo,
        null => 0,
    }
}

fn main() {
    println!("📘 Tutorial: 14_type_arithmetic.fp");
    println!("🧭 Focus: Type arithmetic examples: combining and relating types with operators.");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    let base_foo: Foo = Foo { a: 1, common: 2, foo: 3 };
    println!("Foo a={} common={} foo={}", base_foo.a, base_foo.common, base_foo.foo);

    let base_bar: Bar = Bar { common: 4, bar: 5 };
    println!("Bar common={} bar={}", base_bar.common, base_bar.bar);

    // Field spread in struct literals.
    let merged: FooPlusBar = FooPlusBar { ..base_foo, bar: 6 };
    println!(
        "FooPlusBar a={} common={} foo={} bar={}",
        merged.a, merged.common, merged.foo, merged.bar
    );

    let reduced: FooMinusBar = FooMinusBar { a: 10, foo: 20 };
    println!("FooMinusBar a={} foo={}", reduced.a, reduced.foo);

    let overlap: FooAndBar = FooAndBar { common: 99 };
    println!("FooAndBar common={}", overlap.common);

    let record: InlineRecord = InlineRecord { tag: 7, value: 42 };
    println!("InlineRecord tag={} value={}", record.tag, record.value);

    let left: FooOrBar = Foo { a: 2, common: 3, foo: 4 };
    let right: FooOrBar = base_bar;
    let handler: FooOrBarFn = describe_union;
    println!("FooOrBar left sum={}", handler(left));
    println!("FooOrBar right sum={}", handler(right));

    let _ints: Int4 = [1, 2, 3, 4];
    println!("Int4[0]={} Int4[1]={} Int4[2]={} Int4[3]={}", _ints[0], _ints[1], _ints[2], _ints[3]);

    let lit_int: LiteralInt = 42i64;
    let lit_bool: LiteralBool = true;
    let lit_str: LiteralStr = "hello";
    let lit_unit: LiteralUnit = ();
    let lit_null: LiteralNull = null;
    let lit_enum: LiteralStrEnum = "green";
    println!("LiteralInt {}", lit_int);
    println!("LiteralBool {}", lit_bool);
    println!("LiteralStr {}", lit_str);
    println!("LiteralUnit {}", lit_unit);
    println!("LiteralNull {}", lit_null);
    println!("LiteralStrEnum {}", lit_enum);

    let maybe: FooMaybe = Foo { a: 5, common: 6, foo: 7 };
    println!("FooMaybe sum={}", describe_optional(maybe));

    let _ = print_display;
}
