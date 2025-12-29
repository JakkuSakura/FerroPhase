#!/usr/bin/env fp run
//! Build blocks with typed quote tokens and expression-driven splice.

fn build_items(flag: bool) -> Vec<quote<item>> {
    if flag {
        [quote<item> { struct Alpha { id: i64 } }]
    } else {
        [quote<item> { struct Beta { id: i64 } }]
    }
}

const {
    splice build_items(true);
}

fn main() {
    let alpha = Alpha { id: 1 };
    let _ = alpha.id;
}
