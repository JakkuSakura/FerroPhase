#!/usr/bin/env fp run
//! Build blocks with typed quote tokens and expression-driven splice.

const fn build_items(flag: bool) -> quote<item> {
    quote<item> {
        if flag {
            struct Alpha {
                id: i64
            }
        } else {
            struct Beta {
                id: i64
            }
        }
    }
}

splice build_items(true);


fn main() {
    let alpha = Alpha { id: 1 };
    let _ = alpha.id;
}
