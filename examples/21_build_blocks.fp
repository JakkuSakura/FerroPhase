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
quote fn build_items_2(flag: bool) -> item {
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

splice build_items(true);
splice build_items_2(false);

fn main() {
    let alpha = Alpha { id: 1 };
    let _ = alpha.id;
    let beta = Beta { id: 2 };
    let _ = beta.id;
}
