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
    println!("📘 Tutorial: 21_build_blocks.fp");
    println!("🧭 Focus: Build blocks with typed quote tokens and expression-driven splice.");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    let alpha = Alpha { id: 1 };
    println!("build_items(true) -> Alpha {{ id: {} }}", alpha.id);
    let beta = Beta { id: 2 };
    println!("build_items_2(false) -> Beta {{ id: {} }}", beta.id);
    println!("generated types are usable at runtime");
}
