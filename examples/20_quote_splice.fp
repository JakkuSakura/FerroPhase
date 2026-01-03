#!/usr/bin/env fp run
//! Quote, splice, and emit: staged code generation with runtime output.

fn apply_ops(const ops: [i32], mut x: i32, limit: i32) -> i32 {
    const {
        for (i, op) in ops.iter().enumerate() {
            // Mix emit! and splice(quote<expr>) to generate different statements.
            if op % 2 == 0 {
                splice(quote<expr> { x = x + op; });
            } else {
                emit! { x = x + op; }
            }

            // Emit a runtime log at each generated step.
            splice(quote<expr> { println!("step {}: {}", i, x); });

            // Splice an early-return check into the runtime code.
            splice(quote<expr> { if x >= limit { return x; } });
        }
    }
    x
}

fn main() {
    let result1 = apply_ops([1, 2, 3, 4], 0, 6);
    println!("result1={}", result1);

    let result2 = apply_ops([5, -2, 7], 10, 30);
    println!("result2={}", result2);
}
