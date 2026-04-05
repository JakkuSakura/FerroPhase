// Minimal semantic fixture: error paths via Result-like enum.
// Expected output:
// ok: 5
// err: divide by zero

enum DivResult {
    Ok(i64),
    Err(&'static str),
}

fn safe_div(a: i64, b: i64) -> DivResult {
    if b == 0 {
        DivResult::Err("divide by zero")
    } else {
        DivResult::Ok(a / b)
    }
}

fn main() {
    let ok = safe_div(10, 2);
    match ok {
        DivResult::Ok(value) => println!("ok: {}", value),
        DivResult::Err(msg) => println!("err: {}", msg),
    }

    let err = safe_div(10, 0);
    match err {
        DivResult::Ok(value) => println!("ok: {}", value),
        DivResult::Err(msg) => println!("err: {}", msg),
    }
}
