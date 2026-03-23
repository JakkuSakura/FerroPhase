pub fn that(condition: bool) {
    if !condition {
        panic("assertion failed");
    }
}

pub fn eq_str(lhs: &str, rhs: &str) {
    if lhs != rhs {
        panic(f"assertion failed: left={lhs}, right={rhs}");
    }
}

pub fn ne_str(lhs: &str, rhs: &str) {
    if lhs == rhs {
        panic(f"assertion failed: both={lhs}");
    }
}

pub fn eq_i64(lhs: i64, rhs: i64) {
    if lhs != rhs {
        panic(f"assertion failed: left={lhs}, right={rhs}");
    }
}

pub fn ne_i64(lhs: i64, rhs: i64) {
    if lhs == rhs {
        panic(f"assertion failed: both={lhs}");
    }
}
