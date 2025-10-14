pub(super) fn render_float(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{:.1}", value)
    } else {
        value.to_string()
    }
}

pub(super) fn escape_zig_string(raw: &str) -> String {
    raw.chars().flat_map(|c| c.escape_default()).collect()
}
