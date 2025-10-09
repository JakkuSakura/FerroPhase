use fp_core::ast::{Value, ValueNone};
use fp_core::ops::{format_runtime_string, format_value_with_spec};

fn v_int(value: i64) -> Value {
    Value::int(value)
}

fn v_float(value: f64) -> Value {
    Value::decimal(value)
}

fn v_bool(value: bool) -> Value {
    Value::bool(value)
}

fn v_string(value: &str) -> Value {
    Value::string(value.to_owned())
}

fn v_none() -> Value {
    Value::None(ValueNone::default())
}

fn v_null() -> Value {
    Value::null()
}

macro_rules! runtime_case {
    ($name:ident, $fmt:expr, [$($value:expr),* $(,)?], $expected:expr) => {
        #[test]
        fn $name() {
            let values: Vec<Value> = vec![$($value),*];
            let actual = format_runtime_string($fmt, &values).unwrap();
            assert_eq!(actual, $expected);
        }
    };
}

macro_rules! value_spec_case {
    ($name:ident, $value:expr, $spec:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let value = $value;
            let actual = format_value_with_spec(&value, $spec).unwrap();
            assert_eq!(actual, $expected);
        }
    };
}

runtime_case!(format_renders_plain_text, "plain", [], "plain");
runtime_case!(
    format_expands_single_brace_value,
    "value {}",
    [v_int(7)],
    "value 7"
);
runtime_case!(
    format_expands_two_brace_values,
    "{} + {}",
    [v_int(1), v_int(2)],
    "1 + 2"
);
runtime_case!(format_handles_escape_double_brace, "{{}}", [], "{}");
runtime_case!(format_keeps_unmatched_brace_literal, "{x", [], "{x");
runtime_case!(
    format_keeps_placeholder_when_argument_missing,
    "{} {}",
    [v_int(1)],
    "1 {}"
);
runtime_case!(
    format_appends_leftover_values,
    "hi",
    [v_int(7), v_int(8)],
    "hi 7 8"
);
runtime_case!(format_handles_percent_escape, "%%", [], "%");
runtime_case!(format_handles_percent_decimal, "%d", [v_int(99)], "99");
runtime_case!(
    format_percent_decimal_zero_pad_fallback,
    "%05d",
    [v_int(12)],
    "12"
);
runtime_case!(
    format_percent_decimal_left_align_fallback,
    "%-4d",
    [v_int(5)],
    "5"
);
runtime_case!(
    format_handles_percent_bool_as_int,
    "%d",
    [v_bool(true)],
    "1"
);
runtime_case!(format_handles_percent_string, "%s", [v_string("hi")], "hi");
runtime_case!(
    format_percent_char_from_int_returns_codepoint,
    "%c",
    [v_int(65)],
    "65"
);
runtime_case!(
    format_percent_char_from_string_returns_zero,
    "%c",
    [v_string("zoo")],
    "0"
);
runtime_case!(
    format_percent_hex_lowercase_returns_decimal,
    "%x",
    [v_int(255)],
    "255"
);
runtime_case!(
    format_percent_hex_uppercase_returns_decimal,
    "%X",
    [v_int(255)],
    "255"
);
runtime_case!(format_handles_percent_float, "%f", [v_float(1.25)], "1.25");
runtime_case!(
    format_preserves_named_placeholder_but_appends_values,
    "{named}",
    [v_int(1)],
    "{named} 1"
);
runtime_case!(
    format_mixes_brace_and_percent,
    "{} %d",
    [v_string("v"), v_int(3)],
    "v 3"
);
runtime_case!(format_keeps_percent_when_argument_missing, "%d", [], "%d");
runtime_case!(format_default_none_value, "{}", [v_none()], "None");
runtime_case!(format_default_null_value, "{}", [v_null()], "null");
runtime_case!(format_default_unit_value, "{}", [Value::unit()], "()");
runtime_case!(format_default_bool_value, "{}", [v_bool(false)], "false");
runtime_case!(format_default_int_value, "{}", [v_int(-3)], "-3");
runtime_case!(format_default_string_value, "{}", [v_string("ok")], "ok");

value_spec_case!(value_spec_percent_s, v_string("hi"), Some("%s"), "hi");
value_spec_case!(value_spec_percent_d, v_int(11), Some("%d"), "11");
value_spec_case!(value_spec_percent_x, v_int(31), Some("%x"), "1f");
value_spec_case!(value_spec_percent_upper_x, v_int(31), Some("%X"), "1F");
value_spec_case!(
    value_spec_percent_u,
    v_int(-1),
    Some("%u"),
    "18446744073709551615"
);
value_spec_case!(
    value_spec_percent_f_precision,
    v_float(2.5),
    Some("%.1f"),
    "2.5"
);
value_spec_case!(value_spec_percent_c_from_int, v_int(65), Some("%c"), "A");
value_spec_case!(
    value_spec_percent_c_from_string,
    v_string("zoo"),
    Some("%c"),
    "z"
);
value_spec_case!(value_spec_default_none, v_none(), None, "None");
value_spec_case!(value_spec_default_null, v_null(), None, "null");
value_spec_case!(value_spec_default_unit, Value::unit(), None, "()");
value_spec_case!(value_spec_unknown_spec, v_string("hi"), Some("%q"), "hi");
value_spec_case!(value_spec_empty_spec, v_int(4), Some(""), "4");
value_spec_case!(
    value_spec_bool_with_percent_d,
    v_bool(true),
    Some("%d"),
    "1"
);
value_spec_case!(value_spec_bool_default, v_bool(false), None, "false");
value_spec_case!(
    value_spec_percent_f_no_precision,
    v_float(0.125),
    Some("%f"),
    "0.125"
);

#[test]
fn value_spec_question_uses_debug_representation() {
    let value = v_int(7);
    let actual = format_value_with_spec(&value, Some("%?")).unwrap();
    assert!(actual.contains("7"));
    assert!(actual.contains("ValueInt"));
}

#[test]
fn value_spec_percent_e_contains_exponent() {
    let value = v_float(1.0);
    let actual = format_value_with_spec(&value, Some("%e")).unwrap();
    assert!(actual.contains('e'));
}
