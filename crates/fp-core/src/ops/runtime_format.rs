use crate::ast::Value;
use crate::error::Result;

/// Format a runtime string that may contain `{}`-style or `printf`-style (`%`) placeholders.
///
/// This mirrors the behaviour of `println!` while supporting dynamic format strings at runtime.
pub fn format_runtime_string(format_str: &str, values: &[Value]) -> Result<String> {
    let mut result = String::new();
    let mut arg_index = 0usize;
    let mut chars = format_str.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            '{' => {
                if matches!(chars.peek(), Some('{')) {
                    // Escaped brace `{{`
                    chars.next();
                    result.push('{');
                    continue;
                }

                if matches!(chars.peek(), Some('}')) {
                    chars.next();
                    if let Some(value) = values.get(arg_index) {
                        let formatted = format_value_with_spec(value, None)?;
                        result.push_str(&formatted);
                        arg_index += 1;
                    } else {
                        // Not enough arguments; leave the placeholder intact for visibility.
                        result.push_str("{}");
                    }
                } else {
                    // Non-placeholder brace; treat as literal.
                    result.push('{');
                }
            }
            '}' => {
                if matches!(chars.peek(), Some('}')) {
                    chars.next();
                    result.push('}');
                } else {
                    result.push('}');
                }
            }
            '%' => {
                if matches!(chars.peek(), Some('%')) {
                    chars.next();
                    result.push('%');
                    continue;
                }

                // Parse printf-style specifiers such as %d, %05d, %.2f, etc.
                let spec = parse_printf_spec(&mut chars);
                if let Some(value) = values.get(arg_index) {
                    let spec_string = format!("%{}", spec);
                    let formatted = match value {
                        Value::Int(i) => format_with_llvm_int(spec.as_str(), i.value)?,
                        Value::Bool(b) => {
                            let int_val = if b.value { 1 } else { 0 };
                            format_with_llvm_int(spec.as_str(), int_val)?
                        }
                        Value::Decimal(d) => format_with_llvm_float(spec.as_str(), d.value)?,
                        Value::String(s) => {
                            if spec.contains('s') {
                                s.value.clone()
                            } else {
                                format_with_llvm_float(
                                    spec.as_str(),
                                    s.value.parse::<f64>().unwrap_or(0.0),
                                )?
                            }
                        }
                        _ => format_value_with_spec(value, Some(&spec_string))?,
                    };
                    result.push_str(&formatted);
                    arg_index += 1;
                } else {
                    result.push('%');
                    result.push_str(&spec);
                }
            }
            _ => result.push(ch),
        }
    }

    // If there are leftover values, append them space-separated to avoid silently ignoring them.
    if arg_index < values.len() {
        if !result.is_empty() && !result.ends_with(' ') {
            result.push(' ');
        }
        for (idx, value) in values[arg_index..].iter().enumerate() {
            if idx > 0 {
                result.push(' ');
            }
            result.push_str(&format_value_with_spec(value, None)?);
        }
    }

    Ok(result)
}

/// Format a single value with an optional format specification.
///
/// The specification can either be Rust-style (e.g. `:?`) or printf-style starting with `%`.
pub fn format_value_with_spec(value: &Value, spec: Option<&str>) -> Result<String> {
    if let Some(spec_str) = spec {
        if spec_str.starts_with('%') {
            return format_printf_value(value, &spec_str[1..]);
        }

        // Minimal Rust-style support.
        if spec_str == "?" {
            return Ok(format!("{:?}", value));
        }
    }

    Ok(default_value_string(value))
}

fn format_printf_value(value: &Value, spec: &str) -> Result<String> {
    if spec.is_empty() {
        return Ok(default_value_string(value));
    }

    let ty = spec.chars().last().unwrap_or('s');
    let modifiers = &spec[..spec.len().saturating_sub(1)];
    let (width, precision, pad_char, left_align) = parse_printf_modifiers(modifiers);

    let mut formatted = match ty {
        'd' | 'i' | 'u' | 'x' | 'X' => format_integer(value, ty)?,
        'f' | 'F' | 'e' | 'E' => format_float(value, ty, precision)?,
        's' => match value {
            Value::String(s) => s.value.clone(),
            _ => default_value_string(value),
        },
        'c' => match value {
            Value::Int(i) => char::from_u32(i.value as u32)
                .map(|c| c.to_string())
                .unwrap_or_else(|| default_value_string(value)),
            Value::String(s) => s
                .value
                .chars()
                .next()
                .map(|c| c.to_string())
                .unwrap_or_default(),
            _ => default_value_string(value),
        },
        '?' => format!("{:?}", value),
        _ => default_value_string(value),
    };

    if let Some(width) = width {
        formatted = apply_width(formatted, width, pad_char, left_align);
    }

    Ok(formatted)
}

fn format_integer(value: &Value, ty: char) -> Result<String> {
    let int_value = match value {
        Value::Int(i) => i.value,
        Value::Decimal(d) => d.value as i64,
        Value::Bool(b) => {
            if b.value {
                1
            } else {
                0
            }
        }
        other => return Ok(default_value_string(other)),
    };

    let formatted = match ty {
        'd' | 'i' => int_value.to_string(),
        'u' => format!("{}", (int_value as u128) as u64),
        'x' => format!("{:x}", int_value),
        'X' => format!("{:X}", int_value),
        _ => int_value.to_string(),
    };

    Ok(formatted)
}

fn format_float(value: &Value, ty: char, precision: Option<usize>) -> Result<String> {
    let float_value = match value {
        Value::Decimal(d) => d.value,
        Value::Int(i) => i.value as f64,
        other => return Ok(default_value_string(other)),
    };

    let formatted = match ty {
        'f' | 'F' => match precision {
            Some(p) => format!("{:.*}", p, float_value),
            None => float_value.to_string(),
        },
        'e' | 'E' => {
            let lower = match precision {
                Some(p) => format!("{:.*e}", p, float_value),
                None => format!("{:e}", float_value),
            };
            if ty == 'E' {
                lower.to_uppercase()
            } else {
                lower
            }
        }
        _ => float_value.to_string(),
    };

    Ok(formatted)
}

fn format_with_llvm_int(_spec: &str, value: i64) -> Result<String> {
    let buf = format!("{}{}", "", value);
    Ok(buf)
}

fn format_with_llvm_float(_spec: &str, value: f64) -> Result<String> {
    let buf = format!("{}{}", "", value);
    Ok(buf)
}

fn apply_width(text: String, width: usize, pad_char: char, left_align: bool) -> String {
    if text.len() >= width {
        return text;
    }

    let padding_len = width - text.len();
    let padding: String = std::iter::repeat(pad_char).take(padding_len).collect();

    if left_align {
        format!("{}{}", text, padding)
    } else {
        format!("{}{}", padding, text)
    }
}

fn parse_printf_modifiers(spec: &str) -> (Option<usize>, Option<usize>, char, bool) {
    let mut width = None;
    let mut precision = None;
    let mut pad_char = ' ';
    let mut left_align = false;
    let mut digits = String::new();
    let mut precision_digits = String::new();
    let mut seen_dot = false;

    for ch in spec.chars() {
        match ch {
            '-' => left_align = true,
            '0' if !seen_dot && digits.is_empty() => {
                pad_char = '0';
                digits.push(ch);
            }
            '.' => seen_dot = true,
            c if c.is_ascii_digit() => {
                if seen_dot {
                    precision_digits.push(c);
                } else {
                    digits.push(c);
                }
            }
            _ => {}
        }
    }

    if !digits.is_empty() {
        if let Ok(value) = digits.parse::<usize>() {
            if value > 0 {
                width = Some(value);
            }
        }
    }

    if !precision_digits.is_empty() {
        if let Ok(value) = precision_digits.parse::<usize>() {
            precision = Some(value);
        }
    }

    (width, precision, pad_char, left_align)
}

fn parse_printf_spec(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> String {
    let mut spec = String::new();

    while let Some(&next) = chars.peek() {
        spec.push(next);
        chars.next();

        if next.is_ascii_alphabetic() {
            break;
        }
    }

    if spec.is_empty() {
        spec.push('s');
    }

    spec
}

fn default_value_string(value: &Value) -> String {
    match value {
        Value::String(s) => s.value.clone(),
        Value::Int(i) => i.value.to_string(),
        Value::Decimal(d) => d.value.to_string(),
        Value::Bool(b) => b.value.to_string(),
        Value::Unit(_) => "()".to_string(),
        Value::None(_) => "None".to_string(),
        Value::Null(_) => "null".to_string(),
        other => format!("{:?}", other),
    }
}
