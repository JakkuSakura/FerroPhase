use super::interpreter::*;
use super::*;

impl LirInterpreter {
    pub(super) fn render_intrinsic(&self, format: &str, args: &[LirValue]) -> LirResult<String> {
        let mut rendered = format.to_string();
        for arg in args {
            let value = self.resolve_runtime_value(arg, &arg.ty)?;
            let text = self.render_typed_value(&value, &arg.ty)?;
            let placeholder = Self::next_format_placeholder(&rendered).ok_or_else(|| {
                VmError::Runtime("intrinsic format has fewer placeholders than arguments".into())
            })?;
            rendered.replace_range(placeholder, &text);
        }
        Ok(rendered)
    }

    fn next_format_placeholder(format: &str) -> Option<std::ops::Range<usize>> {
        let bytes = format.as_bytes();
        let mut index = 0;
        while index < bytes.len() {
            if bytes[index] == b'{' && bytes.get(index + 1) == Some(&b'}') {
                return Some(index..index + 2);
            }
            if bytes[index] != b'%' {
                index += 1;
                continue;
            }
            if bytes.get(index + 1) == Some(&b'%') {
                index += 2;
                continue;
            }
            let mut end = index + 1;
            while let Some(byte) = bytes.get(end) {
                if byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'-' | b'+' | b'#' | b'*')
                {
                    end += 1;
                } else {
                    break;
                }
            }
            return (end > index + 1).then_some(index..end);
        }
        None
    }

    pub(super) fn render_typed_value(&self, value: &Value, ty: &LirType) -> LirResult<String> {
        if matches!(ty, LirType::Ptr(inner) if matches!(inner.as_ref(), LirType::I8)) {
            let Value::Pointer(pointer) = value else {
                return Err(VmError::TypeMismatch {
                    expected: "string pointer".into(),
                    found: format!("{value:?}"),
                });
            };
            let handle = usize::try_from(pointer.value)
                .map_err(|_| VmError::Runtime("negative string pointer".into()))?;
            if let Some(backing) = self.state.objects.get(handle) {
                return match backing {
                    Value::String(string) => Ok(string.value.clone()),
                    Value::Bytes(bytes) => String::from_utf8(bytes.value.as_ref().to_vec())
                        .map_err(|error| {
                            VmError::Runtime(format!("invalid UTF-8 string: {error}"))
                        }),
                    other => Err(VmError::TypeMismatch {
                        expected: "string backing object".into(),
                        found: format!("{other:?}"),
                    }),
                };
            }

            // Native lowering passes the first field of a `&str` fat pointer
            // to printf-style intrinsics.  A global string literal therefore
            // arrives here as a virtual-memory address rather than an object
            // handle.  Read that representation as the NUL-terminated
            // backing bytes used by the native ABI.
            let bytes = self.state.mem.load_c_string(pointer.value as u64)?;
            return String::from_utf8(bytes)
                .map_err(|error| VmError::Runtime(format!("invalid UTF-8 string: {error}")));
        }
        if let LirType::Struct {
            fields,
            name: Some(name),
            ..
        } = ty
        {
            if name == "__slice" && fields.len() == 2 {
                let Value::Tuple(tuple) = value else {
                    return Err(VmError::TypeMismatch {
                        expected: "slice fat pointer".into(),
                        found: format!("{value:?}"),
                    });
                };
                let [Value::Pointer(pointer), length] = tuple.values.as_slice() else {
                    return Err(VmError::TypeMismatch {
                        expected: "slice pointer and length".into(),
                        found: format!("{:?}", tuple.values),
                    });
                };
                let handle = usize::try_from(pointer.value)
                    .map_err(|_| VmError::Runtime("negative string pointer".into()))?;
                let backing = self.state.objects.get(handle).ok_or_else(|| {
                    VmError::Runtime(format!("string handle {handle} is out of range"))
                })?;
                let bytes = match backing {
                    Value::String(string) => string.value.as_bytes().to_vec(),
                    Value::Bytes(bytes) => bytes.value.as_ref().to_vec(),
                    other => {
                        return Err(VmError::TypeMismatch {
                            expected: "string backing object".into(),
                            found: format!("{other:?}"),
                        });
                    }
                };
                let length = match length {
                    Value::UInt(length) => length.value,
                    Value::Int(length) => u64::try_from(length.value)
                        .map_err(|_| VmError::Runtime("negative slice length".into()))?,
                    other => {
                        return Err(VmError::TypeMismatch {
                            expected: "integer slice length".into(),
                            found: format!("{other:?}"),
                        });
                    }
                } as usize;
                let bytes = bytes.get(..length).ok_or_else(|| {
                    VmError::Runtime("slice length exceeds string backing object".into())
                })?;
                return String::from_utf8(bytes.to_vec())
                    .map_err(|error| VmError::Runtime(format!("invalid UTF-8 slice: {error}")));
            }
        }
        Ok(self.render_value(value))
    }

    pub(super) fn render_value(&self, v: &Value) -> String {
        match v {
            Value::Unit(_) => "()".to_string(),
            Value::Bool(b) => b.value.to_string(),
            Value::Int(i) => i.value.to_string(),
            Value::UInt(u) => u.value.to_string(),
            Value::Decimal(d) => d.value.to_string(),
            Value::String(s) => s.value.clone(),
            Value::Null(_) => "null".to_string(),
            Value::Tuple(t) => {
                let items: Vec<String> = t.values.iter().map(|x| self.render_value(x)).collect();
                format!("({})", items.join(", "))
            }
            Value::List(l) => {
                let items: Vec<String> = l.values.iter().map(|x| self.render_value(x)).collect();
                format!("[{}]", items.join(", "))
            }
            Value::Map(m) => {
                let items: Vec<String> = m
                    .entries
                    .iter()
                    .map(|e| {
                        format!(
                            "{}: {}",
                            self.render_value(&e.key),
                            self.render_value(&e.value)
                        )
                    })
                    .collect();
                format!("{{{}}}", items.join(", "))
            }
            _ => format!("{v:?}"),
        }
    }
}
