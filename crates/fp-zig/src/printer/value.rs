use fp_core::ast::{Value, ValueBool, ValueDecimal, ValueInt, ValueString};

use super::{utils::render_float, ZigEmitter};

impl ZigEmitter {
    pub(super) fn render_value(&self, value: &Value) -> Option<String> {
        match value {
            Value::Int(ValueInt { value }) => Some(value.to_string()),
            Value::Bool(ValueBool { value }) => Some(value.to_string()),
            Value::Decimal(ValueDecimal { value }) => Some(render_float(*value)),
            Value::String(ValueString { value, .. }) => Some(format!(
                "\"{}\"",
                value
                    .chars()
                    .flat_map(|c| c.escape_default())
                    .collect::<String>()
            )),
            Value::Unit(_) => None,
            Value::Null(_) => Some("null".to_string()),
            Value::Undefined(_) => Some("undefined".to_string()),
            Value::Struct(struct_value) => {
                let mut parts = Vec::new();
                for field in &struct_value.structural.fields {
                    let value = self.render_value(&field.value)?;
                    parts.push(format!(".{} = {}", field.name.name, value));
                }
                Some(format!(".{{ {} }}", parts.join(", ")))
            }
            _ => None,
        }
    }
}
