use std::collections::HashMap;

use fp_core::ast::{
    Ty, TypeEnum, TypePrimitive, TypeStruct, TypeTuple, TypeVec, Value, ValueList, ValueMap,
    ValueMapEntry, ValueStruct, ValueTuple,
};
use fp_core::Result;
use itertools::Itertools;

/// Generate Python source code snippets from FerroPhase AST fragments.
#[derive(Default)]
pub struct PythonGenerator {
    needs_dataclass: bool,
    needs_enum: bool,
    needs_typing_any: bool,
}

impl PythonGenerator {
    pub fn new() -> Self {
        Self::default()
    }

    /// Render a Python module covering the provided structs, enums, and constants.
    pub fn render_module(
        mut self,
        structs: &[TypeStruct],
        enums: &[TypeEnum],
        constants: &HashMap<String, Value>,
    ) -> Result<String> {
        let mut sections = Vec::new();

        if !structs.is_empty() {
            self.needs_dataclass = true;
            sections.push(self.render_structs(structs)?);
        }

        if !enums.is_empty() {
            self.needs_enum = true;
            sections.push(self.render_enums(enums)?);
        }

        if !constants.is_empty() {
            sections.push(self.render_constants(constants));
        }

        sections.push(self.render_main_stub());

        let mut output = String::new();
        let imports = self.render_imports();
        if !imports.is_empty() {
            output.push_str(&imports);
            output.push('\n');
        }

        output.push_str(&sections.into_iter().filter(|s| !s.is_empty()).join("\n\n"));
        if !output.ends_with('\n') {
            output.push('\n');
        }
        Ok(output)
    }

    fn render_imports(&self) -> String {
        let mut lines = Vec::new();
        if self.needs_dataclass {
            lines.push("from dataclasses import dataclass".to_string());
        }
        if self.needs_enum {
            lines.push("from enum import Enum".to_string());
        }
        if self.needs_typing_any {
            lines.push("from typing import Any".to_string());
        }

        if lines.is_empty() {
            String::new()
        } else {
            lines.join("\n") + "\n"
        }
    }

    fn render_structs(&mut self, structs: &[TypeStruct]) -> Result<String> {
        let mut blocks = Vec::new();
        for struct_def in structs {
            let mut block = String::new();
            block.push_str("@dataclass\n");
            block.push_str(&format!("class {}:\n", struct_def.name.name));
            if struct_def.fields.is_empty() {
                block.push_str("    pass\n");
            } else {
                for field in &struct_def.fields {
                    let ty_repr = self.render_type(&field.value);
                    block.push_str(&format!("    {}: {}\n", field.name.name, ty_repr));
                }
            }
            blocks.push(block);
        }
        Ok(blocks.join("\n"))
    }

    fn render_enums(&mut self, enums: &[TypeEnum]) -> Result<String> {
        let mut blocks = Vec::new();
        for enum_def in enums {
            let mut block = String::new();
            block.push_str(&format!("class {}(Enum):\n", enum_def.name.name));
            if enum_def.variants.is_empty() {
                block.push_str("    ...\n");
            } else {
                for variant in &enum_def.variants {
                    block.push_str(&format!("    {}\n", variant.name.name));
                }
            }
            blocks.push(block);
        }
        Ok(blocks.join("\n"))
    }

    fn render_constants(&mut self, constants: &HashMap<String, Value>) -> String {
        let mut entries: Vec<_> = constants.iter().collect();
        entries.sort_by(|(a, _), (b, _)| a.cmp(b));

        entries
            .into_iter()
            .map(|(name, value)| {
                let rendered = self.render_value(value);
                format!("{} = {}", name, rendered)
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn render_main_stub(&self) -> String {
        [
            "def main() -> None:",
            "    print(\"Python output\")",
            "",
            "if __name__ == \"__main__\":",
            "    main()",
        ]
        .join("\n")
    }

    fn render_type(&mut self, ty: &Ty) -> String {
        match ty {
            Ty::Primitive(prim) => match prim {
                TypePrimitive::Bool => "bool".to_string(),
                TypePrimitive::String => "str".to_string(),
                TypePrimitive::Int(_) => "int".to_string(),
                TypePrimitive::Decimal(_) => "float".to_string(),
                TypePrimitive::Char => "str".to_string(),
                TypePrimitive::List => {
                    self.needs_typing_any = true;
                    "list[Any]".to_string()
                }
            },
            Ty::Vec(TypeVec { ty }) => {
                let inner = self.render_type(ty);
                format!("list[{}]", inner)
            }
            Ty::Tuple(TypeTuple { types }) => {
                if types.is_empty() {
                    "tuple[()]".to_string()
                } else {
                    let members = types.iter().map(|t| self.render_type(t)).join(", ");
                    format!("tuple[{}]", members)
                }
            }
            Ty::Struct(struct_ty) => struct_ty.name.name.clone(),
            Ty::Enum(enum_ty) => enum_ty.name.name.clone(),
            Ty::Reference(reference) => self.render_type(&reference.ty),
            Ty::Unit(_) => "None".to_string(),
            Ty::Any(_) | Ty::Unknown(_) => {
                self.needs_typing_any = true;
                "Any".to_string()
            }
            Ty::Value(_) | Ty::Expr(_) | Ty::AnyBox(_) | Ty::Type(_) => {
                self.needs_typing_any = true;
                "Any".to_string()
            }
            _ => {
                self.needs_typing_any = true;
                "Any".to_string()
            }
        }
    }

    fn render_value(&mut self, value: &Value) -> String {
        match value {
            Value::Int(v) => v.value.to_string(),
            Value::Bool(v) => if v.value { "True" } else { "False" }.to_string(),
            Value::Decimal(v) => v.value.to_string(),
            Value::Char(v) => format!("{:?}", v.value),
            Value::String(v) => format!("{:?}", v.value),
            Value::Unit(_) | Value::Null(_) | Value::Undefined(_) | Value::None(_) => {
                "None".to_string()
            }
            Value::Some(some) => self.render_value(&some.value),
            Value::Option(option) => option
                .value
                .as_ref()
                .map(|inner| self.render_value(inner))
                .unwrap_or_else(|| "None".to_string()),
            Value::List(ValueList { values }) => {
                let rendered = values.iter().map(|v| self.render_value(v)).join(", ");
                format!("[{}]", rendered)
            }
            Value::Map(ValueMap { entries }) => self.render_map(entries),
            Value::Tuple(ValueTuple { values }) => {
                let rendered = values.iter().map(|v| self.render_value(v)).join(", ");
                if values.len() == 1 {
                    format!("({},)", rendered)
                } else {
                    format!("({})", rendered)
                }
            }
            Value::Struct(ValueStruct { ty, structural }) => {
                let assignments = structural
                    .fields
                    .iter()
                    .map(|field| format!("{}={}", field.name.name, self.render_value(&field.value)))
                    .join(", ");
                format!("{}({})", ty.name.name, assignments)
            }
            Value::Structural(structural) => {
                let entries = structural
                    .fields
                    .iter()
                    .map(|field| {
                        format!("{:?}: {}", field.name.name, self.render_value(&field.value))
                    })
                    .join(", ");
                format!("{{{}}}", entries)
            }
            _ => {
                self.needs_typing_any = true;
                "None".to_string()
            }
        }
    }

    fn render_map(&mut self, entries: &[ValueMapEntry]) -> String {
        let rendered = entries
            .iter()
            .map(|ValueMapEntry { key, value }| {
                format!("{}: {}", self.render_value(key), self.render_value(value))
            })
            .join(", ");
        format!("{{{}}}", rendered)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    use fp_core::ast::{DecimalType, Ident, StructuralField};

    fn make_struct() -> TypeStruct {
        TypeStruct {
            name: Ident::new("Point"),
            fields: vec![
                StructuralField::new(
                    Ident::new("x"),
                    Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
                ),
                StructuralField::new(
                    Ident::new("y"),
                    Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
                ),
            ],
        }
    }

    #[test]
    fn renders_basic_dataclass() {
        let code = PythonGenerator::new()
            .render_module(&[make_struct()], &[], &HashMap::new())
            .unwrap();
        assert!(code.contains("@dataclass"));
        assert!(code.contains("class Point"));
        assert!(code.contains("def main"));
    }
}
