//! Experimental Zig code generation support for FerroPhase.
//!
//! The goal of this module is to provide a minimal serializer that
//! understands a subset of the FerroPhase AST and produces readable Zig
//! skeletons. The focus is on emitting type definitions and function
//! signatures so that downstream users have a starting point even while the
//! implementation of full expression lowering is still underway.

use eyre::Result;
use fp_core::ast::{
    AstSerializer, Expr, ExprKind, File, FunctionParam, Item, ItemKind, Node, NodeKind,
    StructuralField, Ty, TypeEnum, TypePrimitive, TypeStruct, Value, ValueBool, ValueDecimal,
    ValueInt, ValueString, Visibility,
};

/// Public entry point that implements `AstSerializer` so the CLI can pick it up.
pub struct ZigSerializer;

impl AstSerializer for ZigSerializer {
    fn serialize_node(&self, node: &Node) -> fp_core::error::Result<String> {
        let mut emitter = ZigEmitter::new();
        emitter.emit_node(node)?;
        Ok(emitter.finish())
    }
}

struct ZigEmitter {
    code: String,
    indent: usize,
    wrote_header: bool,
}

impl ZigEmitter {
    fn new() -> Self {
        Self {
            code: String::new(),
            indent: 0,
            wrote_header: false,
        }
    }

    fn finish(mut self) -> String {
        if !self.code.ends_with('\n') {
            self.code.push('\n');
        }
        self.code.trim_end().to_string()
    }

    fn emit_node(&mut self, node: &Node) -> Result<()> {
        match node.kind() {
            NodeKind::File(file) => self.emit_file(file)?,
            NodeKind::Item(item) => self.emit_item(item)?,
            NodeKind::Expr(expr) => {
                self.ensure_header();
                self.push_placeholder_comment(
                    "top-level expressions are not supported for Zig output yet",
                );
                if let Some(rendered) = self.render_expr(expr) {
                    self.push_comment(&format!("Original expression: {}", rendered));
                }
            }
        }
        Ok(())
    }

    fn emit_file(&mut self, file: &File) -> Result<()> {
        self.ensure_header();
        for (index, item) in file.items.iter().enumerate() {
            if index > 0 {
                self.push_blank_line();
            }
            self.emit_item(item)?;
        }
        Ok(())
    }

    fn emit_item(&mut self, item: &Item) -> Result<()> {
        match item.kind() {
            ItemKind::DefStruct(def) => self.emit_struct(def)?,
            ItemKind::DefEnum(def) => self.emit_enum(def)?,
            ItemKind::DefConst(def) => self.emit_const(def)?,
            ItemKind::DefFunction(def) => self.emit_function(def)?,
            ItemKind::Module(module) => self.emit_module(module)?,
            ItemKind::Expr(expr) => {
                self.push_placeholder_comment(
                    "module level expressions are not yet supported in Zig output",
                );
                if let Some(rendered) = self.render_expr(expr) {
                    self.push_comment(&format!("Original expression: {}", rendered));
                }
            }
            other => {
                self.push_placeholder_comment(&format!(
                    "skipping unsupported item kind `{other:?}` in Zig backend",
                ));
            }
        }
        Ok(())
    }

    fn emit_struct(&mut self, def: &fp_core::ast::ItemDefStruct) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(def.visibility);
        self.push_line(&format!(
            "{}const {} = struct {{",
            visibility, def.name.name
        ));
        self.indent += 1;
        if def.value.fields.is_empty() {
            self.push_line("// NOTE: struct has no fields");
        } else {
            for field in &def.value.fields {
                self.emit_struct_field(field);
            }
        }
        self.indent -= 1;
        self.push_line("};");
        Ok(())
    }

    fn emit_enum(&mut self, def: &fp_core::ast::ItemDefEnum) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(def.visibility);
        self.push_line(&format!("{}const {} = enum {{", visibility, def.name.name));
        self.indent += 1;
        if def.value.variants.is_empty() {
            self.push_line("_ = void;");
        } else {
            for variant in &def.value.variants {
                self.push_line(&format!("{},", variant.name.name));
            }
        }
        self.indent -= 1;
        self.push_line("};");
        Ok(())
    }

    fn emit_const(&mut self, def: &fp_core::ast::ItemDefConst) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(def.visibility);
        let ty = def
            .ty_annotation()
            .or(def.ty.as_ref())
            .map(|ty| self.render_type(ty))
            .unwrap_or_else(|| "anytype".to_string());

        let value_expr = self.render_expr(def.value.as_ref());
        match value_expr {
            Some(value) => {
                self.push_line(&format!(
                    "{}const {}: {} = {};",
                    visibility, def.name.name, ty, value
                ));
            }
            None => {
                self.push_line(&format!(
                    "{}const {}: {} = undefined;",
                    visibility, def.name.name, ty
                ));
                self.push_comment("TODO: translate constant initializer");
            }
        }
        Ok(())
    }

    fn emit_function(&mut self, def: &fp_core::ast::ItemDefFunction) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(def.visibility);
        let params = def
            .sig
            .params
            .iter()
            .map(|param| self.render_param(param))
            .collect::<Vec<_>>()
            .join(", ");

        let ret_ty = def
            .sig
            .ret_ty
            .as_ref()
            .map(|ty| self.render_type(ty))
            .unwrap_or_else(|| "void".to_string());

        self.push_line(&format!(
            "{}fn {}({}) {} {{",
            visibility, def.name.name, params, ret_ty
        ));
        self.indent += 1;
        if def.sig.params.is_empty() {
            self.push_comment("TODO: translate function body");
        } else {
            let placeholders = def
                .sig
                .params
                .iter()
                .map(|param| param.name.as_str())
                .collect::<Vec<_>>();
            self.push_line(&format!("_ = .{{ {} }};", placeholders.join(", ")));
            self.push_comment("TODO: translate function body");
        }
        self.push_line("@panic(\"FerroPhase Zig backend: body not generated yet\");");
        self.indent -= 1;
        self.push_line("}");
        Ok(())
    }

    fn emit_module(&mut self, module: &fp_core::ast::Module) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(module.visibility);
        self.push_line(&format!(
            "{}const {} = struct {{",
            visibility, module.name.name
        ));
        self.indent += 1;
        if module.items.is_empty() {
            self.push_comment("empty module");
        } else {
            for (index, item) in module.items.iter().enumerate() {
                if index > 0 {
                    self.push_blank_line();
                }
                self.emit_item(item)?;
            }
        }
        self.indent -= 1;
        self.push_line("};");
        Ok(())
    }

    fn emit_struct_field(&mut self, field: &StructuralField) {
        let ty = self.render_type(&field.value);
        self.push_line(&format!("{}: {},", field.name.name, ty));
    }

    fn render_param(&self, param: &FunctionParam) -> String {
        let ty = param
            .ty_annotation()
            .map(|ty| self.render_type(ty))
            .unwrap_or_else(|| self.render_type(&param.ty));
        format!("{}: {}", param.name.as_str(), ty)
    }

    fn render_visibility(&self, visibility: Visibility) -> &'static str {
        match visibility {
            Visibility::Public => "pub ",
            Visibility::Inherited | Visibility::Private => "",
        }
    }

    fn render_expr(&self, expr: &Expr) -> Option<String> {
        match expr.kind() {
            ExprKind::Value(value) => self.render_value(value),
            _ => None,
        }
    }

    fn render_value(&self, value: &Value) -> Option<String> {
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

    fn render_type(&self, ty: &Ty) -> String {
        match ty {
            Ty::Primitive(TypePrimitive::Int(int_ty)) => match int_ty {
                fp_core::ast::TypeInt::I64 => "i64".to_string(),
                fp_core::ast::TypeInt::U64 => "u64".to_string(),
                fp_core::ast::TypeInt::I32 => "i32".to_string(),
                fp_core::ast::TypeInt::U32 => "u32".to_string(),
                fp_core::ast::TypeInt::I16 => "i16".to_string(),
                fp_core::ast::TypeInt::U16 => "u16".to_string(),
                fp_core::ast::TypeInt::I8 => "i8".to_string(),
                fp_core::ast::TypeInt::U8 => "u8".to_string(),
                fp_core::ast::TypeInt::BigInt => "i128".to_string(),
            },
            Ty::Primitive(TypePrimitive::Decimal(decimal_ty)) => match decimal_ty {
                fp_core::ast::DecimalType::F64 => "f64".to_string(),
                fp_core::ast::DecimalType::F32 => "f32".to_string(),
                _ => "f64".to_string(),
            },
            Ty::Primitive(TypePrimitive::Bool) => "bool".to_string(),
            Ty::Primitive(TypePrimitive::Char) => "u8".to_string(),
            Ty::Primitive(TypePrimitive::String) => "[]const u8".to_string(),
            Ty::Struct(TypeStruct { name, .. }) => name.name.clone(),
            Ty::Enum(TypeEnum { name, .. }) => name.name.clone(),
            Ty::Reference(reference) => self.render_type(&reference.ty),
            Ty::Vec(vec_ty) => {
                let inner = self.render_type(&vec_ty.ty);
                format!("[]const {}", inner)
            }
            Ty::Slice(slice_ty) => {
                let inner = self.render_type(&slice_ty.elem);
                format!("[]const {}", inner)
            }
            Ty::Unit(_) => "void".to_string(),
            _ => "anytype".to_string(),
        }
    }

    fn ensure_header(&mut self) {
        if self.wrote_header {
            return;
        }
        self.push_comment("Generated by FerroPhase Zig backend (experimental)");
        self.push_blank_line();
        self.wrote_header = true;
    }

    fn push_placeholder_comment(&mut self, message: &str) {
        self.ensure_header();
        self.push_comment(message);
    }

    fn push_comment(&mut self, message: &str) {
        for line in message.lines() {
            self.push_line(&format!("// {}", line.trim()));
        }
    }

    fn push_blank_line(&mut self) {
        if self.code.ends_with("\n\n") || self.code.is_empty() {
            return;
        }
        if !self.code.ends_with('\n') {
            self.code.push('\n');
        }
        self.code.push('\n');
    }

    fn push_line(&mut self, line: &str) {
        for _ in 0..self.indent {
            self.code.push_str("    ");
        }
        self.code.push_str(line);
        self.code.push('\n');
    }
}

fn render_float(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{:.1}", value)
    } else {
        value.to_string()
    }
}

#[cfg(test)]
mod tests {
    use fp_core::ast::{self, AstSerializer, Expr, Item, ItemKind, Node, Ty, Value};

    use super::ZigSerializer;

    #[test]
    fn renders_basic_function_stub() {
        let body = Expr::value(Value::unit()).into();
        let mut function = ast::ItemDefFunction::new_simple(ast::Ident::new("main"), body);
        function.sig.ret_ty = Some(Ty::unit());

        let item = Item::new(ItemKind::DefFunction(function));
        let node = Node::from(ast::NodeKind::Item(item));

        let serializer = ZigSerializer;
        let rendered = serializer.serialize_node(&node).expect("serialize");

        assert!(rendered.contains("pub fn main"));
        assert!(rendered.contains("TODO"));
    }
}
