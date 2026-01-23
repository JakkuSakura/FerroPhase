mod serializer;

use fp_core::ast::{
    File, FunctionParam, Item, ItemKind, Module, Node, NodeKind, StructuralField, Ty, TypeEnum,
    TypeInt, TypePrimitive, TypeStruct, TypeTuple, TypeVec, Value, Visibility,
};
use fp_core::error::Result;

pub use serializer::SyclSerializer;

pub(crate) struct SyclEmitter {
    code: String,
    indent: usize,
    wrote_header: bool,
}

impl SyclEmitter {
    pub(crate) fn new() -> Self {
        Self {
            code: String::new(),
            indent: 0,
            wrote_header: false,
        }
    }

    pub(crate) fn finish(mut self) -> String {
        if !self.code.ends_with('\n') {
            self.code.push('\n');
        }
        self.code.trim_end().to_string()
    }

    pub(crate) fn emit_node(&mut self, node: &Node) -> Result<()> {
        match node.kind() {
            NodeKind::File(file) => self.emit_file(file)?,
            NodeKind::Item(item) => self.emit_item(item)?,
            NodeKind::Expr(_) => {
                self.ensure_header();
                self.push_placeholder_comment(
                    "top-level expressions are not supported for SYCL output yet",
                );
            }
            NodeKind::Query(_) => {
                self.ensure_header();
                self.push_placeholder_comment("query documents are not supported for SYCL output");
            }
            NodeKind::Schema(_) => {
                self.ensure_header();
                self.push_placeholder_comment("schema documents are not supported for SYCL output");
            }
            NodeKind::Workspace(_) => {
                self.ensure_header();
                self.push_placeholder_comment(
                    "workspace snapshots are not supported for SYCL output",
                );
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
            ItemKind::Expr(_) => {
                self.push_placeholder_comment(
                    "module level expressions are not yet supported in SYCL output",
                );
            }
            other => {
                self.push_placeholder_comment(&format!(
                    "skipping unsupported item kind `{other:?}` in SYCL backend",
                ));
            }
        }
        Ok(())
    }

    fn emit_struct(&mut self, def: &fp_core::ast::ItemDefStruct) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(&def.visibility);
        self.push_line(&format!("{}struct {} {{", visibility, def.name.name));
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
        let visibility = self.render_visibility(&def.visibility);
        self.push_line(&format!("{}enum class {} {{", visibility, def.name.name));
        self.indent += 1;
        if def.value.variants.is_empty() {
            self.push_line("_ = 0,");
        } else {
            for (index, variant) in def.value.variants.iter().enumerate() {
                self.push_line(&format!("{} = {},", variant.name.name, index));
            }
        }
        self.indent -= 1;
        self.push_line("};");
        Ok(())
    }

    fn emit_const(&mut self, def: &fp_core::ast::ItemDefConst) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(&def.visibility);
        let ty = def
            .ty_annotation()
            .or(def.ty.as_ref())
            .map(|ty| self.render_type(ty))
            .unwrap_or_else(|| "auto".to_string());

        let value_expr = self.render_expr(def.value.as_ref());
        match value_expr {
            Some(value) => {
                self.push_line(&format!(
                    "{}constexpr {} {} = {};",
                    visibility, ty, def.name.name, value
                ));
            }
            None => {
                self.push_line(&format!(
                    "{}constexpr {} {} = {{}};",
                    visibility, ty, def.name.name
                ));
                self.push_comment("TODO: translate constant initializer");
            }
        }
        Ok(())
    }

    fn emit_function(&mut self, def: &fp_core::ast::ItemDefFunction) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(&def.visibility);
        let params = def
            .sig
            .params
            .iter()
            .map(|param| self.render_param(param))
            .collect::<Vec<_>>()
            .join(", ");

        let mut ret_ty = def
            .sig
            .ret_ty
            .as_ref()
            .map(|ty| self.render_type(ty))
            .unwrap_or_else(|| {
                if def.name.name == "main" {
                    "int".to_string()
                } else {
                    "void".to_string()
                }
            });
        if def.name.name == "main" && ret_ty == "void" {
            ret_ty = "int".to_string();
        }

        self.push_line(&format!(
            "{}{} {}({}) {{",
            visibility, ret_ty, def.name.name, params
        ));
        self.indent += 1;
        if def.name.name == "main" {
            self.push_comment("TODO: translate function body");
            self.push_line("return 0;");
        } else if ret_ty == "void" {
            for param in &def.sig.params {
                self.push_line(&format!("(void){};", param.name.as_str()));
            }
            self.push_comment("TODO: translate function body");
        } else {
            for param in &def.sig.params {
                self.push_line(&format!("(void){};", param.name.as_str()));
            }
            self.push_comment("TODO: translate function body");
            self.push_line("return {};");
        }
        self.indent -= 1;
        self.push_line("}");
        Ok(())
    }

    fn emit_module(&mut self, module: &Module) -> Result<()> {
        self.ensure_header();
        let visibility = self.render_visibility(&module.visibility);
        self.push_line(&format!("{}namespace {} {{", visibility, module.name.name));
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
        self.push_line("}");
        Ok(())
    }

    fn emit_struct_field(&mut self, field: &StructuralField) {
        let ty = self.render_type(&field.value);
        self.push_line(&format!("{} {};", ty, field.name.name));
    }

    fn render_param(&self, param: &FunctionParam) -> String {
        let ty = param
            .ty_annotation()
            .map(|ty| self.render_type(ty))
            .unwrap_or_else(|| self.render_type(&param.ty));
        format!("{} {}", ty, param.name.as_str())
    }

    fn render_type(&self, ty: &Ty) -> String {
        match ty {
            Ty::Primitive(prim) => self.render_primitive(prim),
            Ty::Struct(TypeStruct { name, .. }) => name.name.clone(),
            Ty::Enum(TypeEnum { name, .. }) => name.name.clone(),
            Ty::Tuple(TypeTuple { types }) => {
                let rendered = types
                    .iter()
                    .map(|ty| self.render_type(ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("std::tuple<{}>", rendered)
            }
            Ty::Vec(TypeVec { ty }) => {
                let elem = self.render_type(ty);
                format!("std::vector<{}>", elem)
            }
            Ty::Unit(_) => "void".to_string(),
            Ty::Unknown(_) | Ty::Any(_) | Ty::Nothing(_) => "auto".to_string(),
            _ => "auto".to_string(),
        }
    }

    fn render_primitive(&self, prim: &TypePrimitive) -> String {
        match prim {
            TypePrimitive::Int(int_ty) => self.render_int_type(int_ty),
            TypePrimitive::Decimal(decimal_ty) => match decimal_ty {
                fp_core::ast::DecimalType::F32 => "float".to_string(),
                fp_core::ast::DecimalType::F64 => "double".to_string(),
                fp_core::ast::DecimalType::BigDecimal => "double".to_string(),
                fp_core::ast::DecimalType::Decimal { .. } => "double".to_string(),
            },
            TypePrimitive::Bool => "bool".to_string(),
            TypePrimitive::Char => "char".to_string(),
            TypePrimitive::String => "std::string".to_string(),
            TypePrimitive::List => "std::vector<auto>".to_string(),
        }
    }

    fn render_int_type(&self, int_ty: &TypeInt) -> String {
        match int_ty {
            TypeInt::I8 => "std::int8_t".to_string(),
            TypeInt::I16 => "std::int16_t".to_string(),
            TypeInt::I32 => "std::int32_t".to_string(),
            TypeInt::I64 => "std::int64_t".to_string(),
            TypeInt::U8 => "std::uint8_t".to_string(),
            TypeInt::U16 => "std::uint16_t".to_string(),
            TypeInt::U32 => "std::uint32_t".to_string(),
            TypeInt::U64 => "std::uint64_t".to_string(),
            TypeInt::BigInt => "std::int64_t".to_string(),
        }
    }

    fn render_expr(&self, expr: &fp_core::ast::Expr) -> Option<String> {
        match expr.kind() {
            fp_core::ast::ExprKind::Value(value) => self.render_value(value.as_ref()),
            _ => None,
        }
    }

    fn render_value(&self, value: &Value) -> Option<String> {
        match value {
            Value::Int(v) => Some(v.value.to_string()),
            Value::Decimal(v) => Some(v.value.to_string()),
            Value::Bool(v) => Some(v.value.to_string()),
            Value::Char(v) => Some(format!("'{}'", v.value)),
            Value::String(v) => Some(format!("\"{}\"", v.value)),
            Value::Unit(_) => Some("{}".to_string()),
            _ => None,
        }
    }

    fn render_visibility(&self, visibility: &Visibility) -> &'static str {
        match visibility {
            Visibility::Public | Visibility::Crate | Visibility::Restricted(_) => "",
            Visibility::Inherited | Visibility::Private => "",
        }
    }

    fn ensure_header(&mut self) {
        if self.wrote_header {
            return;
        }
        self.push_comment("Generated by FerroPhase SYCL backend (experimental)");
        self.push_line("#include <sycl/sycl.hpp>");
        self.push_line("#include <cstdint>");
        self.push_line("#include <string>");
        self.push_line("#include <tuple>");
        self.push_line("#include <vector>");
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

#[cfg(test)]
mod tests;
