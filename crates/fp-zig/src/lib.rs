//! Experimental Zig code generation support for FerroPhase.
//!
//! The goal of this module is to provide a minimal serializer that
//! understands a subset of the FerroPhase AST and produces readable Zig
//! skeletons. The focus is on emitting type definitions and function
//! signatures so that downstream users have a starting point even while the
//! implementation of full expression lowering is still underway.

use eyre::Result;
use fp_core::ast::FormatTemplatePart;
use fp_core::ast::{
    AstSerializer, BlockStmt, Expr, ExprBlock, ExprIntrinsicCall, ExprInvoke, ExprKind, File,
    FunctionParam, Item, ItemKind, Locator, Node, NodeKind, StructuralField, Ty, TypeEnum,
    TypePrimitive, TypeStruct, Value, ValueBool, ValueDecimal, ValueInt, ValueString, Visibility,
};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicCallPayload};

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
        let body_emitted = self.emit_function_body(def)?;
        if !body_emitted {
            for param in &def.sig.params {
                self.push_line(&format!("_ = {};", param.name.as_str()));
            }
            self.push_comment("TODO: translate function body");
            self.push_line("@panic(\"FerroPhase Zig backend: body not generated yet\");");
        }
        self.indent -= 1;
        self.push_line("}");
        Ok(())
    }

    fn emit_function_body(&mut self, def: &fp_core::ast::ItemDefFunction) -> Result<bool> {
        let body_expr = def.body.as_ref();
        match body_expr.kind() {
            ExprKind::Block(block) => self.emit_block(block),
            _ => {
                if let Some(rendered) = self.render_expr(body_expr) {
                    if rendered.is_empty() {
                        self.push_line("return;");
                    } else {
                        self.push_line(&format!("return {};", rendered));
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        }
    }

    fn emit_block(&mut self, block: &ExprBlock) -> Result<bool> {
        let mut emitted = false;
        for stmt in &block.stmts {
            if self.emit_stmt(stmt)? {
                emitted = true;
            }
        }
        Ok(emitted)
    }

    fn emit_stmt(&mut self, stmt: &BlockStmt) -> Result<bool> {
        match stmt {
            BlockStmt::Let(stmt_let) => {
                if let Some(ident) = stmt_let.pat.as_ident() {
                    if let Some(init) = &stmt_let.init {
                        if let Some(value) = self.render_expr(init) {
                            self.push_line(&format!("var {} = {};", ident.name, value));
                        } else {
                            self.push_comment("TODO: unsupported initializer in Zig backend");
                        }
                    } else {
                        self.push_line(&format!("var {} = undefined;", ident.name));
                    }
                } else {
                    self.push_comment(
                        "TODO: complex patterns in let bindings are not supported yet",
                    );
                }
                Ok(true)
            }
            BlockStmt::Expr(expr_stmt) => {
                let expr = expr_stmt.expr.as_ref();
                if let ExprKind::IntrinsicCall(call) = expr.kind() {
                    if self.emit_intrinsic_statement(call)? {
                        return Ok(true);
                    }
                }

                if let ExprKind::Block(block_expr) = expr.kind() {
                    return self.emit_block(block_expr);
                }

                if let Some(rendered) = self.render_expr(expr) {
                    if expr_stmt.has_value() {
                        if rendered.is_empty() {
                            self.push_line("return;");
                        } else {
                            self.push_line(&format!("return {};", rendered));
                        }
                    } else if !rendered.is_empty() {
                        self.push_line(&format!("{};", rendered));
                    }
                    return Ok(true);
                }

                if expr_stmt.has_value() {
                    self.push_line("return;");
                    return Ok(true);
                }

                self.push_comment("TODO: unsupported expression statement in Zig backend");
                Ok(true)
            }
            BlockStmt::Item(item) => {
                self.emit_item(item.as_ref())?;
                Ok(true)
            }
            BlockStmt::Noop => Ok(false),
            BlockStmt::Any(_) => {
                self.push_comment("TODO: placeholder statements are not supported in Zig backend");
                Ok(true)
            }
        }
    }

    fn emit_intrinsic_statement(&mut self, call: &ExprIntrinsicCall) -> Result<bool> {
        match call.kind() {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                let newline = matches!(call.kind(), IntrinsicCallKind::Println);
                if let Some(rendered) = self.render_print_call(call, newline) {
                    self.push_line(&rendered);
                    return Ok(true);
                }
            }
            IntrinsicCallKind::Return => {
                if let IntrinsicCallPayload::Args { args } = &call.payload {
                    if let Some(expr) = args.first() {
                        if let Some(rendered) = self.render_expr(expr) {
                            if rendered.is_empty() {
                                self.push_line("return;");
                            } else {
                                self.push_line(&format!("return {};", rendered));
                            }
                        } else {
                            self.push_line("return;");
                        }
                    } else {
                        self.push_line("return;");
                    }
                    return Ok(true);
                }
            }
            _ => {}
        }
        Ok(false)
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
            ExprKind::Locator(locator) => Some(self.render_locator(locator)),
            ExprKind::Select(select) => {
                let target = self.render_expr(select.obj.as_ref())?;
                Some(format!("{}.{}", target, select.field.name))
            }
            ExprKind::Invoke(invoke) => self.render_invoke(invoke),
            ExprKind::IntrinsicCall(call) => self.render_intrinsic_expr(call),
            _ => None,
        }
    }

    fn render_locator(&self, locator: &Locator) -> String {
        match locator {
            Locator::Ident(ident) => ident.name.clone(),
            _ => format!("{}", locator).replace("::", "."),
        }
    }

    fn render_invoke(&self, invoke: &ExprInvoke) -> Option<String> {
        use fp_core::ast::ExprInvokeTarget;

        let target = match &invoke.target {
            ExprInvokeTarget::Function(locator) => self.render_locator(locator),
            ExprInvokeTarget::Expr(expr) => self.render_expr(expr.as_ref())?,
            ExprInvokeTarget::Method(select) => {
                let receiver = self.render_expr(select.obj.as_ref())?;
                format!("{}.{}", receiver, select.field.name)
            }
            _ => return None,
        };

        let mut args = Vec::new();
        for arg in &invoke.args {
            args.push(self.render_expr(arg)?);
        }

        Some(format!("{}({})", target, args.join(", ")))
    }

    fn render_intrinsic_expr(&self, _call: &ExprIntrinsicCall) -> Option<String> {
        None
    }

    fn render_print_call(&self, call: &ExprIntrinsicCall, newline: bool) -> Option<String> {
        let (mut format_string, args) = self.render_print_payload(call)?;
        if newline && !format_string.ends_with('\n') {
            format_string.push('\n');
        }
        let literal = format!("\"{}\"", escape_zig_string(&format_string));
        let arg_tuple = if args.is_empty() {
            ".{}".to_string()
        } else {
            format!(".{{ {} }}", args.join(", "))
        };
        Some(format!("std.debug.print({}, {});", literal, arg_tuple))
    }

    fn render_print_payload(&self, call: &ExprIntrinsicCall) -> Option<(String, Vec<String>)> {
        match &call.payload {
            IntrinsicCallPayload::Format { template } => {
                if !template.kwargs.is_empty() {
                    return None;
                }
                let mut fmt = String::new();
                for part in &template.parts {
                    match part {
                        FormatTemplatePart::Literal(literal) => fmt.push_str(literal),
                        FormatTemplatePart::Placeholder(_) => fmt.push_str("{any}"),
                    }
                }
                let mut args = Vec::new();
                for expr in &template.args {
                    args.push(self.render_expr(expr)?);
                }
                Some((fmt, args))
            }
            IntrinsicCallPayload::Args { args } => {
                let mut rendered_args = Vec::new();
                for expr in args {
                    rendered_args.push(self.render_expr(expr)?);
                }

                if args.len() == 1 {
                    if let ExprKind::Value(value) = args[0].kind() {
                        if let Value::String(str_val) = value.as_ref() {
                            return Some((str_val.value.clone(), Vec::new()));
                        }
                    }
                }

                let fmt = if rendered_args.is_empty() {
                    String::new()
                } else {
                    std::iter::repeat("{any}")
                        .take(rendered_args.len())
                        .collect::<Vec<_>>()
                        .join(" ")
                };

                Some((fmt, rendered_args))
            }
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

fn escape_zig_string(raw: &str) -> String {
    raw.chars().flat_map(|c| c.escape_default()).collect()
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
