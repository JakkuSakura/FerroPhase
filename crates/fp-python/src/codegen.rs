use std::collections::{HashMap, HashSet};

use eyre::eyre;
use fp_core::ast::{
    self, AstSerializer, BlockStmt, Expr, ExprBlock, ExprIntrinsicCall, ExprInvoke,
    ExprInvokeTarget, ExprKind, ExprStringTemplate, ExprStruct, ExprUnOp, FormatArgRef,
    FormatTemplatePart, FunctionParam, Item, ItemKind, Node, NodeKind, Pattern, Ty, TypeEnum,
    TypePrimitive, TypeStruct, TypeTuple, TypeVec, Value, ValueList, ValueMap, ValueMapEntry,
    ValueStruct, ValueTuple,
};
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::UnOpKind;
use itertools::Itertools;

pub struct PythonSerializer;

impl AstSerializer for PythonSerializer {
    fn serialize_node(&self, node: &Node) -> Result<String> {
        let mut emitter = PythonEmitter::new();
        emitter.visit_node(node)?;
        Ok(emitter.finish())
    }
}

struct PythonEmitter {
    code: String,
    indent: usize,
    needs_dataclass: bool,
    needs_enum: bool,
    needs_typing_any: bool,
    structs: HashMap<String, TypeStruct>,
    seen_structs: HashSet<String>,
    saw_main: bool,
}

impl PythonEmitter {
    fn new() -> Self {
        Self {
            code: String::new(),
            indent: 0,
            needs_dataclass: false,
            needs_enum: false,
            needs_typing_any: false,
            structs: HashMap::new(),
            seen_structs: HashSet::new(),
            saw_main: false,
        }
    }

    fn visit_node(&mut self, node: &Node) -> Result<()> {
        match node.kind() {
            NodeKind::File(file) => {
                for item in &file.items {
                    self.emit_item(item)?;
                }
            }
            NodeKind::Item(item) => self.emit_item(item)?,
            NodeKind::Expr(expr) => {
                if let ExprKind::Block(block) = expr.kind() {
                    self.emit_script_block(block)?;
                } else {
                    let rendered = self.render_expr(expr)?;
                    self.push_line(&rendered);
                }
            }
            NodeKind::Query(_) => {
                self.ensure_blank_line();
                self.push_line("# Query documents are not yet supported for Python output");
            }
            NodeKind::Schema(_) => {
                self.ensure_blank_line();
                self.push_line("# Schema documents are not yet supported for Python output");
            }
            NodeKind::Workspace(_) => {
                self.ensure_blank_line();
                self.push_line("# Workspace snapshots are not supported for Python output");
            }
        }
        Ok(())
    }

    fn emit_item(&mut self, item: &Item) -> Result<()> {
        match item.kind() {
            ItemKind::DefStruct(struct_item) => {
                self.emit_struct(&struct_item.value)?;
            }
            ItemKind::DefEnum(enum_item) => {
                self.emit_enum(&enum_item.value)?;
            }
            ItemKind::DefConst(const_item) => {
                self.emit_const(const_item)?;
            }
            ItemKind::DefFunction(function_item) => {
                self.emit_function(function_item)?;
            }
            ItemKind::Module(module) => {
                for child in &module.items {
                    self.emit_item(child)?;
                }
            }
            ItemKind::Expr(expr) => {
                let rendered = self.render_expr(expr)?;
                self.push_line(&rendered);
            }
            ItemKind::Import(_)
            | ItemKind::DefType(_)
            | ItemKind::DefStatic(_)
            | ItemKind::DeclConst(_)
            | ItemKind::DeclStatic(_)
            | ItemKind::DeclFunction(_)
            | ItemKind::DeclType(_)
            | ItemKind::DefTrait(_)
            | ItemKind::Impl(_)
            | ItemKind::DefStructural(_)
            | ItemKind::Macro(_)
            | ItemKind::Any(_) => {
                // Unsupported in Python output for now.
            }
        }
        Ok(())
    }

    fn emit_struct(&mut self, struct_def: &TypeStruct) -> Result<()> {
        let struct_name = struct_def.name.name.clone();
        if !self.seen_structs.insert(struct_name.clone()) {
            return Ok(());
        }

        self.structs.insert(struct_name.clone(), struct_def.clone());
        self.needs_dataclass = true;

        self.ensure_blank_line();
        self.push_line("@dataclass");
        self.push_line(&format!("class {}:", struct_name));
        self.indent += 1;
        if struct_def.fields.is_empty() {
            self.push_line("pass");
        } else {
            for field in &struct_def.fields {
                let ty = self.render_type(&field.value);
                self.push_line(&format!("{}: {}", field.name.name, ty));
            }
        }
        self.indent -= 1;
        self.push_line("");
        Ok(())
    }

    fn emit_enum(&mut self, enum_def: &TypeEnum) -> Result<()> {
        self.needs_enum = true;
        self.ensure_blank_line();
        self.push_line(&format!("class {}(Enum):", enum_def.name.name));
        self.indent += 1;
        if enum_def.variants.is_empty() {
            self.push_line("...");
        } else {
            for (index, variant) in enum_def.variants.iter().enumerate() {
                self.push_line(&format!("{} = {}", variant.name.name, index));
            }
        }
        self.indent -= 1;
        self.push_line("");
        Ok(())
    }

    fn emit_const(&mut self, const_item: &ast::ItemDefConst) -> Result<()> {
        self.ensure_blank_line();
        let value = self.render_expr(const_item.value.as_ref())?;
        let line = if let Some(ty) = const_item.ty.as_ref() {
            value_with_type(&const_item.name.name, &self.render_type(ty), &value)
        } else {
            format!("{} = {}", const_item.name.name, value)
        };
        self.push_line(&line);
        Ok(())
    }

    fn emit_function(&mut self, func: &ast::ItemDefFunction) -> Result<()> {
        if func.name.name == "main" {
            self.saw_main = true;
        }

        self.ensure_blank_line();
        let params = self.render_params(&func.sig.params)?;
        let ret_annotation = func
            .sig
            .ret_ty
            .as_ref()
            .map(|ty| format!(" -> {}", self.render_type(ty)))
            .unwrap_or_else(|| " -> None".to_string());
        self.start_block(&format!(
            "def {}({}){}:",
            func.name.name, params, ret_annotation
        ));

        if let ExprKind::Block(block) = func.body.as_ref().kind() {
            self.emit_block(block)?;
        } else {
            let rendered = self.render_expr(func.body.as_ref())?;
            self.push_line(&format!("return {}", rendered));
        }

        self.end_block();
        Ok(())
    }

    fn emit_block(&mut self, block: &ExprBlock) -> Result<()> {
        let mut had_content = false;
        for stmt in &block.stmts {
            self.emit_stmt(stmt)?;
            had_content = true;
        }
        if !had_content {
            self.push_line("pass");
        }
        Ok(())
    }

    fn emit_script_block(&mut self, block: &ExprBlock) -> Result<()> {
        for stmt in &block.stmts {
            if let BlockStmt::Item(item) = stmt {
                self.emit_item(item.as_ref())?;
            }
        }

        let mut wrote_main = false;
        for stmt in &block.stmts {
            if matches!(stmt, BlockStmt::Item(_)) {
                continue;
            }

            if !wrote_main {
                if !self.saw_main {
                    self.saw_main = true;
                }
                self.ensure_blank_line();
                self.start_block("def main() -> None:");
                wrote_main = true;
            }
            self.emit_stmt(stmt)?;
        }

        if wrote_main {
            self.end_block();
        }

        Ok(())
    }

    fn emit_stmt(&mut self, stmt: &BlockStmt) -> Result<()> {
        match stmt {
            BlockStmt::Let(stmt_let) => {
                let name = self.render_pattern(&stmt_let.pat);
                if let Some(init) = &stmt_let.init {
                    let value = self.render_expr(init)?;
                    self.push_line(&format!("{} = {}", name, value));
                } else {
                    self.push_line(&format!("{} = None", name));
                }
            }
            BlockStmt::Expr(expr_stmt) => {
                let expr = expr_stmt.expr.as_ref();
                if let ExprKind::IntrinsicCall(call) = expr.kind() {
                    self.emit_intrinsic_statement(call)?;
                } else if let ExprKind::Block(block_expr) = expr.kind() {
                    self.emit_block(block_expr)?;
                } else {
                    let rendered = self.render_expr(expr)?;
                    if expr_stmt.has_value() {
                        self.push_line(&format!("return {}", rendered));
                    } else {
                        self.push_line(&rendered);
                    }
                }
            }
            BlockStmt::Item(item) => self.emit_item(item.as_ref())?,
            BlockStmt::Noop => {}
            BlockStmt::Any(_) => {
                return Err(eyre!(
                    "Normalization cannot process placeholder statements during transpile"
                )
                .into());
            }
        }
        Ok(())
    }

    fn emit_intrinsic_statement(&mut self, call: &ExprIntrinsicCall) -> Result<()> {
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                let rendered_args =
                    if let Some((template, args, kwargs)) = extract_format_call(call) {
                        vec![self.render_format_string(template, args, kwargs)?]
                    } else {
                        call.args
                            .iter()
                            .map(|expr| self.render_expr(expr))
                            .collect::<Result<Vec<_>>>()?
                    };
                let joined = rendered_args.join(", ");
                self.push_line(&format!("print({})", joined));
                Ok(())
            }
            _ => {
                let rendered = self.render_intrinsic_expr(call)?;
                self.push_line(&rendered);
                Ok(())
            }
        }
    }

    fn render_params(&mut self, params: &[FunctionParam]) -> Result<String> {
        if params.is_empty() {
            return Ok(String::new());
        }

        let rendered = params
            .iter()
            .map(|param| {
                let ty = self.render_type(&param.ty);
                Ok(format!("{}: {}", param.name.name, ty))
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(rendered.join(", "))
    }

    fn render_expr(&mut self, expr: &Expr) -> Result<String> {
        match expr.kind() {
            ExprKind::Value(value) => Ok(self.render_value(value.as_ref())),
            ExprKind::Locator(locator) => Ok(self.render_locator(locator)),
            ExprKind::Invoke(invoke) => self.render_invoke(invoke),
            ExprKind::Select(select) => Ok(format!(
                "{}.{}",
                self.render_expr(select.obj.as_ref())?,
                select.field.name
            )),
            ExprKind::Assign(assign) => Ok(format!(
                "{} = {}",
                self.render_expr(assign.target.as_ref())?,
                self.render_expr(assign.value.as_ref())?
            )),
            ExprKind::BinOp(bin_op) => Ok(format!(
                "({} {} {})",
                self.render_expr(bin_op.lhs.as_ref())?,
                render_bin_op(&bin_op.kind),
                self.render_expr(bin_op.rhs.as_ref())?
            )),
            ExprKind::FormatString(format) => Ok(self.render_template_literal(format)),
            ExprKind::Struct(struct_expr) => self.render_struct_literal(struct_expr),
            ExprKind::Array(array) => {
                let values = array
                    .values
                    .iter()
                    .map(|expr| self.render_expr(expr))
                    .collect::<Result<Vec<_>>>()?;
                Ok(format!("[{}]", values.join(", ")))
            }
            ExprKind::ArrayRepeat(repeat) => Ok(format!(
                "[{} for _ in range({})]",
                self.render_expr(repeat.elem.as_ref())?,
                self.render_expr(repeat.len.as_ref())?
            )),
            ExprKind::Tuple(tuple) => {
                let values = tuple
                    .values
                    .iter()
                    .map(|expr| self.render_expr(expr))
                    .collect::<Result<Vec<_>>>()?;
                Ok(format!("({})", values.join(", ")))
            }
            ExprKind::UnOp(unop) => self.render_unary(unop),
            ExprKind::IntrinsicCall(call) => self.render_intrinsic_expr(call),
            ExprKind::Paren(paren) => Ok(format!("({})", self.render_expr(paren.expr.as_ref())?)),
            ExprKind::Reference(reference) => self.render_expr(reference.referee.as_ref()),
            ExprKind::Dereference(deref) => self.render_expr(deref.referee.as_ref()),
            ExprKind::Cast(cast) => self.render_expr(cast.expr.as_ref()),
            ExprKind::Any(_) => Err(eyre!(
                "Normalization cannot process placeholder expressions during transpile"
            )
            .into()),
            other => Err(eyre!("Unsupported expression in transpiler: {:?}", other).into()),
        }
    }

    fn render_unary(&mut self, unop: &ExprUnOp) -> Result<String> {
        let inner = self.render_expr(unop.val.as_ref())?;
        let rendered = match unop.op {
            UnOpKind::Not => format!("not ({inner})"),
            UnOpKind::Neg => format!("-({inner})"),
            UnOpKind::Deref => inner,
            UnOpKind::Any(ref ident) => format!("{}({inner})", ident.as_str()),
        };
        Ok(rendered)
    }

    fn render_invoke(&mut self, invoke: &ExprInvoke) -> Result<String> {
        let target = match &invoke.target {
            ExprInvokeTarget::Function(locator) => self.render_locator(locator),
            ExprInvokeTarget::Method(select) => format!(
                "{}.{}",
                self.render_expr(select.obj.as_ref())?,
                select.field.name
            ),
            ExprInvokeTarget::Expr(inner) => self.render_expr(inner.as_ref())?,
            ExprInvokeTarget::BinOp(kind) => {
                if invoke.args.len() == 2 {
                    let lhs = self.render_expr(&invoke.args[0])?;
                    let rhs = self.render_expr(&invoke.args[1])?;
                    return Ok(format!("({} {} {})", lhs, render_bin_op(kind), rhs));
                } else {
                    return Err(eyre!("Binary operator call expects two arguments").into());
                }
            }
            ExprInvokeTarget::Type(_) => {
                return Err(
                    eyre!("Invoking type objects is not supported for Python output").into(),
                );
            }
            ExprInvokeTarget::Closure(_) => {
                return Err(eyre!("Closure invocation is not supported for Python output").into());
            }
        };

        let args = invoke
            .args
            .iter()
            .map(|arg| self.render_expr(arg))
            .collect::<Result<Vec<_>>>()?
            .join(", ");

        Ok(format!("{}({})", target, args))
    }

    fn render_intrinsic_expr(&mut self, call: &ExprIntrinsicCall) -> Result<String> {
        match call.kind {
            IntrinsicCallKind::Format => {
                if let Some((template, args, kwargs)) = extract_format_call(call) {
                    self.render_format_string(template, args, kwargs)
                } else {
                    Err(eyre!("format intrinsic expects a format template").into())
                }
            }
            IntrinsicCallKind::Len => {
                if !call.args.is_empty() {
                    Ok(format!("len({})", self.render_expr(&call.args[0])?))
                } else {
                    Err(eyre!("len intrinsic expects an argument").into())
                }
            }
            _ => Err(eyre!("Unsupported intrinsic call {:?}", call.kind).into()),
        }
    }

    fn render_struct_literal(&mut self, struct_expr: &ExprStruct) -> Result<String> {
        if let Some(name) = self.extract_struct_name(struct_expr.name.as_ref()) {
            if let Some(struct_def) = self.structs.get(&name).cloned() {
                let mut fields = Vec::new();
                for field_def in &struct_def.fields {
                    let value_expr = struct_expr
                        .fields
                        .iter()
                        .find(|field| field.name.name == field_def.name.name)
                        .and_then(|field| field.value.as_ref());
                    if let Some(expr) = value_expr {
                        fields.push(format!(
                            "{}={}",
                            field_def.name.name,
                            self.render_expr(expr)?
                        ));
                    }
                }
                return Ok(format!("{}({})", name, fields.join(", ")));
            }
        }

        let mut entries = Vec::new();
        for field in &struct_expr.fields {
            if let Some(value) = &field.value {
                entries.push(format!("{}: {}", field.name.name, self.render_expr(value)?));
            }
        }
        Ok(format!("{{{}}}", entries.join(", ")))
    }

    fn render_format_string(
        &mut self,
        format: &ExprStringTemplate,
        args: &[Expr],
        kwargs: &[fp_core::ast::ExprKwArg],
    ) -> Result<String> {
        let mut template = String::from("f\"");
        let mut implicit_index = 0usize;
        for part in &format.parts {
            match part {
                FormatTemplatePart::Literal(text) => {
                    template.push_str(&escape_fstring(text));
                }
                FormatTemplatePart::Placeholder(placeholder) => {
                    template.push_str("{");
                    let expr = match &placeholder.arg_ref {
                        FormatArgRef::Implicit => {
                            let expr = args.get(implicit_index).ok_or_else(|| {
                                eyre!("Missing implicit argument for format placeholder")
                            })?;
                            implicit_index += 1;
                            expr
                        }
                        FormatArgRef::Positional(index) => args
                            .get(*index)
                            .ok_or_else(|| eyre!("Missing positional argument {}", index))?,
                        FormatArgRef::Named(name) => kwargs
                            .iter()
                            .find(|kw| &kw.name == name)
                            .map(|kw| &kw.value)
                            .ok_or_else(|| eyre!("Missing named argument {}", name))?,
                    };
                    template.push_str(&self.render_expr(expr)?);
                    if let Some(spec) = &placeholder.format_spec {
                        template.push(':');
                        template.push_str(&spec.raw);
                    }
                    template.push('}');
                }
            }
        }
        template.push('"');
        Ok(template)
    }

    fn render_template_literal(&self, format: &ExprStringTemplate) -> String {
        let mut template = String::from("f\"");
        let mut implicit_index = 0usize;
        for part in &format.parts {
            match part {
                FormatTemplatePart::Literal(text) => {
                    template.push_str(&escape_fstring(text));
                }
                FormatTemplatePart::Placeholder(placeholder) => {
                    template.push('{');
                    match &placeholder.arg_ref {
                        FormatArgRef::Implicit => {
                            template.push_str(&format!("_arg{}", implicit_index));
                            implicit_index += 1;
                        }
                        FormatArgRef::Positional(index) => {
                            template.push_str(&format!("_arg{}", index));
                        }
                        FormatArgRef::Named(name) => {
                            template.push_str(name);
                        }
                    }
                    if let Some(spec) = &placeholder.format_spec {
                        template.push(':');
                        template.push_str(&spec.raw);
                    }
                    template.push('}');
                }
            }
        }
        template.push('"');
        template
    }

    fn render_locator(&self, locator: &ast::Locator) -> String {
        locator
            .to_string()
            .split("::")
            .map(|segment| segment.to_string())
            .collect::<Vec<_>>()
            .join(".")
    }

    fn render_pattern(&self, pattern: &Pattern) -> String {
        if let Some(ident) = pattern.as_ident() {
            ident.name.clone()
        } else {
            "_".to_string()
        }
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
            Ty::Any(_)
            | Ty::Unknown(_)
            | Ty::Value(_)
            | Ty::Expr(_)
            | Ty::TypeBinaryOp(_)
            | Ty::AnyBox(_)
            | Ty::Type(_) => {
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
            Value::Bool(v) => {
                if v.value {
                    "True".to_string()
                } else {
                    "False".to_string()
                }
            }
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
                let rendered = values
                    .iter()
                    .map(|v| self.render_value(v))
                    .collect::<Vec<_>>();
                format!("[{}]", rendered.join(", "))
            }
            Value::Map(ValueMap { entries }) => {
                let rendered = entries
                    .iter()
                    .map(|ValueMapEntry { key, value }| {
                        format!("{}: {}", self.render_value(key), self.render_value(value))
                    })
                    .collect::<Vec<_>>();
                format!("{{{}}}", rendered.join(", "))
            }
            Value::Struct(ValueStruct { structural, .. }) => {
                let rendered = structural
                    .fields
                    .iter()
                    .map(|field| {
                        format!("{}: {}", field.name.name, self.render_value(&field.value))
                    })
                    .collect::<Vec<_>>();
                format!("{{{}}}", rendered.join(", "))
            }
            Value::Tuple(ValueTuple { values }) => {
                let rendered = values
                    .iter()
                    .map(|v| self.render_value(v))
                    .collect::<Vec<_>>();
                format!("({})", rendered.join(", "))
            }
            _ => {
                self.needs_typing_any = true;
                "None".to_string()
            }
        }
    }

    fn extract_struct_name(&self, expr: &Expr) -> Option<String> {
        match expr.kind() {
            ExprKind::Locator(locator) => locator
                .to_string()
                .split("::")
                .map(|segment| segment.to_string())
                .last(),
            _ => None,
        }
    }

    fn ensure_blank_line(&mut self) {
        if self.code.is_empty() {
            return;
        }
        if !self.code.ends_with('\n') {
            self.code.push('\n');
        }
        if !self.code.ends_with("\n\n") {
            self.code.push('\n');
        }
    }

    fn push_line(&mut self, line: &str) {
        if line.is_empty() {
            self.code.push('\n');
            return;
        }
        for _ in 0..self.indent {
            self.code.push_str("    ");
        }
        self.code.push_str(line);
        self.code.push('\n');
    }

    fn start_block(&mut self, header: &str) {
        self.push_line(header);
        self.indent += 1;
    }

    fn end_block(&mut self) {
        self.indent = self.indent.saturating_sub(1);
        self.push_line("");
    }

    fn finish(self) -> String {
        let mut imports = Vec::new();
        if self.needs_dataclass {
            imports.push("from dataclasses import dataclass".to_string());
        }
        if self.needs_enum {
            imports.push("from enum import Enum".to_string());
        }
        if self.needs_typing_any {
            imports.push("from typing import Any".to_string());
        }

        let mut output = String::new();
        if !imports.is_empty() {
            output.push_str(&imports.join("\n"));
            output.push_str("\n\n");
        }

        output.push_str(self.code.trim_end());
        output.push('\n');

        if self.saw_main {
            output.push_str("\nif __name__ == \"__main__\":\n");
            output.push_str("    main()\n");
        }

        output
    }
}

fn extract_format_call(
    call: &ExprIntrinsicCall,
) -> Option<(&ExprStringTemplate, &[Expr], &[fp_core::ast::ExprKwArg])> {
    let first = call.args.first()?;
    let template = match first.kind() {
        ExprKind::FormatString(format) => format,
        _ => return None,
    };
    Some((template, &call.args[1..], &call.kwargs))
}

fn render_bin_op(kind: &fp_core::ops::BinOpKind) -> &'static str {
    use fp_core::ops::BinOpKind::*;
    match kind {
        Add | AddTrait => "+",
        Sub => "-",
        Mul => "*",
        Div => "/",
        Mod => "%",
        Shl => "<<",
        Shr => ">>",
        And => "and",
        Or => "or",
        BitAnd => "&",
        BitOr => "|",
        BitXor => "^",
        Eq => "==",
        Ne => "!=",
        Lt => "<",
        Le => "<=",
        Gt => ">",
        Ge => ">=",
    }
}

fn escape_fstring(text: &str) -> String {
    text.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
}

fn value_with_type(name: &str, ty: &str, value: &str) -> String {
    format!("{}: {} = {}", name, ty, value)
}
