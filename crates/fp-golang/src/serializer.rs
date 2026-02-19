//! Go source serializer for FerroPhase AST.

use std::collections::BTreeSet;

use fp_core::ast::{
    AstSerializer, BlockStmt, BlockStmtExpr, Expr, ExprBlock, ExprIntrinsicCall, ExprInvoke,
    ExprInvokeTarget, ExprKind, File, Item, ItemDefConst, ItemDefEnum, ItemDefFunction,
    ItemDefStruct, ItemDefType, ItemImport, ItemImportPath, ItemImportRename, ItemImportTree,
    ItemKind, Node, NodeKind, PatternKind, Ty, TypeArray, TypePrimitive, TypeTuple, TypeVec, Value,
    ValueStruct, ValueTuple,
};
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicCallKind;

/// Public entry point used by the CLI transpiler.
#[derive(Clone, Debug)]
pub struct GoSerializer {
    package: String,
}

impl GoSerializer {
    pub fn new(package: impl Into<String>) -> Self {
        Self {
            package: package.into(),
        }
    }
}

impl Default for GoSerializer {
    fn default() -> Self {
        Self::new("main")
    }
}

impl AstSerializer for GoSerializer {
    fn serialize_node(&self, node: &Node) -> Result<String> {
        let mut emitter = GoEmitter::new(self.package.clone());
        emitter.emit_node(node)?;
        Ok(emitter.finish())
    }
}

struct GoEmitter {
    code: String,
    indent: usize,
    package: String,
    imports: BTreeSet<String>,
    needs_fmt: bool,
}

impl GoEmitter {
    fn new(package: String) -> Self {
        Self {
            code: String::new(),
            indent: 0,
            package,
            imports: BTreeSet::new(),
            needs_fmt: false,
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
            NodeKind::File(file) => self.emit_file(file),
            NodeKind::Item(item) => self.emit_file(&File {
                path: Default::default(),
                items: vec![item.clone()],
            }),
            NodeKind::Expr(expr) => {
                self.emit_header();
                if let Some(rendered) = self.render_expr(expr)? {
                    self.push_line(&rendered);
                } else {
                    self.push_comment("unsupported top-level expression");
                }
                Ok(())
            }
            NodeKind::Query(_) => {
                self.emit_header();
                self.push_comment("query documents are not supported for Go output");
                Ok(())
            }
            NodeKind::Schema(_) => {
                self.emit_header();
                self.push_comment("schema documents are not supported for Go output");
                Ok(())
            }
            NodeKind::Workspace(_) => {
                self.emit_header();
                self.push_comment("workspace snapshots are not supported for Go output");
                Ok(())
            }
        }
    }

    fn emit_file(&mut self, file: &File) -> Result<()> {
        self.scan_items_for_fmt(&file.items);
        self.collect_imports(&file.items);
        if self.needs_fmt {
            self.imports.insert("fmt".to_string());
        }

        self.emit_header();
        self.emit_imports();
        for item in &file.items {
            self.emit_item(item)?;
        }
        Ok(())
    }

    fn emit_header(&mut self) {
        if !self.code.is_empty() {
            return;
        }
        self.push_line(&format!("package {}", self.package));
        self.push_blank_line();
    }

    fn emit_imports(&mut self) {
        if self.imports.is_empty() {
            return;
        }
        if self.imports.len() == 1 {
            let value = self.imports.iter().next().unwrap();
            self.push_line(&format!("import \"{}\"", value));
            self.push_blank_line();
            return;
        }

        self.push_line("import (");
        self.indent += 1;
        let imports: Vec<String> = self.imports.iter().cloned().collect();
        for import in imports {
            self.push_line(&format!("\"{}\"", import));
        }
        self.indent -= 1;
        self.push_line(")");
        self.push_blank_line();
    }

    fn emit_item(&mut self, item: &Item) -> Result<()> {
        match item.kind() {
            ItemKind::DefStruct(def) => self.emit_struct(def),
            ItemKind::DefEnum(def) => self.emit_enum(def),
            ItemKind::DefType(def) => self.emit_type_alias(def),
            ItemKind::DefConst(def) => self.emit_const(def),
            ItemKind::DefFunction(def) => self.emit_function(def),
            ItemKind::Module(module) => {
                for child in &module.items {
                    self.emit_item(child)?;
                }
                Ok(())
            }
            ItemKind::Expr(expr) => self.emit_expr_item(expr),
            ItemKind::Import(_) => Ok(()),
            _ => {
                self.push_comment(&format!("unsupported item in Go output: {:?}", item.kind()));
                Ok(())
            }
        }
    }

    fn emit_struct(&mut self, def: &ItemDefStruct) -> Result<()> {
        self.push_line(&format!("type {} struct {{", def.name.name));
        self.indent += 1;
        if def.value.fields.is_empty() {
            self.push_line("// empty struct");
        } else {
            for field in &def.value.fields {
                let ty = self.render_type(&field.value);
                self.push_line(&format!("{} {}", field.name.name, ty));
            }
        }
        self.indent -= 1;
        self.push_line("}");
        self.push_blank_line();
        Ok(())
    }

    fn emit_enum(&mut self, def: &ItemDefEnum) -> Result<()> {
        self.push_line(&format!("type {} int", def.name.name));
        self.push_line("const (");
        self.indent += 1;
        for (index, variant) in def.value.variants.iter().enumerate() {
            let name = &variant.name.name;
            if index == 0 {
                self.push_line(&format!("{} {} = iota", name, def.name.name));
            } else {
                self.push_line(&format!("{}", name));
            }
        }
        if def.value.variants.is_empty() {
            self.push_line("_ = iota");
        }
        self.indent -= 1;
        self.push_line(")");
        self.push_blank_line();
        Ok(())
    }

    fn emit_type_alias(&mut self, def: &ItemDefType) -> Result<()> {
        let ty = self.render_type(&def.value);
        self.push_line(&format!("type {} {}", def.name.name, ty));
        self.push_blank_line();
        Ok(())
    }

    fn emit_const(&mut self, def: &ItemDefConst) -> Result<()> {
        let keyword = if def.mutable == Some(true) {
            "var"
        } else {
            "const"
        };
        let value = match self.render_expr(def.value.as_ref())? {
            Some(rendered) => rendered,
            None => {
                self.push_comment(&format!("unsupported initializer for {}", def.name.name));
                "0".to_string()
            }
        };
        self.push_line(&format!("{} {} = {}", keyword, def.name.name, value));
        Ok(())
    }

    fn emit_function(&mut self, def: &ItemDefFunction) -> Result<()> {
        let params = def
            .sig
            .params
            .iter()
            .map(|param| format!("{} {}", param.name.name, self.render_type(&param.ty)))
            .collect::<Vec<_>>()
            .join(", ");
        let ret = def
            .sig
            .ret_ty
            .as_ref()
            .map(|ty| format!(" {}", self.render_type(ty)))
            .unwrap_or_default();

        self.push_line(&format!("func {}({}){} {{", def.name.name, params, ret));
        self.indent += 1;
        if let ExprKind::Block(block) = def.body.as_ref().kind() {
            self.emit_block(block)?;
        } else if let Some(rendered) = self.render_expr(def.body.as_ref())? {
            self.push_line(&format!("return {}", rendered));
        } else {
            self.push_line("// unsupported function body");
        }
        self.indent -= 1;
        self.push_line("}");
        self.push_blank_line();
        Ok(())
    }

    fn emit_block(&mut self, block: &ExprBlock) -> Result<()> {
        if block.stmts.is_empty() {
            return Ok(());
        }
        for stmt in &block.stmts {
            match stmt {
                BlockStmt::Expr(expr) => self.emit_stmt_expr(expr)?,
                BlockStmt::Let(stmt) => {
                    let name = match &stmt.pat.kind {
                        PatternKind::Ident(ident) => ident.ident.name.clone(),
                        _ => "_".to_string(),
                    };
                    let value = stmt
                        .init
                        .as_ref()
                        .and_then(|expr| self.render_expr(expr).ok().flatten())
                        .unwrap_or_else(|| "0".to_string());
                    self.push_line(&format!("{} := {}", name, value));
                }
                BlockStmt::Item(item) => self.emit_item(item)?,
                BlockStmt::Noop => {}
                BlockStmt::Any(_) => {
                    self.push_comment("unsupported statement in Go output");
                }
            }
        }
        Ok(())
    }

    fn emit_stmt_expr(&mut self, expr: &BlockStmtExpr) -> Result<()> {
        match expr.expr.kind() {
            ExprKind::Return(ret) => {
                if let Some(value) = ret.value.as_ref() {
                    if let Some(rendered) = self.render_expr(value.as_ref())? {
                        self.push_line(&format!("return {}", rendered));
                    } else {
                        self.push_line("return");
                    }
                } else {
                    self.push_line("return");
                }
                Ok(())
            }
            _ => {
                if let Some(rendered) = self.render_expr(expr.expr.as_ref())? {
                    self.push_line(&rendered);
                } else {
                    self.push_comment("unsupported expression statement");
                }
                Ok(())
            }
        }
    }

    fn emit_expr_item(&mut self, expr: &Expr) -> Result<()> {
        if let Some(rendered) = self.render_expr(expr)? {
            self.push_line(&rendered);
        } else {
            self.push_comment("unsupported top-level expression");
        }
        Ok(())
    }

    fn render_expr(&mut self, expr: &Expr) -> Result<Option<String>> {
        match expr.kind() {
            ExprKind::Value(value) => Ok(Some(self.render_value(value))),
            ExprKind::IntrinsicCall(call) => self.render_intrinsic_call(call),
            ExprKind::Invoke(invoke) => self.render_invoke(invoke),
            ExprKind::FormatString(template) => {
                let literal = render_format_template_literal(template);
                Ok(Some(format!("\"{}\"", literal)))
            }
            ExprKind::Block(_) => Ok(None),
            ExprKind::Return(_) => Ok(None),
            _ => Ok(None),
        }
    }

    fn render_intrinsic_call(&mut self, call: &ExprIntrinsicCall) -> Result<Option<String>> {
        match call.kind {
            IntrinsicCallKind::Print => {
                self.needs_fmt = true;
                let args = self.render_call_args(&call.args)?;
                Ok(Some(format!("fmt.Print({})", args)))
            }
            IntrinsicCallKind::Println => {
                self.needs_fmt = true;
                let args = self.render_call_args(&call.args)?;
                Ok(Some(format!("fmt.Println({})", args)))
            }
            IntrinsicCallKind::Format => {
                self.needs_fmt = true;
                let args = self.render_call_args(&call.args)?;
                Ok(Some(format!("fmt.Sprintf({})", args)))
            }
            IntrinsicCallKind::Len => {
                let args = self.render_call_args(&call.args)?;
                Ok(Some(format!("len({})", args)))
            }
            _ => Ok(None),
        }
    }

    fn render_invoke(&mut self, invoke: &ExprInvoke) -> Result<Option<String>> {
        let target = match &invoke.target {
            ExprInvokeTarget::Function(locator) => match locator {
                fp_core::ast::Name::Ident(ident) => ident.name.clone(),
                fp_core::ast::Name::Path(path) => path.join("."),
                _ => return Ok(None),
            },
            _ => return Ok(None),
        };
        let args = self.render_call_args(&invoke.args)?;
        Ok(Some(format!("{}({})", target, args)))
    }

    fn render_call_args(&mut self, args: &[Expr]) -> Result<String> {
        let mut rendered = Vec::new();
        for arg in args {
            if let Some(text) = self.render_expr(arg)? {
                rendered.push(text);
            } else {
                rendered.push("nil".to_string());
            }
        }
        Ok(rendered.join(", "))
    }

    fn render_value(&mut self, value: &Value) -> String {
        match value {
            Value::Int(value) => value.value.to_string(),
            Value::Decimal(value) => value.value.to_string(),
            Value::Bool(value) => value.value.to_string(),
            Value::Char(value) => format!("'{}'", value.value),
            Value::String(value) => format!("\"{}\"", escape_string(&value.value)),
            Value::List(list) => {
                let items = list
                    .values
                    .iter()
                    .map(|item| self.render_value(item))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[]any{{{}}}", items)
            }
            Value::Map(map) => {
                let entries = map
                    .entries
                    .iter()
                    .map(|entry| {
                        let key = self.render_value(&entry.key);
                        let value = self.render_value(&entry.value);
                        format!("{}: {}", key, value)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("map[any]any{{{}}}", entries)
            }
            Value::Tuple(tuple) => self.render_tuple_value(tuple),
            Value::Struct(value) => self.render_struct_value(value),
            _ => "nil".to_string(),
        }
    }

    fn render_tuple_value(&mut self, tuple: &ValueTuple) -> String {
        let fields = tuple
            .values
            .iter()
            .enumerate()
            .map(|(idx, value)| format!("_{}: {}", idx, self.render_value(value)))
            .collect::<Vec<_>>()
            .join(", ");
        format!("struct{{{}}}{{{}}}", render_tuple_fields(tuple), fields)
    }

    fn render_struct_value(&mut self, value: &ValueStruct) -> String {
        let fields = value
            .structural
            .fields
            .iter()
            .map(|field| format!("{}: {}", field.name.name, self.render_value(&field.value)))
            .collect::<Vec<_>>()
            .join(", ");
        format!("{}{{{}}}", value.ty.name.name, fields)
    }

    fn render_type(&mut self, ty: &Ty) -> String {
        match ty {
            Ty::Primitive(primitive) => render_primitive_type(primitive),
            Ty::Struct(struct_ty) => struct_ty.name.name.clone(),
            Ty::Enum(enum_ty) => enum_ty.name.name.clone(),
            Ty::Vec(TypeVec { ty }) => format!("[]{}", self.render_type(ty)),
            Ty::Array(TypeArray { elem, len }) => {
                let len = self.render_array_len(len.as_ref());
                format!("[{}]{}", len, self.render_type(elem))
            }
            Ty::Tuple(TypeTuple { types }) => render_tuple_type(types, self),
            Ty::Reference(reference) => format!("*{}", self.render_type(&reference.ty)),
            Ty::Slice(slice) => format!("[]{}", self.render_type(&slice.elem)),
            Ty::Unit(_) => "struct{}".to_string(),
            Ty::Any(_) | Ty::Unknown(_) => "any".to_string(),
            Ty::Value(value) => format!("{}", self.render_value(&value.value)),
            _ => "any".to_string(),
        }
    }

    fn render_array_len(&self, expr: &Expr) -> String {
        match expr.kind() {
            ExprKind::Value(value) => match value.as_ref() {
                Value::Int(value) => value.value.to_string(),
                _ => "0".to_string(),
            },
            _ => "0".to_string(),
        }
    }

    fn collect_imports(&mut self, items: &[Item]) {
        for item in items {
            match item.kind() {
                ItemKind::Import(import) => {
                    if let Some(path) = import_to_path(import) {
                        self.imports.insert(path);
                    }
                }
                ItemKind::Module(module) => self.collect_imports(&module.items),
                _ => {}
            }
        }
    }

    fn scan_items_for_fmt(&mut self, items: &[Item]) {
        for item in items {
            match item.kind() {
                ItemKind::DefFunction(def) => {
                    if self.expr_uses_fmt(def.body.as_ref()) {
                        self.needs_fmt = true;
                    }
                }
                ItemKind::Expr(expr) => {
                    if self.expr_uses_fmt(expr) {
                        self.needs_fmt = true;
                    }
                }
                ItemKind::Module(module) => self.scan_items_for_fmt(&module.items),
                _ => {}
            }
        }
    }

    fn expr_uses_fmt(&self, expr: &Expr) -> bool {
        match expr.kind() {
            ExprKind::IntrinsicCall(call) => matches!(
                call.kind,
                IntrinsicCallKind::Print | IntrinsicCallKind::Println | IntrinsicCallKind::Format
            ),
            ExprKind::Invoke(invoke) => invoke.args.iter().any(|arg| self.expr_uses_fmt(arg)),
            ExprKind::Block(block) => block.stmts.iter().any(|stmt| match stmt {
                BlockStmt::Expr(expr) => self.expr_uses_fmt(expr.expr.as_ref()),
                BlockStmt::Let(stmt) => stmt
                    .init
                    .as_ref()
                    .map(|expr| self.expr_uses_fmt(expr))
                    .unwrap_or(false),
                BlockStmt::Item(item) => match item.kind() {
                    ItemKind::Expr(expr) => self.expr_uses_fmt(expr),
                    _ => false,
                },
                _ => false,
            }),
            _ => false,
        }
    }

    fn push_line(&mut self, line: &str) {
        for _ in 0..self.indent {
            self.code.push_str("    ");
        }
        self.code.push_str(line);
        self.code.push('\n');
    }

    fn push_comment(&mut self, message: &str) {
        self.push_line(&format!("// {}", message));
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
}

fn render_primitive_type(primitive: &TypePrimitive) -> String {
    match primitive {
        TypePrimitive::Bool => "bool".to_string(),
        TypePrimitive::String => "string".to_string(),
        TypePrimitive::Char => "rune".to_string(),
        TypePrimitive::Int(int_ty) => match int_ty {
            fp_core::ast::TypeInt::I64 => "int64".to_string(),
            fp_core::ast::TypeInt::I32 => "int32".to_string(),
            fp_core::ast::TypeInt::I16 => "int16".to_string(),
            fp_core::ast::TypeInt::I8 => "int8".to_string(),
            fp_core::ast::TypeInt::U64 => "uint64".to_string(),
            fp_core::ast::TypeInt::U32 => "uint32".to_string(),
            fp_core::ast::TypeInt::U16 => "uint16".to_string(),
            fp_core::ast::TypeInt::U8 => "uint8".to_string(),
            fp_core::ast::TypeInt::BigInt => "int".to_string(),
        },
        TypePrimitive::Decimal(decimal) => match decimal {
            fp_core::ast::DecimalType::F32 => "float32".to_string(),
            fp_core::ast::DecimalType::F64 => "float64".to_string(),
            _ => "float64".to_string(),
        },
        TypePrimitive::List => "[]any".to_string(),
    }
}

fn render_tuple_type(types: &[Ty], emitter: &mut GoEmitter) -> String {
    let fields = types
        .iter()
        .enumerate()
        .map(|(idx, ty)| format!("_{} {}", idx, emitter.render_type(ty)))
        .collect::<Vec<_>>()
        .join("; ");
    format!("struct{{{}}}", fields)
}

fn render_tuple_fields(tuple: &ValueTuple) -> String {
    tuple
        .values
        .iter()
        .enumerate()
        .map(|(idx, _)| format!("_{} any", idx))
        .collect::<Vec<_>>()
        .join("; ")
}

fn escape_string(input: &str) -> String {
    input
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
}

fn render_format_template_literal(template: &fp_core::ast::ExprStringTemplate) -> String {
    let mut out = String::new();
    for part in &template.parts {
        match part {
            fp_core::ast::FormatTemplatePart::Literal(text) => out.push_str(text),
            fp_core::ast::FormatTemplatePart::Placeholder(_) => out.push_str("{}"),
        }
    }
    out
}

fn import_to_path(import: &ItemImport) -> Option<String> {
    match &import.tree {
        ItemImportTree::Ident(ident) => Some(ident.name.clone()),
        ItemImportTree::Path(path) => Some(path_to_string(path)),
        ItemImportTree::Rename(rename) => Some(rename.from.name.clone()),
        ItemImportTree::Group(group) => group
            .items
            .iter()
            .filter_map(|item| match item {
                ItemImportTree::Ident(ident) => Some(ident.name.clone()),
                ItemImportTree::Path(path) => Some(path_to_string(path)),
                ItemImportTree::Rename(rename) => Some(rename.from.name.clone()),
                _ => None,
            })
            .next(),
        _ => None,
    }
}

fn path_to_string(path: &ItemImportPath) -> String {
    let mut segments = Vec::new();
    for seg in &path.segments {
        match seg {
            ItemImportTree::Ident(ident) => segments.push(ident.name.clone()),
            ItemImportTree::Rename(ItemImportRename { from, .. }) => {
                segments.push(from.name.clone())
            }
            _ => {}
        }
    }
    segments.join("/")
}
