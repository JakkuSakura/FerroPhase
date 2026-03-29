use std::collections::{HashMap, HashSet};

use eyre::eyre;
use fp_core::ast::{
    self, AstSerializer, BlockStmt, Expr, ExprBlock, ExprBreak, ExprContinue, ExprFor, ExprIf,
    ExprIndex, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind, ExprReturn,
    ExprStringTemplate, ExprStruct, ExprUnOp, ExprWhile, FormatArgRef, FormatTemplatePart,
    FunctionParam, Item, ItemImport, ItemImportTree, ItemKind, Node, NodeKind, Pattern,
    PatternKind, Ty, TypeEnum, TypePrimitive, TypeStruct, TypeTuple, TypeVec, Value, ValueList,
    ValueMap, ValueMapEntry, ValueStruct, ValueTuple,
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
                    self.emit_expr_statement(expr, false)?;
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
                self.emit_expr_statement(expr, false)?;
            }
            ItemKind::Import(import) => {
                self.emit_import(import)?;
            }
            ItemKind::OpaqueType(_)
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
        } else if self.is_statement_expr(func.body.as_ref()) {
            self.emit_expr_statement(func.body.as_ref(), false)?;
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
                self.emit_expr_statement(expr_stmt.expr.as_ref(), expr_stmt.has_value())?;
            }
            BlockStmt::Item(item) => self.emit_item(item.as_ref())?,
            BlockStmt::Defer(_) => {
                self.push_line("# defer statements are not supported in Python output");
            }
            BlockStmt::Noop => {}
            BlockStmt::Any(_) => {
                return Err(eyre!(
                    "Normalization cannot process placeholder statements during target emission"
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

    fn emit_import(&mut self, import: &ItemImport) -> Result<()> {
        self.push_line(&self.render_import(import)?);
        Ok(())
    }

    fn emit_expr_statement(&mut self, expr: &Expr, as_return: bool) -> Result<()> {
        match expr.kind() {
            ExprKind::IntrinsicCall(call) if !as_return => self.emit_intrinsic_statement(call),
            ExprKind::Block(block) if !as_return => self.emit_block(block),
            ExprKind::If(stmt_if) if self.is_statement_if(stmt_if) => {
                self.emit_if_statement(stmt_if)
            }
            ExprKind::While(stmt_while) => self.emit_while_statement(stmt_while),
            ExprKind::For(stmt_for) => self.emit_for_statement(stmt_for),
            ExprKind::Return(stmt_return) => self.emit_return_statement(stmt_return),
            ExprKind::Break(stmt_break) => self.emit_break_statement(stmt_break),
            ExprKind::Continue(stmt_continue) => self.emit_continue_statement(stmt_continue),
            _ => {
                let rendered = self.render_expr(expr)?;
                if as_return {
                    self.push_line(&format!("return {}", rendered));
                } else {
                    self.push_line(&rendered);
                }
                Ok(())
            }
        }
    }

    fn emit_if_statement(&mut self, stmt_if: &ExprIf) -> Result<()> {
        let cond = self.render_expr(stmt_if.cond.as_ref())?;
        self.start_block(&format!("if {}:", cond));
        self.emit_statement_body(stmt_if.then.as_ref())?;
        self.end_block();

        if let Some(elze) = &stmt_if.elze {
            self.start_block("else:");
            self.emit_statement_body(elze.as_ref())?;
            self.end_block();
        }

        Ok(())
    }

    fn emit_while_statement(&mut self, stmt_while: &ExprWhile) -> Result<()> {
        let cond = self.render_expr(stmt_while.cond.as_ref())?;
        self.start_block(&format!("while {}:", cond));
        self.emit_statement_body(stmt_while.body.as_ref())?;
        self.end_block();
        Ok(())
    }

    fn emit_for_statement(&mut self, stmt_for: &ExprFor) -> Result<()> {
        let pat = self.render_pattern(stmt_for.pat.as_ref());
        let iter = self.render_expr(stmt_for.iter.as_ref())?;
        self.start_block(&format!("for {} in {}:", pat, iter));
        self.emit_statement_body(stmt_for.body.as_ref())?;
        self.end_block();
        Ok(())
    }

    fn emit_return_statement(&mut self, stmt_return: &ExprReturn) -> Result<()> {
        if let Some(value) = &stmt_return.value {
            let rendered = self.render_expr(value.as_ref())?;
            self.push_line(&format!("return {}", rendered));
        } else {
            self.push_line("return");
        }
        Ok(())
    }

    fn emit_break_statement(&mut self, stmt_break: &ExprBreak) -> Result<()> {
        if let Some(value) = &stmt_break.value {
            return Err(eyre!(
                "Python output does not support break with value: {}",
                self.render_expr(value.as_ref())?
            )
            .into());
        }
        self.push_line("break");
        Ok(())
    }

    fn emit_continue_statement(&mut self, _stmt_continue: &ExprContinue) -> Result<()> {
        self.push_line("continue");
        Ok(())
    }

    fn emit_statement_body(&mut self, expr: &Expr) -> Result<()> {
        match expr.kind() {
            ExprKind::Block(block) => self.emit_block(block),
            _ => self.emit_expr_statement(expr, false),
        }
    }

    fn is_statement_if(&self, stmt_if: &ExprIf) -> bool {
        matches!(stmt_if.then.as_ref().kind(), ExprKind::Block(_))
            || matches!(
                stmt_if.elze.as_ref().map(|expr| expr.kind()),
                Some(ExprKind::Block(_))
            )
            || stmt_if.elze.is_none()
    }

    fn is_statement_expr(&self, expr: &Expr) -> bool {
        match expr.kind() {
            ExprKind::If(stmt_if) => self.is_statement_if(stmt_if),
            ExprKind::While(_)
            | ExprKind::For(_)
            | ExprKind::Return(_)
            | ExprKind::Break(_)
            | ExprKind::Continue(_) => true,
            _ => false,
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
            ExprKind::Name(locator) => Ok(self.render_locator(locator)),
            ExprKind::Invoke(invoke) => self.render_invoke(invoke),
            ExprKind::Select(select) => Ok(format!(
                "{}.{}",
                self.render_expr(select.obj.as_ref())?,
                select.field.name
            )),
            ExprKind::Index(index) => self.render_index(index),
            ExprKind::Assign(assign) => Ok(format!(
                "{} = {}",
                self.render_expr(assign.target.as_ref())?,
                self.render_expr(assign.value.as_ref())?
            )),
            ExprKind::If(expr_if) => self.render_if_expr(expr_if),
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
                "Normalization cannot process placeholder expressions during target emission"
            )
            .into()),
            other => Err(eyre!("Unsupported expression in target emitter: {:?}", other).into()),
        }
    }

    fn render_if_expr(&mut self, expr_if: &ExprIf) -> Result<String> {
        let Some(elze) = &expr_if.elze else {
            return Err(eyre!("Python conditional expressions require an else branch").into());
        };
        if matches!(expr_if.then.as_ref().kind(), ExprKind::Block(_))
            || matches!(elze.as_ref().kind(), ExprKind::Block(_))
        {
            return Err(eyre!("Cannot render block-based if expression inline").into());
        }
        Ok(format!(
            "({} if {} else {})",
            self.render_expr(expr_if.then.as_ref())?,
            self.render_expr(expr_if.cond.as_ref())?,
            self.render_expr(elze.as_ref())?
        ))
    }

    fn render_index(&mut self, index: &ExprIndex) -> Result<String> {
        Ok(format!(
            "{}[{}]",
            self.render_expr(index.obj.as_ref())?,
            self.render_expr(index.index.as_ref())?
        ))
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

    fn render_locator(&self, locator: &ast::Name) -> String {
        locator
            .to_string()
            .split("::")
            .map(|segment| segment.to_string())
            .collect::<Vec<_>>()
            .join(".")
    }

    fn render_pattern(&self, pattern: &Pattern) -> String {
        match pattern.kind() {
            PatternKind::Ident(ident) => ident.ident.name.clone(),
            PatternKind::Tuple(tuple) => {
                let values = tuple
                    .patterns
                    .iter()
                    .map(|pattern| self.render_pattern(pattern))
                    .collect::<Vec<_>>();
                format!("({})", values.join(", "))
            }
            _ => pattern
                .as_ident()
                .map(|ident| ident.name.clone())
                .unwrap_or_else(|| "_".to_string()),
        }
    }

    fn render_import(&self, import: &ItemImport) -> Result<String> {
        if let Some(module) = import.module_path() {
            let module_text = render_import_module(&module.segments)?;
            let names = render_imported_names(import)?;
            let prefix = ".".repeat(import.level() as usize);
            return Ok(format!("from {}{} import {}", prefix, module_text, names));
        }

        match &import.tree {
            ItemImportTree::Group(group) => Ok(format!(
                "import {}",
                group
                    .items
                    .iter()
                    .map(render_import_path_like)
                    .collect::<Result<Vec<_>>>()?
                    .join(", ")
            )),
            _ => Ok(format!("import {}", render_import_path_like(&import.tree)?)),
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
            ExprKind::Name(locator) => locator
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

fn render_import_module(segments: &[ItemImportTree]) -> Result<String> {
    if segments.is_empty() {
        return Err(eyre!("Import-from statement is missing a module path").into());
    }
    segments
        .iter()
        .map(render_import_module_segment)
        .collect::<Result<Vec<_>>>()
        .map(|segments| segments.join("."))
}

fn render_import_module_segment(tree: &ItemImportTree) -> Result<String> {
    match tree {
        ItemImportTree::Ident(ident) => Ok(ident.name.clone()),
        ItemImportTree::Path(path) => path
            .segments
            .iter()
            .map(render_import_module_segment)
            .collect::<Result<Vec<_>>>()
            .map(|segments| segments.join(".")),
        _ => Err(eyre!("Unsupported Python module import segment: {}", tree).into()),
    }
}

fn render_import_name(tree: &ItemImportTree) -> Result<String> {
    match tree {
        ItemImportTree::Ident(ident) => Ok(ident.name.clone()),
        ItemImportTree::Rename(rename) => Ok(format!("{} as {}", rename.from.name, rename.to.name)),
        ItemImportTree::Glob => Ok("*".to_string()),
        ItemImportTree::Path(path) => render_import_path_like(&ItemImportTree::Path(path.clone())),
        ItemImportTree::Group(group) => group
            .items
            .iter()
            .map(render_import_name)
            .collect::<Result<Vec<_>>>()
            .map(|names| names.join(", ")),
        _ => Err(eyre!("Unsupported Python import target: {}", tree).into()),
    }
}

fn render_import_path_like(tree: &ItemImportTree) -> Result<String> {
    match tree {
        ItemImportTree::Ident(ident) => Ok(ident.name.clone()),
        ItemImportTree::Rename(rename) => Ok(format!("{} as {}", rename.from.name, rename.to.name)),
        ItemImportTree::Path(path) => path
            .segments
            .iter()
            .map(render_import_module_segment)
            .collect::<Result<Vec<_>>>()
            .map(|segments| segments.join(".")),
        _ => Err(eyre!("Unsupported Python import tree: {}", tree).into()),
    }
}

fn render_imported_names(import: &ItemImport) -> Result<String> {
    let module = import
        .module_path()
        .ok_or_else(|| eyre!("expected from-import metadata"))?;
    let ItemImportTree::Path(full_path) = &import.tree else {
        return render_import_name(&import.tree);
    };

    if full_path.segments.len() < module.segments.len() {
        return Err(eyre!("import tree is shorter than from-import module path").into());
    }

    let suffix = &full_path.segments[module.segments.len()..];
    if suffix.is_empty() {
        return Err(eyre!("from-import is missing imported names").into());
    }

    if suffix.len() == 1 {
        return render_import_name(&suffix[0]);
    }

    render_import_name(&ItemImportTree::Path(fp_core::ast::ItemImportPath {
        segments: suffix.to_vec(),
    }))
}

#[cfg(test)]
mod tests {
    use fp_core::ast::AstSerializer;
    use fp_core::frontend::LanguageFrontend;

    use crate::PythonFrontend;

    use super::PythonSerializer;

    fn round_trip(source: &str) -> String {
        let frontend = PythonFrontend::new();
        let parsed = frontend.parse(source, None).expect("parse python");
        let serializer = PythonSerializer;
        serializer
            .serialize_node(&parsed.ast)
            .expect("serialize python")
    }

    #[test]
    fn serializes_control_flow_statements() {
        let source = "\
def walk(items):
    for item in items:
        if item:
            continue
        while item < 3:
            break
    return items[0]
";
        let rendered = round_trip(source);
        assert!(rendered.contains("def walk(items: Any) -> None:"));
        assert!(rendered.contains("for item in items:"));
        assert!(rendered.contains("if item:"));
        assert!(rendered.contains("continue"));
        assert!(rendered.contains("while (item < 3):"));
        assert!(rendered.contains("break"));
        assert!(rendered.contains("return items[0]"));
    }

    #[test]
    fn serializes_python_imports() {
        let source = "\
import os, sys as system
import pkg.submodule
from pkg import submodule
from collections import defaultdict as dd, deque
from package import *
";
        let rendered = round_trip(source);
        assert!(rendered.contains("import os, sys as system"));
        assert!(rendered.contains("import pkg.submodule"));
        assert!(rendered.contains("from pkg import submodule"));
        assert!(rendered.contains("from collections import defaultdict as dd, deque"));
        assert!(rendered.contains("from package import *"));
    }

    #[test]
    fn serializes_conditional_expression() {
        let source = "\
def pick(flag, left, right):
    return left if flag else right
";
        let rendered = round_trip(source);
        assert!(rendered.contains("return (left if flag else right)"));
    }
}
