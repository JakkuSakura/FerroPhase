use eyre::eyre;
use fp_core::ast::{
    AstSerializer, BlockStmt, EnumTypeVariant, Expr, ExprAssign, ExprBinOp, ExprBlock, ExprIf,
    ExprIndex, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget, ExprKind, ExprLoop, ExprMatch,
    ExprRange, ExprRangeLimit, ExprReturn, ExprSelect, ExprStruct, ExprUnOp, ExprWhile,
    FunctionParam, Item, ItemDefConst, ItemDefEnum, ItemDefFunction, ItemImpl, ItemKind, Name,
    Node, NodeKind, Pattern, PatternKind, PatternTupleStruct, Ty, TypeStruct, Value, ValueList,
    ValueMap, ValueMapEntry, ValueStruct, ValueTuple,
};
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::{BinOpKind, UnOpKind};
use itertools::Itertools;
use std::collections::HashMap;

/// Public entry point used by the CLI target emitter.
pub struct GdscriptSerializer;

impl AstSerializer for GdscriptSerializer {
    fn serialize_node(&self, node: &Node) -> Result<String> {
        let mut emitter = GdscriptEmitter::new();
        emitter.emit_node(node)?;
        Ok(emitter.finish())
    }
}

fn collect_enums(items: &[Item]) -> HashMap<String, EnumInfo> {
    let mut out = HashMap::new();
    for item in items {
        match item.kind() {
            ItemKind::DefEnum(def) => {
                let variants = def
                    .value
                    .variants
                    .iter()
                    .map(|variant| {
                        let kind = classify_variant(variant);
                        let fields = match (&kind, &variant.value) {
                            (EnumVariantKind::Structural, Ty::Structural(structural)) => structural
                                .fields
                                .iter()
                                .map(|field| field.name.name.clone())
                                .collect(),
                            _ => Vec::new(),
                        };

                        EnumVariantInfo {
                            name: variant.name.name.clone(),
                            kind,
                            discriminant: variant
                                .discriminant
                                .as_ref()
                                .map(|expr| expr.as_ref().clone()),
                            fields,
                        }
                    })
                    .collect::<Vec<_>>();

                let plain_enum = variants
                    .iter()
                    .all(|variant| variant.kind == EnumVariantKind::Unit);

                out.insert(
                    def.name.name.clone(),
                    EnumInfo {
                        name: def.name.name.clone(),
                        variants,
                        plain_enum,
                    },
                );
            }
            ItemKind::Module(module) => {
                out.extend(collect_enums(&module.items));
            }
            _ => {}
        }
    }
    out
}

fn collect_impls(items: &[Item]) -> HashMap<String, Vec<ItemDefFunction>> {
    let mut out: HashMap<String, Vec<ItemDefFunction>> = HashMap::new();
    for item in items {
        match item.kind() {
            ItemKind::Impl(def) => {
                if def.trait_ty.is_some() {
                    continue;
                }
                let Some(self_ty) = (match def.self_ty.kind() {
                    ExprKind::Name(name) => {
                        let rendered = format!("{name}");
                        let normalized = normalize_type_name(&rendered);
                        Some(
                            normalized
                                .split("::")
                                .last()
                                .unwrap_or(normalized.as_str())
                                .to_string(),
                        )
                    }
                    _ => None,
                }) else {
                    continue;
                };
                let methods = out.entry(self_ty).or_default();
                for inner in &def.items {
                    if let ItemKind::DefFunction(func) = inner.kind() {
                        methods.push(func.clone());
                    }
                }
            }
            ItemKind::Module(module) => {
                for (k, v) in collect_impls(&module.items) {
                    out.entry(k).or_default().extend(v);
                }
            }
            _ => {}
        }
    }
    out
}

fn classify_variant(variant: &EnumTypeVariant) -> EnumVariantKind {
    match &variant.value {
        Ty::Unit(_) => EnumVariantKind::Unit,
        Ty::Tuple(tuple) if tuple.types.len() == 1 => EnumVariantKind::Tuple1,
        Ty::Structural(_) => EnumVariantKind::Structural,
        _ => EnumVariantKind::Other,
    }
}

fn render_pattern_condition(pat: &Pattern, scrutinee: &str) -> Result<String> {
    match pat.kind() {
        PatternKind::Wildcard(_) => Ok("true".to_string()),
        PatternKind::Variant(variant) => {
            let tag = variant_tag_from_expr(&variant.name)?;
            Ok(format!("{scrutinee}.tag == \"{}\"", escape_string(&tag)))
        }
        PatternKind::TupleStruct(tuple) => {
            let tag = variant_tag_from_name(&tuple.name)?;
            Ok(format!("{scrutinee}.tag == \"{}\"", escape_string(&tag)))
        }
        _ => Err(eyre!("unsupported match pattern in gdscript output: {pat:?}").into()),
    }
}

fn variant_tag_from_expr(expr: &Expr) -> Result<String> {
    match expr.kind() {
        ExprKind::Name(name) => variant_tag_from_name(name),
        ExprKind::Value(value) => match value.as_ref() {
            Value::String(s) => Ok(s.value.clone()),
            Value::Char(c) => Ok(c.value.to_string()),
            _ => Err(eyre!("unsupported variant name value in match: {expr:?}").into()),
        },
        _ => Err(eyre!("unsupported variant name expression in match: {expr:?}").into()),
    }
}

fn variant_tag_from_name(name: &Name) -> Result<String> {
    let rendered = render_name(name);
    Ok(rendered
        .split('.')
        .last()
        .unwrap_or(rendered.as_str())
        .to_string())
}

fn extract_tuple1_binding(pat: &Pattern) -> Option<String> {
    let PatternKind::TupleStruct(PatternTupleStruct { patterns, .. }) = pat.kind() else {
        return None;
    };
    if patterns.len() != 1 {
        return None;
    }
    patterns[0].as_ident().map(|ident| ident.name.clone())
}

#[derive(Debug, Clone)]
struct EnumInfo {
    name: String,
    variants: Vec<EnumVariantInfo>,
    plain_enum: bool,
}

#[derive(Debug, Clone)]
struct EnumVariantInfo {
    name: String,
    kind: EnumVariantKind,
    discriminant: Option<Expr>,
    fields: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EnumVariantKind {
    Unit,
    Tuple1,
    Structural,
    Other,
}

fn render_let_name(pattern: &Pattern) -> Result<&str> {
    pattern
        .as_ident()
        .map(|ident| ident.name.as_str())
        .ok_or_else(|| eyre!("unsupported let pattern for gdscript emission: {pattern:?}").into())
}

struct GdscriptEmitter {
    code: String,
    indent: usize,
    loop_result_stack: Vec<Option<String>>,
    temp_counter: usize,
    enums: HashMap<String, EnumInfo>,
    impls: HashMap<String, Vec<ItemDefFunction>>,
}

impl GdscriptEmitter {
    fn new() -> Self {
        Self {
            code: String::new(),
            indent: 0,
            loop_result_stack: Vec::new(),
            temp_counter: 0,
            enums: HashMap::new(),
            impls: HashMap::new(),
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
            NodeKind::File(file) => {
                self.collect_symbols(&file.items);
                for item in &file.items {
                    self.emit_item(item)?;
                }
            }
            NodeKind::Item(item) => self.emit_item(item)?,
            NodeKind::Expr(expr) => {
                let rendered = self.render_expr(expr)?;
                self.push_line(&rendered);
            }
            NodeKind::Query(_) => {
                self.push_comment("query documents are not supported for gdscript output");
            }
            NodeKind::Schema(_) => {
                self.push_comment("schema documents are not supported for gdscript output");
            }
            NodeKind::Workspace(_) => {
                self.push_comment("workspace snapshots are not supported for gdscript output");
            }
        }
        Ok(())
    }

    fn emit_item(&mut self, item: &Item) -> Result<()> {
        match item.kind() {
            ItemKind::DefFunction(def) => self.emit_function(def),
            ItemKind::DefConst(def) => self.emit_const(def),
            ItemKind::DefStruct(def) => self.emit_struct(&def.value),
            ItemKind::DefEnum(def) => self.emit_enum(def),
            ItemKind::Impl(def) => self.emit_impl(def),
            ItemKind::Module(module) => {
                for child in &module.items {
                    self.emit_item(child)?;
                }
                Ok(())
            }
            ItemKind::Expr(expr) => {
                let rendered = self.render_expr(expr)?;
                self.push_line(&rendered);
                Ok(())
            }
            ItemKind::Import(_)
            | ItemKind::DefType(_)
            | ItemKind::DefStatic(_)
            | ItemKind::DeclConst(_)
            | ItemKind::DeclStatic(_)
            | ItemKind::DeclFunction(_)
            | ItemKind::DeclType(_)
            | ItemKind::DefTrait(_)
            | ItemKind::DefStructural(_)
            | ItemKind::Macro(_)
            | ItemKind::Any(_) => {
                self.push_comment(&format!(
                    "unsupported item in gdscript output: {:?}",
                    item.kind()
                ));
                Ok(())
            }
        }
    }

    fn emit_const(&mut self, def: &ItemDefConst) -> Result<()> {
        let keyword = if def.mutable == Some(true) {
            "var"
        } else {
            "const"
        };
        let value = self.render_expr(def.value.as_ref())?;
        self.push_line(&format!("{keyword} {} = {value}", def.name.name));
        Ok(())
    }

    fn emit_enum(&mut self, def: &ItemDefEnum) -> Result<()> {
        let Some(info) = self.enums.get(def.name.name.as_str()).cloned() else {
            self.push_comment(&format!("enum {} missing collection info", def.name.name));
            return Ok(());
        };

        if info.variants.is_empty() {
            self.push_comment(&format!("enum {} is empty", def.name.name));
            return Ok(());
        }

        if info.plain_enum {
            self.emit_plain_enum(&info)
        } else {
            self.emit_class_enum(&info)
        }
    }

    fn emit_plain_enum(&mut self, info: &EnumInfo) -> Result<()> {
        let variants = info
            .variants
            .iter()
            .map(|variant| {
                if let Some(discriminant) = &variant.discriminant {
                    let rendered = self.render_expr(discriminant)?;
                    Ok(format!("{} = {rendered}", variant.name))
                } else {
                    Ok(variant.name.clone())
                }
            })
            .collect::<Result<Vec<_>>>()?
            .join(", ");

        self.push_line(&format!("enum {} {{ {variants} }}", info.name));
        Ok(())
    }

    fn emit_class_enum(&mut self, info: &EnumInfo) -> Result<()> {
        self.push_line(&format!("class {}:", info.name));
        self.indent += 1;
        self.push_line("var tag");
        self.push_line("var data");
        self.push_blank_line();

        for variant in &info.variants {
            match variant.kind {
                EnumVariantKind::Unit => {
                    self.push_line(&format!("static func {}():", variant.name));
                    self.indent += 1;
                    self.emit_enum_ctor(&variant.name, Vec::new())?;
                    self.indent -= 1;
                    self.push_blank_line();
                }
                EnumVariantKind::Tuple1 => {
                    let arg = "value";
                    self.push_line(&format!("static func {}({arg}):", variant.name));
                    self.indent += 1;
                    self.emit_enum_ctor(&variant.name, vec![format!("\"0\": {arg}")])?;
                    self.indent -= 1;
                    self.push_blank_line();
                }
                EnumVariantKind::Structural => {
                    let fields = &variant.fields;

                    let params = fields.iter().join(", ");
                    self.push_line(&format!("static func {}({params}):", variant.name));
                    self.indent += 1;
                    let entries = fields
                        .iter()
                        .map(|field| format!("\"{field}\": {field}"))
                        .collect();
                    self.emit_enum_ctor(&variant.name, entries)?;
                    self.indent -= 1;
                    self.push_blank_line();
                }
                EnumVariantKind::Other => {
                    self.push_comment(&format!(
                        "unsupported enum variant {}::{}",
                        info.name, variant.name
                    ));
                }
            }
        }

        if let Some(methods) = self.impls.get(&info.name).cloned() {
            for method in &methods {
                self.emit_impl_method(method)?;
                self.push_blank_line();
            }
        }

        if self.code.ends_with("\n\n") {
            self.code.pop();
        }
        self.indent -= 1;
        self.push_blank_line();
        Ok(())
    }

    fn emit_enum_ctor(&mut self, tag: &str, entries: Vec<String>) -> Result<()> {
        self.push_line(&format!("var inst = {}.new()", self.current_class_name()?));
        self.push_line(&format!("inst.tag = \"{}\"", escape_string(tag)));
        let dict = format!("{{{}}}", entries.join(", "));
        self.push_line(&format!("inst.data = {dict}"));
        self.push_line("return inst");
        Ok(())
    }

    fn current_class_name(&self) -> Result<&str> {
        // Best-effort: look at the last emitted `class Name:` line.
        // This is safe because constructors are emitted immediately after the class header.
        let name = self
            .code
            .lines()
            .rev()
            .find_map(|line| line.strip_prefix("class "))
            .and_then(|rest| rest.strip_suffix(':'))
            .map(str::trim)
            .ok_or_else(|| eyre!("internal error: missing enclosing class name"))?;
        Ok(name)
    }

    fn emit_impl(&mut self, def: &ItemImpl) -> Result<()> {
        // Impl blocks are folded into class enums / structs during emission.
        // Leave a note only if it's a trait impl or on an unknown self type.
        if def.trait_ty.is_some() {
            self.push_comment("trait impl blocks are not supported in gdscript output");
            return Ok(());
        }

        let Some(self_ty) = self.extract_self_ty_name(def) else {
            self.push_comment("unsupported impl self type in gdscript output");
            return Ok(());
        };

        if self.enums.contains_key(&self_ty) {
            return Ok(());
        }

        self.push_comment(&format!("unsupported impl in gdscript output: {self_ty}"));
        Ok(())
    }

    fn emit_impl_method(&mut self, def: &ItemDefFunction) -> Result<()> {
        let keyword = if def.sig.receiver.is_some() {
            "func"
        } else {
            "static func"
        };
        let params = self.render_params(&def.sig.params)?;
        self.push_line(&format!("{keyword} {}({params}):", def.name.name));
        self.indent += 1;
        match def.body.kind() {
            ExprKind::Block(block) => self.emit_block(block)?,
            _ => {
                let rendered = self.render_expr(def.body.as_ref())?;
                self.push_line(&format!("return {rendered}"));
            }
        }
        self.indent -= 1;
        Ok(())
    }

    fn extract_self_ty_name(&self, def: &ItemImpl) -> Option<String> {
        match def.self_ty.kind() {
            ExprKind::Name(name) => {
                let rendered = format!("{name}");
                let normalized = normalize_type_name(&rendered);
                Some(
                    normalized
                        .split("::")
                        .last()
                        .unwrap_or(normalized.as_str())
                        .to_string(),
                )
            }
            _ => None,
        }
    }

    fn emit_struct(&mut self, def: &TypeStruct) -> Result<()> {
        self.push_line(&format!("class {}:", def.name.name));
        self.indent += 1;

        if def.fields.is_empty() {
            self.push_line("pass");
            self.indent -= 1;
            self.push_blank_line();
            return Ok(());
        }

        for field in &def.fields {
            self.push_line(&format!("var {}", field.name.name));
        }
        self.push_blank_line();

        let init_params = def
            .fields
            .iter()
            .map(|field| field.name.name.as_str())
            .join(", ");
        self.push_line(&format!("func _init({init_params}):"));
        self.indent += 1;
        for field in &def.fields {
            let name = field.name.name.as_str();
            self.push_line(&format!("self.{name} = {name}"));
        }
        self.indent -= 1;
        self.indent -= 1;
        self.push_blank_line();
        Ok(())
    }

    fn emit_function(&mut self, def: &ItemDefFunction) -> Result<()> {
        let params = self.render_params(&def.sig.params)?;
        self.push_line(&format!("func {}({params}):", def.name.name));
        self.indent += 1;

        match def.body.kind() {
            ExprKind::Block(block) => self.emit_block(block)?,
            _ => {
                let rendered = self.render_expr(def.body.as_ref())?;
                self.push_line(&format!("return {rendered}"));
            }
        }

        self.indent -= 1;
        self.push_blank_line();
        Ok(())
    }

    fn emit_block(&mut self, block: &ExprBlock) -> Result<()> {
        if block.stmts.is_empty() {
            self.push_line("pass");
            return Ok(());
        }
        for stmt in &block.stmts {
            self.emit_stmt(stmt)?;
        }
        Ok(())
    }

    fn emit_stmt(&mut self, stmt: &BlockStmt) -> Result<()> {
        match stmt {
            BlockStmt::Item(item) => self.emit_item(item.as_ref()),
            BlockStmt::Let(stmt_let) => {
                let name = render_let_name(&stmt_let.pat)?;
                if let Some(init) = &stmt_let.init {
                    let value = self.render_expr(init)?;
                    self.push_line(&format!("var {name} = {value}"));
                } else {
                    self.push_line(&format!("var {name}"));
                }
                Ok(())
            }
            BlockStmt::Expr(expr_stmt) => {
                let expr = expr_stmt.expr.as_ref();
                match expr.kind() {
                    ExprKind::IntrinsicCall(call) if call.kind == IntrinsicCallKind::Print => {
                        self.emit_print_call(call)?;
                        Ok(())
                    }
                    ExprKind::IntrinsicCall(call) if call.kind == IntrinsicCallKind::Println => {
                        self.emit_print_call(call)?;
                        Ok(())
                    }
                    ExprKind::IntrinsicCall(call) if call.kind == IntrinsicCallKind::Sleep => {
                        let seconds = call
                            .args
                            .first()
                            .map(|expr| self.render_expr(expr))
                            .transpose()?
                            .unwrap_or_else(|| "0".to_string());
                        self.push_comment(&format!("sleep({seconds})"));
                        Ok(())
                    }
                    ExprKind::IntrinsicCall(call) => {
                        self.push_comment(&format!(
                            "unsupported intrinsic call in gdscript output: {:?}",
                            call.kind
                        ));
                        if expr_stmt.has_value() {
                            self.push_line("return null");
                        }
                        Ok(())
                    }
                    ExprKind::Block(block) => self.emit_block(block),
                    ExprKind::Async(async_expr) => self.emit_block_like(async_expr.expr.as_ref()),
                    ExprKind::If(expr_if) => self.emit_if(expr_if),
                    ExprKind::While(expr_while) => self.emit_while(expr_while),
                    ExprKind::For(expr_for) => self.emit_for(expr_for),
                    ExprKind::Loop(expr_loop) => self.emit_loop(expr_loop, expr_stmt.has_value()),
                    ExprKind::Match(expr_match) if expr_stmt.has_value() => {
                        let rendered = self.render_match_expr(expr_match)?;
                        self.push_line(&format!("return {rendered}"));
                        Ok(())
                    }
                    ExprKind::Return(expr_return) => self.emit_return(expr_return),
                    ExprKind::Break(expr_break) => self.emit_break(expr_break),
                    ExprKind::Continue(_) => {
                        self.push_line("continue");
                        Ok(())
                    }
                    _ => {
                        let rendered = self.render_expr(expr)?;
                        if expr_stmt.has_value() {
                            self.push_line(&format!("return {rendered}"));
                        } else {
                            self.push_line(&rendered);
                        }
                        Ok(())
                    }
                }
            }
            BlockStmt::Noop => Ok(()),
            BlockStmt::Any(_) => Err(eyre!(
                "gdscript target cannot process placeholder statements during emission"
            )
            .into()),
        }
    }

    fn emit_if(&mut self, expr_if: &ExprIf) -> Result<()> {
        let cond = self.render_expr(expr_if.cond.as_ref())?;
        self.push_line(&format!("if {cond}:"));
        self.indent += 1;
        self.emit_block_like(expr_if.then.as_ref())?;
        self.indent -= 1;

        if let Some(elze) = &expr_if.elze {
            self.push_line("else:");
            self.indent += 1;
            self.emit_block_like(elze.as_ref())?;
            self.indent -= 1;
        }
        Ok(())
    }

    fn emit_return(&mut self, expr_return: &ExprReturn) -> Result<()> {
        if let Some(value) = &expr_return.value {
            let rendered = self.render_expr(value.as_ref())?;
            self.push_line(&format!("return {rendered}"));
        } else {
            self.push_line("return");
        }
        Ok(())
    }

    fn emit_block_like(&mut self, expr: &Expr) -> Result<()> {
        match expr.kind() {
            ExprKind::Block(block) => self.emit_block(block),
            _ => {
                let rendered = self.render_expr(expr)?;
                self.push_line(&rendered);
                Ok(())
            }
        }
    }

    fn emit_print_call(&mut self, call: &fp_core::ast::ExprIntrinsicCall) -> Result<()> {
        let rendered_args = if let Some(template) = call.args.first().and_then(|expr| {
            let ExprKind::FormatString(template) = expr.kind() else {
                return None;
            };
            Some(template)
        }) {
            self.render_format_template(template, &call.args[1..], &call.kwargs)?
        } else {
            call.args
                .iter()
                .map(|arg| self.render_expr(arg))
                .collect::<Result<Vec<_>>>()?
                .join(", ")
        };

        self.push_line(&format!("print({rendered_args})"));
        Ok(())
    }

    fn render_params(&mut self, params: &[FunctionParam]) -> Result<String> {
        Ok(params.iter().map(|p| p.name.name.as_str()).join(", "))
    }

    fn emit_for(&mut self, expr_for: &fp_core::ast::ExprFor) -> Result<()> {
        let name = render_let_name(expr_for.pat.as_ref())?;
        let iter = self.render_expr(expr_for.iter.as_ref())?;

        self.push_line(&format!("for {name} in {iter}:"));
        self.indent += 1;
        self.emit_block_like(expr_for.body.as_ref())?;
        self.indent -= 1;
        Ok(())
    }

    fn emit_while(&mut self, expr_while: &ExprWhile) -> Result<()> {
        let cond = self.render_expr(expr_while.cond.as_ref())?;
        self.push_line(&format!("while {cond}:"));
        self.indent += 1;
        self.emit_block_like(expr_while.body.as_ref())?;
        self.indent -= 1;
        Ok(())
    }

    fn emit_loop(&mut self, expr_loop: &ExprLoop, returns_value: bool) -> Result<()> {
        if returns_value {
            let result_var = self.next_temp("loop_result");
            self.push_line(&format!("var {result_var} = null"));
            self.emit_loop_body(expr_loop, Some(result_var.clone()))?;
            self.push_line(&format!("return {result_var}"));
            Ok(())
        } else {
            self.emit_loop_body(expr_loop, None)
        }
    }

    fn emit_loop_body(&mut self, expr_loop: &ExprLoop, result_var: Option<String>) -> Result<()> {
        self.push_line("while true:");
        self.indent += 1;
        self.loop_result_stack.push(result_var);
        let result = self.emit_block_like(expr_loop.body.as_ref());
        self.loop_result_stack.pop();
        self.indent -= 1;
        result
    }

    fn emit_break(&mut self, expr_break: &fp_core::ast::ExprBreak) -> Result<()> {
        if let Some(value) = &expr_break.value {
            let Some(Some(var)) = self.loop_result_stack.last().cloned() else {
                return Err(
                    eyre!("break with value is only supported inside loop expressions").into(),
                );
            };
            let rendered = self.render_expr(value.as_ref())?;
            self.push_line(&format!("{var} = {rendered}"));
        }
        self.push_line("break");
        Ok(())
    }

    fn next_temp(&mut self, prefix: &str) -> String {
        let name = format!("__fp_{prefix}_{}", self.temp_counter);
        self.temp_counter += 1;
        name
    }

    fn render_expr(&mut self, expr: &Expr) -> Result<String> {
        match expr.kind() {
            ExprKind::Value(value) => self.render_value(value.as_ref()),
            ExprKind::Name(name) => self.render_name_expr(name),
            ExprKind::Invoke(invoke) => self.render_invoke(invoke),
            ExprKind::Select(select) => self.render_select(select),
            ExprKind::Assign(assign) => self.render_assign(assign),
            ExprKind::BinOp(bin_op) => self.render_bin_op(bin_op),
            ExprKind::UnOp(un_op) => self.render_un_op(un_op),
            ExprKind::Range(range) => self.render_range(range),
            ExprKind::Block(block) => self.render_block_expr(block),
            ExprKind::Async(async_expr) => self.render_expr(async_expr.expr.as_ref()),
            ExprKind::Struct(struct_expr) => self.render_struct_expr(struct_expr),
            ExprKind::Match(expr_match) => self.render_match_expr(expr_match),
            ExprKind::IntrinsicCall(call) => self.render_intrinsic_call_expr(call),
            ExprKind::Index(index) => self.render_index_expr(index),
            ExprKind::FormatString(_) => Err(eyre!(
                "format string expressions are only supported inside print/println calls"
            )
            .into()),
            ExprKind::Paren(paren) => Ok(format!("({})", self.render_expr(paren.expr.as_ref())?)),
            ExprKind::Reference(reference) => self.render_expr(reference.referee.as_ref()),
            ExprKind::Dereference(deref) => self.render_expr(deref.referee.as_ref()),
            ExprKind::Cast(cast) => self.render_expr(cast.expr.as_ref()),
            ExprKind::If(expr_if) => {
                let cond = self.render_expr(expr_if.cond.as_ref())?;
                let then = self.render_expr(expr_if.then.as_ref())?;
                let elze = match &expr_if.elze {
                    Some(elze) => self.render_expr(elze.as_ref())?,
                    None => "null".to_string(),
                };
                Ok(format!("({then} if {cond} else {elze})"))
            }
            ExprKind::Any(_) => Err(eyre!(
                "gdscript target cannot process placeholder expressions during emission"
            )
            .into()),
            other => Err(eyre!("unsupported expression in gdscript emitter: {other:?}").into()),
        }
    }

    fn render_index_expr(&mut self, index: &ExprIndex) -> Result<String> {
        let obj = self.render_expr(index.obj.as_ref())?;
        let idx = self.render_expr(index.index.as_ref())?;
        Ok(format!("{obj}[{idx}]"))
    }

    fn render_intrinsic_call_expr(&mut self, call: &ExprIntrinsicCall) -> Result<String> {
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                let rendered_args = if let Some(template) = call.args.first().and_then(|expr| {
                    let ExprKind::FormatString(template) = expr.kind() else {
                        return None;
                    };
                    Some(template)
                }) {
                    self.render_format_template(template, &call.args[1..], &call.kwargs)?
                } else {
                    call.args
                        .iter()
                        .map(|arg| self.render_expr(arg))
                        .collect::<Result<Vec<_>>>()?
                        .join(", ")
                };

                Ok(format!("print({rendered_args})"))
            }
            IntrinsicCallKind::Format => {
                let Some(template_expr) = call.args.first() else {
                    return Err(eyre!("format intrinsic expects a format template arg").into());
                };
                let ExprKind::FormatString(template) = template_expr.kind() else {
                    return Err(eyre!(
                        "format intrinsic expects a format string template as its first arg"
                    )
                    .into());
                };
                self.render_format_template(template, &call.args[1..], &call.kwargs)
            }
            IntrinsicCallKind::Sleep => Ok("null".to_string()),
            _ => Ok("null".to_string()),
        }
    }

    fn render_range(&mut self, range: &ExprRange) -> Result<String> {
        let start = match &range.start {
            Some(start) => self.render_expr(start.as_ref())?,
            None => "0".to_string(),
        };

        let Some(end) = &range.end else {
            return Err(eyre!("open-ended ranges are not supported in gdscript output").into());
        };
        let end_rendered = self.render_expr(end.as_ref())?;
        let end_adjusted = match range.limit {
            ExprRangeLimit::Exclusive => end_rendered,
            ExprRangeLimit::Inclusive => format!("({end_rendered} + 1)"),
        };

        if let Some(step) = &range.step {
            let step_rendered = self.render_expr(step.as_ref())?;
            Ok(format!("range({start}, {end_adjusted}, {step_rendered})"))
        } else {
            Ok(format!("range({start}, {end_adjusted})"))
        }
    }

    fn render_struct_expr(&mut self, struct_expr: &ExprStruct) -> Result<String> {
        if struct_expr.update.is_some() {
            return Err(eyre!("struct update syntax is not supported in gdscript output").into());
        }

        if let ExprKind::Name(name) = struct_expr.name.kind() {
            if let Some((enum_name, variant_name)) = enum_variant_from_name(name) {
                if let Some(enum_info) = self.enums.get(&enum_name) {
                    if !enum_info.plain_enum {
                        if let Some(variant_info) = enum_info
                            .variants
                            .iter()
                            .find(|variant| variant.name == variant_name)
                        {
                            if variant_info.kind == EnumVariantKind::Structural {
                                let fields = variant_info.fields.clone();
                                let mut args = Vec::new();
                                for field_name in fields {
                                    let field = struct_expr
                                        .fields
                                        .iter()
                                        .find(|field| field.name.name == field_name)
                                        .ok_or_else(|| {
                                            eyre!(
                                                "struct literal missing field {enum_name}::{variant_name}.{field_name}"
                                            )
                                        })?;
                                    let rendered = match &field.value {
                                        Some(value) => self.render_expr(value)?,
                                        None => field_name.clone(),
                                    };
                                    args.push(rendered);
                                }
                                return Ok(format!(
                                    "{}.{}({})",
                                    enum_name,
                                    variant_name,
                                    args.join(", ")
                                ));
                            }
                        }
                    }
                }
            }
        }

        let entries = struct_expr
            .fields
            .iter()
            .map(|field| {
                let key = format!("\"{}\"", escape_string(&field.name.name));
                let value = match &field.value {
                    Some(value) => self.render_expr(value)?,
                    None => field.name.name.clone(),
                };
                Ok(format!("{key}: {value}"))
            })
            .collect::<Result<Vec<_>>>()?
            .join(", ");

        Ok(format!("{{{entries}}}"))
    }

    fn render_block_expr(&mut self, block: &ExprBlock) -> Result<String> {
        let Some(last) = block.last_expr() else {
            return Ok("null".to_string());
        };

        if !block.first_stmts().is_empty() {
            // GDScript does not have general-purpose block expressions.
            // For now, degrade to a null expression rather than failing emission.
            return Ok("null".to_string());
        }

        self.render_expr(last)
    }

    fn render_format_template(
        &mut self,
        template: &fp_core::ast::ExprStringTemplate,
        args: &[Expr],
        kwargs: &[fp_core::ast::ExprKwArg],
    ) -> Result<String> {
        let mut implicit_index = 0usize;
        let mut out_parts: Vec<String> = Vec::new();

        for part in &template.parts {
            match part {
                fp_core::ast::FormatTemplatePart::Literal(lit) => {
                    out_parts.push(format!("\"{}\"", escape_string(lit)));
                }
                fp_core::ast::FormatTemplatePart::Placeholder(placeholder) => {
                    let expr = match &placeholder.arg_ref {
                        fp_core::ast::FormatArgRef::Implicit => {
                            let idx = implicit_index;
                            implicit_index += 1;
                            args.get(idx).ok_or_else(|| {
                                eyre!("format placeholder refers to missing arg at index {idx}")
                            })?
                        }
                        fp_core::ast::FormatArgRef::Positional(idx) => {
                            args.get(*idx).ok_or_else(|| {
                                eyre!("format placeholder refers to missing arg at index {idx}")
                            })?
                        }
                        fp_core::ast::FormatArgRef::Named(name) => kwargs
                            .iter()
                            .find(|kw| kw.name == *name)
                            .map(|kw| &kw.value)
                            .ok_or_else(|| {
                                eyre!("format placeholder refers to missing kwarg {name}")
                            })?,
                    };

                    let rendered = self.render_expr(expr)?;
                    out_parts.push(format!("str({rendered})"));
                }
            }
        }

        if out_parts.is_empty() {
            return Ok("\"\"".to_string());
        }

        Ok(out_parts.join(" + "))
    }

    fn render_invoke(&mut self, invoke: &ExprInvoke) -> Result<String> {
        let rendered_target = match &invoke.target {
            ExprInvokeTarget::Function(name) => self.render_name_expr(name)?,
            ExprInvokeTarget::Method(select) => self.render_select(select)?,
            ExprInvokeTarget::Expr(expr) => self.render_expr(expr.as_ref())?,
            ExprInvokeTarget::BinOp(kind) => {
                if invoke.args.len() != 2 {
                    return Err(eyre!("binary operator invocation expects two args").into());
                }
                let lhs = self.render_expr(&invoke.args[0])?;
                let rhs = self.render_expr(&invoke.args[1])?;
                return Ok(format!("({lhs} {} {rhs})", render_bin_op(kind)));
            }
            ExprInvokeTarget::Type(_) => {
                return Err(
                    eyre!("invoking type objects is not supported in gdscript output").into(),
                );
            }
            ExprInvokeTarget::Closure(_) => {
                return Err(eyre!("invoking closures is not supported in gdscript output").into());
            }
        };

        let args = invoke
            .args
            .iter()
            .map(|expr| self.render_expr(expr))
            .collect::<Result<Vec<_>>>()?
            .join(", ");
        Ok(format!("{rendered_target}({args})"))
    }

    fn render_select(&mut self, select: &ExprSelect) -> Result<String> {
        let obj_rendered = self.render_expr(select.obj.as_ref())?;
        let field = select.field.name.as_str();
        if let Some(info) = self.enums.get(&obj_rendered) {
            if !info.plain_enum {
                if info
                    .variants
                    .iter()
                    .any(|variant| variant.kind == EnumVariantKind::Unit && variant.name == field)
                {
                    return Ok(format!("{}.{}()", obj_rendered, field));
                }
            }
        }
        Ok(format!("{}.{}", obj_rendered, field))
    }

    fn render_name_expr(&mut self, name: &Name) -> Result<String> {
        let rendered = format!("{name}");
        let normalized = normalize_type_name(&rendered);

        let segments: Vec<&str> = normalized.split("::").collect();
        if segments.len() == 2 {
            let enum_name = segments[0];
            let variant = segments[1];
            if let Some(info) = self.enums.get(enum_name) {
                if !info.plain_enum {
                    if info
                        .variants
                        .iter()
                        .any(|v| v.kind == EnumVariantKind::Unit && v.name.as_str() == variant)
                    {
                        return Ok(format!("{}.{}()", enum_name, variant));
                    }
                }
            }
        }

        Ok(normalized.replace("::", "."))
    }

    fn render_assign(&mut self, assign: &ExprAssign) -> Result<String> {
        Ok(format!(
            "{} = {}",
            self.render_expr(assign.target.as_ref())?,
            self.render_expr(assign.value.as_ref())?
        ))
    }

    fn render_bin_op(&mut self, bin_op: &ExprBinOp) -> Result<String> {
        Ok(format!(
            "({} {} {})",
            self.render_expr(bin_op.lhs.as_ref())?,
            render_bin_op(&bin_op.kind),
            self.render_expr(bin_op.rhs.as_ref())?
        ))
    }

    fn render_un_op(&mut self, un_op: &ExprUnOp) -> Result<String> {
        let inner = self.render_expr(un_op.val.as_ref())?;
        match un_op.op {
            UnOpKind::Not => Ok(format!("not ({inner})")),
            UnOpKind::Neg => Ok(format!("-({inner})")),
            UnOpKind::Deref => Ok(inner),
            UnOpKind::Any(ref ident) => Ok(format!("{}({inner})", ident.as_str())),
        }
    }

    fn render_value(&mut self, value: &Value) -> Result<String> {
        match value {
            Value::String(s) => Ok(format!("\"{}\"", escape_string(&s.value))),
            Value::Char(c) => Ok(format!("\"{}\"", escape_string(&c.value.to_string()))),
            Value::Int(i) => Ok(i.value.to_string()),
            Value::Decimal(d) => Ok(d.value.to_string()),
            Value::Bool(b) => Ok(if b.value {
                "true".to_string()
            } else {
                "false".to_string()
            }),
            Value::Unit(_) => Ok("null".to_string()),
            Value::Null(_) => Ok("null".to_string()),
            Value::List(list) => self.render_list(list),
            Value::Tuple(tuple) => self.render_tuple(tuple),
            Value::Map(map) => self.render_map(map),
            Value::Struct(struct_value) => self.render_struct_value(struct_value),
            other => Err(eyre!("unsupported value in gdscript emitter: {other:?}").into()),
        }
    }

    fn render_list(&mut self, list: &ValueList) -> Result<String> {
        let items = list
            .values
            .iter()
            .map(|value| self.render_value(value))
            .collect::<Result<Vec<_>>>()?
            .join(", ");
        Ok(format!("[{items}]"))
    }

    fn render_tuple(&mut self, tuple: &ValueTuple) -> Result<String> {
        let items = tuple
            .values
            .iter()
            .map(|value| self.render_value(value))
            .collect::<Result<Vec<_>>>()?
            .join(", ");
        Ok(format!("[{items}]"))
    }

    fn render_map(&mut self, map: &ValueMap) -> Result<String> {
        let entries = map
            .entries
            .iter()
            .map(|entry| self.render_map_entry(entry))
            .collect::<Result<Vec<_>>>()?
            .join(", ");
        Ok(format!("{{{entries}}}"))
    }

    fn render_map_entry(&mut self, entry: &ValueMapEntry) -> Result<String> {
        let key = self.render_value(&entry.key)?;
        let value = self.render_value(&entry.value)?;
        Ok(format!("{key}: {value}"))
    }

    fn render_struct_value(&mut self, value: &ValueStruct) -> Result<String> {
        let entries = value
            .structural
            .fields
            .iter()
            .map(|field| {
                let key = format!("\"{}\"", escape_string(&field.name.name));
                let value = self.render_value(&field.value)?;
                Ok(format!("{key}: {value}"))
            })
            .collect::<Result<Vec<_>>>()?
            .join(", ");
        Ok(format!("{{{entries}}}"))
    }

    fn push_line(&mut self, line: &str) {
        for _ in 0..self.indent {
            self.code.push_str("    ");
        }
        self.code.push_str(line);
        self.code.push('\n');
    }

    fn push_comment(&mut self, comment: &str) {
        self.push_line(&format!("# {comment}"));
    }

    fn push_blank_line(&mut self) {
        if self.code.ends_with("\n\n") {
            return;
        }
        self.code.push('\n');
    }

    fn collect_symbols(&mut self, items: &[Item]) {
        self.enums = collect_enums(items);
        self.impls = collect_impls(items);
    }

    fn render_match_expr(&mut self, expr_match: &ExprMatch) -> Result<String> {
        let Some(scrutinee) = &expr_match.scrutinee else {
            return Err(eyre!("match expressions without scrutinee not supported").into());
        };
        let scrutinee_rendered = self.render_expr(scrutinee.as_ref())?;

        let mut fallback = "null".to_string();
        for case in expr_match.cases.iter().rev() {
            let mut cond_parts = Vec::new();
            cond_parts.push(self.render_expr(&case.cond)?);
            if let Some(pat) = &case.pat {
                cond_parts.push(render_pattern_condition(pat, &scrutinee_rendered)?);
            }
            if let Some(guard) = &case.guard {
                cond_parts.push(self.render_expr(guard.as_ref())?);
            }
            let cond_rendered = cond_parts.into_iter().join(" and ");

            let body = self.render_match_body(
                &scrutinee_rendered,
                case.pat.as_ref().map(|pat| pat.as_ref()),
                &case.body,
            )?;
            fallback = format!("({body} if ({cond_rendered}) else {fallback})");
        }
        Ok(fallback)
    }

    fn render_match_body(
        &mut self,
        scrutinee: &str,
        pat: Option<&Pattern>,
        body: &Expr,
    ) -> Result<String> {
        if let Some(binding) = pat.and_then(extract_tuple1_binding) {
            if let ExprKind::Name(name) = body.kind() {
                if render_name(name) == binding {
                    return Ok(format!("{scrutinee}.data[\"0\"]"));
                }
            }
            // Binding-aware lowering would require introducing a temporary variable.
            // For now, degrade rather than failing emission.
            return Ok("null".to_string());
        }
        self.render_expr(body)
    }
}

fn render_name(name: &Name) -> String {
    let rendered = format!("{name}");
    rendered.replace("::", ".")
}

fn normalize_type_name(rendered: &str) -> String {
    rendered.split('<').next().unwrap_or(rendered).to_string()
}

fn enum_variant_from_name(name: &Name) -> Option<(String, String)> {
    let rendered = normalize_type_name(&format!("{name}"));
    let mut segments = rendered.split("::");
    let enum_name = segments.next()?.to_string();
    let variant_name = segments.next()?.to_string();
    if segments.next().is_some() {
        return None;
    }
    Some((enum_name, variant_name))
}

fn escape_string(raw: &str) -> String {
    let mut out = String::new();
    for ch in raw.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            other => out.push(other),
        }
    }
    out
}

fn render_bin_op(kind: &BinOpKind) -> &'static str {
    match kind {
        BinOpKind::Add => "+",
        BinOpKind::AddTrait => "+",
        BinOpKind::Sub => "-",
        BinOpKind::Mul => "*",
        BinOpKind::Div => "/",
        BinOpKind::Mod => "%",
        BinOpKind::And => "and",
        BinOpKind::Or => "or",
        BinOpKind::Eq => "==",
        BinOpKind::Ne => "!=",
        BinOpKind::Lt => "<",
        BinOpKind::Le => "<=",
        BinOpKind::Gt => ">",
        BinOpKind::Ge => ">=",
        BinOpKind::BitAnd => "&",
        BinOpKind::BitOr => "|",
        BinOpKind::BitXor => "^",
        BinOpKind::Shl => "<<",
        BinOpKind::Shr => ">>",
    }
}
