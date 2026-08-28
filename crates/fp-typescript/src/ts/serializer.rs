use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;
use std::sync::RwLock;

use eyre::eyre;
use fp_core::ast::{
    AstSerializer, BlockStmt, Expr, ExprBlock, ExprConstBlock, ExprIntrinsicCall, ExprInvoke,
    ExprInvokeTarget, ExprKind, ExprStringTemplate, ExprStruct, File, FormatArgRef,
    FormatTemplatePart, FunctionParam, Ident, Item, Name, Pattern, Ty, TypeEnum, TypePrimitive,
    TypeStruct, TypeTuple, TypeVec, Value, ValueList, ValueMap, ValueMapEntry, ValueStruct,
    ValueTuple,
};
use fp_core::error::Result;
use fp_core::intrinsics::CallKind;
use fp_core::ops::UnOpKind;
use fp_core::writer::{IndentStyle, StyledWriter, WriterConfig};
use itertools::Itertools;

pub struct TypeScriptSerializer {
    emit_type_defs: bool,
    type_defs: RwLock<Option<String>>,
}

impl TypeScriptSerializer {
    pub fn new(emit_type_defs: bool) -> Self {
        Self {
            emit_type_defs,
            type_defs: RwLock::new(None),
        }
    }

    pub fn take_type_defs(&self) -> Option<String> {
        match self.type_defs.write() {
            Ok(mut w) => w.take(),
            Err(poison) => poison.into_inner().take(),
        }
    }
}

/// Marker impl — needed so `TypeScriptSerializer` can be stored as
/// `Arc<dyn AstSerializer>` in `FrontendResult` (see `frontend.rs`).
/// `serialize_file`/`serialize_package` are inherent methods below instead.
impl AstSerializer for TypeScriptSerializer {}

impl TypeScriptSerializer {
    pub fn serialize_file(&self, file: &File) -> Result<String> {
        let mut emitter = ScriptEmitter::new(ScriptFlavor::TypeScript {
            emit_type_defs: self.emit_type_defs,
        });
        emitter.visit_file(file)?;
        let (code, defs) = emitter.finish();
        match self.type_defs.write() {
            Ok(mut w) => *w = defs,
            Err(poison) => *poison.into_inner() = defs,
        }
        Ok(code)
    }

    /// Serializes a package into one TypeScript source file per module.
    /// Returns `Vec<(relative_path, code)>`.
    pub fn serialize_package(
        &self,
        source: &fp_core::ast::package::AstPackage,
    ) -> Result<Vec<(String, String)>> {
        fp_core::ast::package::split_package_into_modules(source)
            .into_iter()
            .map(|module| {
                let rel_path = module.relative_path();
                let file = File {
                    path: std::path::PathBuf::from(&rel_path),
                    attrs: Vec::new(),
                    collected_items: Vec::new(),
                    items: module.items,
                };
                let code = self.serialize_file(&file)?;
                Ok((rel_path, code))
            })
            .collect()
    }
}

fn write_package_files(
    config: &fp_core::backend::BackendConfig,
    package_name: &str,
    ext: &str,
    files: Vec<(String, String)>,
) -> Result<()> {
    let writer = fp_core::backend::PackageWriter::new(config.workspace_root.join(package_name));
    for (rel_path, code) in files {
        let rel = if rel_path.contains('.') {
            rel_path
        } else {
            format!("{rel_path}.{ext}")
        };
        writer.write_file(&rel, code)?;
    }
    Ok(())
}

/// `TargetBackend` wrapper around [`TypeScriptSerializer`] — a thin
/// wrapper rather than an impl directly on `TypeScriptSerializer` so the
/// serializer itself stays usable without a `BackendConfig` (e.g. by
/// tests that only need `serialize_file`).
pub struct TypeScriptBackend {
    serializer: TypeScriptSerializer,
    config: fp_core::backend::BackendConfig,
}

impl TypeScriptBackend {
    pub fn new(config: fp_core::backend::BackendConfig) -> Self {
        Self {
            serializer: TypeScriptSerializer::new(config.type_defs),
            config,
        }
    }
}

impl fp_core::backend::TargetBackend for TypeScriptBackend {
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        fp_core::capabilities::LanguageCapabilities::NATIVE
    }

    fn emit_package_artifact(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
        package_id: &fp_core::ast::package::PackageId,
        mir: &fp_core::mir::MirCodeUnit,
        lir: Option<&fp_core::lir::LirBlob>,
    ) -> Result<()> {
        let package = workspace.package_source(package_id)?;
        let files = self.serializer.serialize_package(&package)?;
        if let Some(output) = &self.config.single_file_output {
            let (_, code) = files
                .into_iter()
                .find(|(path, _)| path.ends_with(".ts") || !path.contains('/'))
                .ok_or_else(|| eyre!("single-file TypeScript package has no source file"))?;
            if let Some(parent) = output.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(output, code)?;
            if let Some(defs) = self.serializer.take_type_defs() {
                let defs_path = output.with_extension("d.ts");
                std::fs::write(defs_path, defs)?;
            }
            return Ok(());
        }
        write_package_files(&self.config, &package.name, "ts", files)
    }
}

pub struct JavaScriptSerializer;

impl JavaScriptSerializer {
    pub fn serialize_file(&self, file: &File) -> Result<String> {
        let mut emitter = ScriptEmitter::new(ScriptFlavor::JavaScript);
        emitter.visit_file(file)?;
        let (code, _) = emitter.finish();
        Ok(code)
    }

    /// Serializes a package into one JavaScript source file per module.
    /// Returns `Vec<(relative_path, code)>`.
    pub fn serialize_package(
        &self,
        source: &fp_core::ast::package::AstPackage,
    ) -> Result<Vec<(String, String)>> {
        fp_core::ast::package::split_package_into_modules(source)
            .into_iter()
            .map(|module| {
                let rel_path = module.relative_path();
                let file = File {
                    path: std::path::PathBuf::from(&rel_path),
                    attrs: Vec::new(),
                    collected_items: Vec::new(),
                    items: module.items,
                };
                let code = self.serialize_file(&file)?;
                Ok((rel_path, code))
            })
            .collect()
    }
}

pub struct JavaScriptBackend {
    config: fp_core::backend::BackendConfig,
}

impl JavaScriptBackend {
    pub fn new(config: fp_core::backend::BackendConfig) -> Self {
        Self { config }
    }
}

impl fp_core::backend::TargetBackend for JavaScriptBackend {
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        fp_core::capabilities::LanguageCapabilities::NATIVE
    }

    fn emit_package_artifact(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
        package_id: &fp_core::ast::package::PackageId,
        mir: &fp_core::mir::MirCodeUnit,
        lir: Option<&fp_core::lir::LirBlob>,
    ) -> Result<()> {
        let package = workspace.package_source(package_id)?;
        let files = JavaScriptSerializer.serialize_package(&package)?;
        if let Some(output) = &self.config.single_file_output {
            let (_, code) = files
                .into_iter()
                .find(|(path, _)| path.ends_with(".js") || !path.contains('/'))
                .ok_or_else(|| eyre!("single-file JavaScript package has no source file"))?;
            if let Some(parent) = output.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(output, code)?;
            return Ok(());
        }
        write_package_files(&self.config, &package.name, "js", files)
    }
}

#[derive(Clone)]
enum ScriptFlavor {
    TypeScript { emit_type_defs: bool },
    JavaScript,
}

struct ScriptEmitter {
    flavor: ScriptFlavor,
    writer: StyledWriter,
    type_defs: Vec<String>,
    structs: HashMap<String, TypeStruct>,
    seen_structs: HashSet<String>,
    saw_main: bool,
    invoked_main: bool,
    synthetic_main: bool,
}

impl ScriptEmitter {
    fn new(flavor: ScriptFlavor) -> Self {
        let writer_config = WriterConfig {
            indent_style: IndentStyle::Spaces(2),
            ..WriterConfig::default()
        };
        Self {
            flavor,
            writer: StyledWriter::new(writer_config),
            type_defs: Vec::new(),
            structs: HashMap::new(),
            seen_structs: HashSet::new(),
            saw_main: false,
            invoked_main: false,
            synthetic_main: false,
        }
    }

    /// Write `header` followed by ` {`, run `body` at one deeper indent
    /// level, then close with `}`. Unlike [`StyledWriter::block`], the
    /// closure gets the whole emitter (not just the writer) since most
    /// bodies need to read/mutate other emitter state (`structs`, `flavor`,
    /// recursive `emit_*`/`render_*` calls).
    fn block<F>(&mut self, header: &str, body: F) -> Result<()>
    where
        F: FnOnce(&mut Self) -> Result<()>,
    {
        self.block_closed(&format!("{header} {{"), "}", body)
    }

    /// Like [`Self::block`], but with the exact opening/closing lines given
    /// verbatim — needed for shapes that aren't `header { ... }`, e.g. a
    /// `return { ... };` object literal or `const X = Object.freeze({ ... });`.
    fn block_closed<F>(&mut self, open_line: &str, close_line: &str, body: F) -> Result<()>
    where
        F: FnOnce(&mut Self) -> Result<()>,
    {
        self.writer.write_line(open_line);
        self.writer.increase_indent();
        let result = body(self);
        self.writer.decrease_indent();
        self.writer.write_line(close_line);
        result
    }

    fn visit_file(&mut self, file: &File) -> Result<()> {
        self.visit_file_items(file)?;
        Ok(())
    }

    fn visit_file_items(&mut self, file: &File) -> Result<()> {
        for item in &file.items {
            self.emit_item(item)?;
        }
        Ok(())
    }

    fn emit_item(&mut self, item: &Item) -> Result<()> {
        if let Some(struct_item) = item.as_struct() {
            self.writer.ensure_blank_line();
            self.emit_struct(&struct_item.value)?;
            return Ok(());
        }

        if let Some(enum_item) = item.as_enum() {
            self.writer.ensure_blank_line();
            self.emit_enum(&enum_item.value)?;
            return Ok(());
        }

        if let Some(const_item) = item.as_const() {
            self.writer.ensure_blank_line();
            self.emit_const(const_item)?;
            return Ok(());
        }

        if let Some(function_item) = item.as_function() {
            self.writer.ensure_blank_line();
            if let Err(err) = self.emit_function(function_item) {
                self.writer.write_line(format!(
                    "// skipped unsupported function {}: {}",
                    function_item.name, err
                ));
            }
            return Ok(());
        }

        if let Some(module) = item.as_module() {
            for child in &module.items {
                self.emit_item(child)?;
            }
            return Ok(());
        }

        if let Some(expr) = item.as_expr() {
            if let ExprKind::Block(block) = expr.kind() {
                if let Err(err) = self.emit_script_block(block) {
                    self.writer
                        .write_line(format!("// skipped unsupported script block: {}", err));
                }
            } else if let Ok(rendered) = self.render_expr(expr) {
                self.writer.write_line(format!("{};", rendered));
            } else {
                self.writer
                    .write_line("// skipped unsupported top-level expression");
            }
        }

        // Ignore imports and other unsupported declarations for script targets
        Ok(())
    }

    fn emit_struct(&mut self, struct_def: &TypeStruct) -> Result<()> {
        let struct_name = struct_def.name.name.clone();
        self.structs.insert(struct_name.clone(), struct_def.clone());
        if !self.seen_structs.insert(struct_name.clone()) {
            return Ok(());
        }

        if let ScriptFlavor::TypeScript { emit_type_defs } = &self.flavor {
            let interface_text = self.build_interface_text(struct_def);
            self.writer.write_verbatim(&interface_text);
            self.writer.newline();

            if *emit_type_defs {
                self.type_defs.push(interface_text.trim_end().to_string());
            }
        }

        self.emit_struct_factory(struct_def)?;
        self.writer.newline();
        self.emit_struct_size_const(struct_def)?;
        self.writer.newline();
        Ok(())
    }

    fn emit_struct_factory(&mut self, struct_def: &TypeStruct) -> Result<()> {
        let params = struct_def
            .fields
            .iter()
            .map(|field| match &self.flavor {
                ScriptFlavor::TypeScript { .. } => {
                    let ty = self.ts_type_from_ty(&field.value);
                    format!("{}: {}", field.name.name, ty)
                }
                ScriptFlavor::JavaScript => field.name.name.clone(),
            })
            .join(", ");

        let return_annotation = match &self.flavor {
            ScriptFlavor::TypeScript { .. } => format!(": {}", struct_def.name.name),
            ScriptFlavor::JavaScript => String::new(),
        };

        let header = format!(
            "function create{}({}){}",
            struct_def.name.name, params, return_annotation
        );
        self.block(&header, |slf| {
            slf.block_closed("return {", "};", |slf| {
                for (index, field) in struct_def.fields.iter().enumerate() {
                    let suffix = if index + 1 == struct_def.fields.len() {
                        ""
                    } else {
                        ","
                    };
                    slf.writer.write_line(format!(
                        "{}: {}{}",
                        field.name.name, field.name.name, suffix
                    ));
                }
                Ok(())
            })
        })
    }

    fn emit_struct_size_const(&mut self, struct_def: &TypeStruct) -> Result<()> {
        let const_name = format!("{}_SIZE", to_upper_snake(struct_def.name.name.as_str()));
        match self.flavor.clone() {
            ScriptFlavor::TypeScript { emit_type_defs } => {
                self.writer.write_line(format!(
                    "const {}: number = {};",
                    const_name,
                    struct_def.fields.len()
                ));
                if emit_type_defs {
                    self.type_defs
                        .push(format!("declare const {}: number;", const_name));
                }
            }
            ScriptFlavor::JavaScript => {
                self.writer.write_line(format!(
                    "const {} = {};",
                    const_name,
                    struct_def.fields.len()
                ));
            }
        }
        Ok(())
    }

    fn emit_enum(&mut self, enum_def: &TypeEnum) -> Result<()> {
        match self.flavor.clone() {
            ScriptFlavor::TypeScript { emit_type_defs } => {
                self.block(&format!("enum {}", enum_def.name.name), |slf| {
                    for variant in &enum_def.variants {
                        slf.writer.write_line(format!("{},", variant.name.name));
                    }
                    Ok(())
                })?;
                if emit_type_defs {
                    let mut block = String::new();
                    let _ = writeln!(block, "enum {} {{", enum_def.name.name);
                    for variant in &enum_def.variants {
                        let _ = writeln!(block, "  {},", variant.name.name);
                    }
                    let _ = writeln!(block, "}}");
                    self.type_defs.push(block.trim_end().to_string());
                }
            }
            ScriptFlavor::JavaScript => {
                self.block_closed(
                    &format!("const {} = Object.freeze({{", enum_def.name.name),
                    "});",
                    |slf| {
                        for variant in &enum_def.variants {
                            slf.writer.write_line(format!(
                                "{}: \"{}\",",
                                variant.name.name, variant.name.name
                            ));
                        }
                        Ok(())
                    },
                )?;
            }
        }
        Ok(())
    }

    fn emit_const(&mut self, const_item: &fp_core::ast::ItemDefConst) -> Result<()> {
        let name = const_item.name.name.clone();
        let value_expr = self.render_expr(const_item.value.as_ref())?;
        match self.flavor.clone() {
            ScriptFlavor::TypeScript { emit_type_defs } => {
                let ty = const_item
                    .ty
                    .as_ref()
                    .map(|ty| self.ts_type_from_ty(ty))
                    .unwrap_or_else(|| "any".into());
                self.writer
                    .write_line(format!("const {}: {} = {};", name, ty, value_expr));
                if emit_type_defs {
                    self.type_defs
                        .push(format!("declare const {}: {};", name, ty));
                }
            }
            ScriptFlavor::JavaScript => {
                self.writer
                    .write_line(format!("const {} = {};", name, value_expr));
            }
        }
        Ok(())
    }

    fn emit_function(&mut self, func: &fp_core::ast::ItemDefFunction) -> Result<()> {
        if func.name.name == "main" {
            self.saw_main = true;
        }

        let params = self.render_params(&func.sig.params)?;
        let header = match &self.flavor {
            ScriptFlavor::TypeScript { .. } => {
                let ret = func
                    .sig
                    .ret_ty
                    .as_ref()
                    .map(|ty| self.ts_type_from_ty(ty))
                    .unwrap_or_else(|| "void".to_string());
                format!("function {}({}): {}", func.name.name, params, ret)
            }
            ScriptFlavor::JavaScript => format!("function {}({})", func.name.name, params),
        };

        self.block(&header, |slf| slf.emit_block(&func.body, true))
    }

    fn emit_block(&mut self, block: &ExprBlock, return_tail: bool) -> Result<()> {
        for (idx, stmt) in block.stmts.iter().enumerate() {
            let is_tail = idx + 1 == block.stmts.len();
            self.emit_stmt(stmt, return_tail && is_tail)?;
        }
        Ok(())
    }

    fn emit_script_block(&mut self, block: &ExprBlock) -> Result<()> {
        for stmt in &block.stmts {
            if let BlockStmt::Item(item) = stmt {
                self.emit_item(item.as_ref())?;
            }
        }

        let has_non_item_stmts = block
            .stmts
            .iter()
            .any(|stmt| !matches!(stmt, BlockStmt::Item(_)));
        if !has_non_item_stmts {
            return Ok(());
        }

        if !self.saw_main {
            self.saw_main = true;
            self.synthetic_main = true;
        }
        let header = match self.flavor.clone() {
            ScriptFlavor::TypeScript { .. } => "function main(): void".to_string(),
            ScriptFlavor::JavaScript => "function main()".to_string(),
        };
        self.block(&header, |slf| {
            for stmt in &block.stmts {
                if matches!(stmt, BlockStmt::Item(_)) {
                    continue;
                }
                slf.emit_stmt(stmt, false)?;
            }
            Ok(())
        })
    }

    fn block_tail_value_expr(block: &ExprBlock) -> Option<&Expr> {
        if block.stmts.len() != 1 {
            return None;
        }
        match &block.stmts[0] {
            BlockStmt::Expr(stmt) if stmt.has_value() => Some(stmt.expr.as_ref()),
            _ => None,
        }
    }

    fn render_if_expr(&mut self, if_expr: &fp_core::ast::ExprIf) -> Result<String> {
        let cond = self.render_expr(if_expr.cond.as_ref())?;

        let then_expr = match if_expr.then.as_ref().kind() {
            ExprKind::Block(block) => Self::block_tail_value_expr(block)
                .map(|e| self.render_expr(e))
                .transpose()?,
            _ => Some(self.render_expr(if_expr.then.as_ref())?),
        };

        let else_expr = match if_expr.elze.as_deref() {
            None => None,
            Some(elze) => match elze.kind() {
                ExprKind::Block(block) => Self::block_tail_value_expr(block)
                    .map(|e| self.render_expr(e))
                    .transpose()?,
                _ => Some(self.render_expr(elze)?),
            },
        };

        match (then_expr, else_expr) {
            (Some(then_expr), Some(else_expr)) => {
                Ok(format!("({cond} ? {then_expr} : {else_expr})"))
            }
            (Some(then_expr), None) => Ok(format!("({cond} ? {then_expr} : undefined)")),
            _ => Err(eyre!("Unsupported if-expression shape for script targets").into()),
        }
    }

    fn render_match_expr(&mut self, match_expr: &fp_core::ast::ExprMatch) -> Result<String> {
        // Lowered match cases already contain boolean conditions.
        // Render as a right-associated ternary chain.
        let mut out: Option<String> = None;
        for case in match_expr.cases.iter().rev() {
            let cond = self.render_expr(case.cond.as_ref())?;
            let body_expr = match case.body.as_ref().kind() {
                ExprKind::Block(block) => Self::block_tail_value_expr(block)
                    .map(|e| self.render_expr(e))
                    .transpose()?,
                _ => Some(self.render_expr(case.body.as_ref())?),
            }
            .ok_or_else(|| eyre!("Unsupported match arm body for script targets"))?;

            out = Some(match out {
                None => body_expr,
                Some(tail) => format!("({cond} ? {body_expr} : {tail})"),
            });
        }
        out.ok_or_else(|| eyre!("match expression has no cases").into())
    }

    fn emit_if_stmt(&mut self, if_expr: &fp_core::ast::ExprIf, return_value: bool) -> Result<()> {
        let cond = self.render_expr(if_expr.cond.as_ref())?;
        self.block(&format!("if ({cond})"), |slf| {
            slf.emit_branch_body(if_expr.then.as_ref(), return_value)
        })?;

        if let Some(elze) = if_expr.elze.as_deref() {
            self.block("else", |slf| slf.emit_branch_body(elze, return_value))?;
        } else if return_value {
            self.block("else", |slf| {
                slf.writer.write_line("return undefined;");
                Ok(())
            })?;
        }

        Ok(())
    }

    fn emit_while_stmt(&mut self, while_expr: &fp_core::ast::ExprWhile) -> Result<()> {
        let cond = self.render_expr(while_expr.cond.as_ref())?;
        self.block(&format!("while ({cond})"), |slf| {
            slf.emit_branch_body(while_expr.body.as_ref(), false)
        })
    }

    fn emit_loop_stmt(&mut self, loop_expr: &fp_core::ast::ExprLoop) -> Result<()> {
        let _ = loop_expr.label.as_ref();
        self.block("while (true)", |slf| {
            slf.emit_branch_body(loop_expr.body.as_ref(), false)
        })
    }

    fn emit_for_stmt(&mut self, for_expr: &fp_core::ast::ExprFor) -> Result<()> {
        let pat = self.render_pattern(for_expr.pat.as_ref());
        match for_expr.iter.as_ref().kind() {
            ExprKind::Range(range) => {
                let start = range
                    .start
                    .as_deref()
                    .map(|e| self.render_expr(e))
                    .transpose()?
                    .unwrap_or_else(|| "0".to_string());
                let end = range
                    .end
                    .as_deref()
                    .map(|e| self.render_expr(e))
                    .transpose()?
                    .unwrap_or_else(|| "0".to_string());
                let cmp = match range.limit {
                    fp_core::ast::ExprRangeLimit::Exclusive => "<",
                    fp_core::ast::ExprRangeLimit::Inclusive => "<=",
                };
                let step = range
                    .step
                    .as_deref()
                    .map(|e| self.render_expr(e))
                    .transpose()?;
                let update = match step {
                    Some(step) => format!("{pat} += {step}"),
                    None => format!("{pat}++"),
                };
                let header = format!("for (let {pat} = {start}; {pat} {cmp} {end}; {update})");
                self.block(&header, |slf| {
                    slf.emit_branch_body(for_expr.body.as_ref(), false)
                })
            }
            _ => {
                let iter = self.render_expr(for_expr.iter.as_ref())?;
                let header = format!("for (const {pat} of {iter})");
                self.block(&header, |slf| {
                    slf.emit_branch_body(for_expr.body.as_ref(), false)
                })
            }
        }
    }

    fn emit_branch_body(&mut self, expr: &Expr, return_value: bool) -> Result<()> {
        match expr.kind() {
            ExprKind::Block(block) => self.emit_block(block, return_value),
            _ => {
                let rendered = self.render_expr(expr)?;
                if return_value {
                    self.writer.write_line(format!("return {rendered};"));
                } else {
                    self.writer.write_line(format!("{rendered};"));
                }
                Ok(())
            }
        }
    }

    fn emit_stmt(&mut self, stmt: &BlockStmt, return_value: bool) -> Result<()> {
        match stmt {
            BlockStmt::Let(stmt_let) => {
                let name = self.render_pattern(&stmt_let.pat);
                if let Some(init) = &stmt_let.init {
                    let value = self.render_expr(init)?;
                    self.writer.write_line(format!("let {} = {};", name, value));
                } else {
                    self.writer.write_line(format!("let {};", name));
                }
            }
            BlockStmt::Expr(expr_stmt) => {
                let expr = expr_stmt.expr.as_ref();
                if let ExprKind::If(if_expr) = expr.kind() {
                    self.emit_if_stmt(if_expr, return_value)?;
                    return Ok(());
                }
                if let ExprKind::While(while_expr) = expr.kind() {
                    self.emit_while_stmt(while_expr)?;
                    return Ok(());
                }
                if let ExprKind::Loop(loop_expr) = expr.kind() {
                    self.emit_loop_stmt(loop_expr)?;
                    return Ok(());
                }
                if let ExprKind::For(for_expr) = expr.kind() {
                    self.emit_for_stmt(for_expr)?;
                    return Ok(());
                }
                if let ExprKind::Return(ret) = expr.kind() {
                    if let Some(value) = &ret.value {
                        let rendered = self.render_expr(value)?;
                        self.writer.write_line(format!("return {};", rendered));
                    } else {
                        self.writer.write_line("return;");
                    }
                } else if let ExprKind::Break(_) = expr.kind() {
                    self.writer.write_line("break;");
                } else if let ExprKind::Continue(_) = expr.kind() {
                    self.writer.write_line("continue;");
                } else if let ExprKind::IntrinsicCall(call) = expr.kind() {
                    self.emit_intrinsic_statement(call)?;
                } else if let ExprKind::Block(block_expr) = expr.kind() {
                    self.emit_block(block_expr, false)?;
                } else {
                    let rendered = self.render_expr(expr)?;
                    if return_value {
                        self.writer.write_line(format!("return {};", rendered));
                    } else {
                        self.writer.write_line(format!("{};", rendered));
                    }
                }
            }
            BlockStmt::Item(item) => self.emit_item(item.as_ref())?,
            BlockStmt::Defer(_) => {
                return Err(
                    eyre!("defer statements are not supported during TypeScript emission").into(),
                );
            }
            BlockStmt::Noop => {}
        }
        Ok(())
    }

    fn emit_intrinsic_statement(&mut self, call: &ExprIntrinsicCall) -> Result<()> {
        match call.kind {
            CallKind::Print | CallKind::Println => {
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
                self.writer.write_line(format!("console.log({});", joined));
                Ok(())
            }
            _ => {
                let rendered = self.render_intrinsic_expr(call)?;
                self.writer.write_line(format!("{};", rendered));
                Ok(())
            }
        }
    }

    fn render_params(&mut self, params: &[FunctionParam]) -> Result<String> {
        let rendered = params
            .iter()
            .map(|param| match &self.flavor {
                ScriptFlavor::TypeScript { .. } => {
                    let ty = self.ts_type_from_ty(&param.ty);
                    Ok(format!("{}: {}", param.name.name, ty))
                }
                ScriptFlavor::JavaScript => Ok(param.name.name.clone()),
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(rendered.join(", "))
    }

    fn render_expr(&mut self, expr: &Expr) -> Result<String> {
        match expr.kind() {
            ExprKind::Value(value) => Ok(render_js_value(value.as_ref())),
            ExprKind::Name(name) => Ok(self.render_name(name)),
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
            ExprKind::UnOp(un_op) => match &un_op.op {
                UnOpKind::Neg => Ok(format!("(-{})", self.render_expr(un_op.val.as_ref())?)),
                UnOpKind::Not => Ok(format!("(!{})", self.render_expr(un_op.val.as_ref())?)),
                other => Err(eyre!("Unsupported unary op in target emitter: {:?}", other).into()),
            },
            ExprKind::FormatString(format) => Ok(render_template_literal(format)),
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
                "Array({}).fill({})",
                self.render_expr(repeat.len.as_ref())?,
                self.render_expr(repeat.elem.as_ref())?
            )),
            ExprKind::Tuple(tuple) => {
                let values = tuple
                    .values
                    .iter()
                    .map(|expr| self.render_expr(expr))
                    .collect::<Result<Vec<_>>>()?;
                Ok(format!("[{}]", values.join(", ")))
            }
            ExprKind::ConstBlock(block) => self.render_const_block_expr(block),
            ExprKind::IntrinsicCall(call) => self.render_intrinsic_expr(call),
            ExprKind::Paren(paren) => Ok(format!("({})", self.render_expr(paren.expr.as_ref())?)),
            ExprKind::Reference(reference) => self.render_expr(reference.referee.as_ref()),
            ExprKind::Dereference(deref) => self.render_expr(deref.referee.as_ref()),
            ExprKind::Cast(cast) => self.render_expr(cast.expr.as_ref()),
            ExprKind::If(if_expr) => self.render_if_expr(if_expr),
            ExprKind::Match(match_expr) => self.render_match_expr(match_expr),
            ExprKind::Closure(closure) => {
                let params = closure
                    .params
                    .iter()
                    .map(|pat| self.render_pattern(pat))
                    .join(", ");
                let body = self.render_expr(closure.body.as_ref())?;
                Ok(format!("({params}) => {body}"))
            }
            other => Err(eyre!("Unsupported expression in target emitter: {:?}", other).into()),
        }
    }

    fn render_invoke(&mut self, invoke: &ExprInvoke) -> Result<String> {
        let target = match &invoke.target {
            ExprInvokeTarget::Function(name) => self.render_name(name),
            ExprInvokeTarget::Method(select) => {
                let obj = self.render_expr(select.obj.as_ref())?;
                if select.field.name == "len" && invoke.args.is_empty() {
                    return Ok(format!("{}.length", obj));
                }
                format!(
                    "{}.{}",
                    obj,
                    self.render_method_name(&select.field, invoke.args.len())
                )
            }
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
                    eyre!("Invoking type objects is not supported for script targets").into(),
                );
            }
            ExprInvokeTarget::Closure(_) => {
                return Err(eyre!("Closure invocation is not supported for script targets").into());
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
            CallKind::Format => {
                if let Some((template, args, kwargs)) = extract_format_call(call) {
                    self.render_format_string(template, args, kwargs)
                } else {
                    Err(eyre!("format intrinsic expects a format template").into())
                }
            }
            CallKind::Len => {
                if !call.args.is_empty() {
                    Ok(format!("{}.length", self.render_expr(&call.args[0])?))
                } else {
                    Err(eyre!("len intrinsic expects an argument").into())
                }
            }
            CallKind::SizeOf => {
                if call.args.len() == 1 {
                    if let Some(ty_name) = self.extract_type_name(&call.args[0]) {
                        Ok(format!("{}_SIZE", to_upper_snake(&ty_name)))
                    } else {
                        Ok("0".to_string())
                    }
                } else {
                    Ok("0".to_string())
                }
            }
            CallKind::FieldCount => {
                if call.args.len() == 1 {
                    if let Some(ty_name) = self.extract_type_name(&call.args[0]) {
                        Ok(format!("{}_SIZE", to_upper_snake(&ty_name)))
                    } else {
                        Ok("0".to_string())
                    }
                } else {
                    Ok("0".to_string())
                }
            }
            CallKind::HasField => {
                if call.args.len() >= 2 {
                    let Some(ty_name) = self.extract_type_name(&call.args[0]) else {
                        return Ok("false".to_string());
                    };
                    let Some(field) = self.extract_string_literal(&call.args[1]) else {
                        return Ok("false".to_string());
                    };
                    let has = self
                        .structs
                        .get(&ty_name)
                        .is_some_and(|def| def.fields.iter().any(|f| f.name.name == field));
                    Ok(if has { "true" } else { "false" }.to_string())
                } else {
                    Ok("false".to_string())
                }
            }
            CallKind::MethodCount => Ok("0".to_string()),
            _ => Err(eyre!("Unsupported intrinsic call {:?}", call.kind).into()),
        }
    }

    fn render_const_block_expr(&mut self, block: &ExprConstBlock) -> Result<String> {
        match block.expr.kind() {
            ExprKind::Block(expr_block) => {
                if let Some(tail) = Self::block_tail_value_expr(expr_block) {
                    self.render_expr(tail)
                } else {
                    Err(eyre!("const block requires a trailing expression").into())
                }
            }
            _ => self.render_expr(block.expr.as_ref()),
        }
    }

    fn render_struct_literal(&mut self, struct_expr: &ExprStruct) -> Result<String> {
        if let Some(name) = self.extract_struct_name(struct_expr.name.as_ref()) {
            if let Some(struct_def) = self.structs.get(&name).cloned() {
                let mut args = Vec::with_capacity(struct_def.fields.len());
                for field_def in &struct_def.fields {
                    let value_expr = struct_expr
                        .fields
                        .iter()
                        .find(|field| field.name.name == field_def.name.name)
                        .and_then(|field| field.value.as_ref());
                    if let Some(expr) = value_expr {
                        args.push(self.render_expr(expr)?);
                    } else {
                        args.push("undefined".to_string());
                    }
                }
                return Ok(format!("create{}({})", name, args.join(", ")));
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
        let mut uses_placeholders = false;
        for part in &format.parts {
            if matches!(part, FormatTemplatePart::Placeholder(_)) {
                uses_placeholders = true;
                break;
            }
        }

        if !uses_placeholders && kwargs.is_empty() {
            let literal = format
                .parts
                .iter()
                .filter_map(|part| {
                    if let FormatTemplatePart::Literal(text) = part {
                        Some(text.as_str())
                    } else {
                        None
                    }
                })
                .collect::<String>();
            return Ok(serde_json::to_string(&literal).unwrap_or_else(|_| "\"\"".into()));
        }

        let mut template = String::from("`");
        let mut implicit_index = 0usize;
        for part in &format.parts {
            match part {
                FormatTemplatePart::Literal(text) => {
                    template.push_str(&escape_template_literal(text));
                }
                FormatTemplatePart::Placeholder(placeholder) => {
                    template.push_str("${");
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
                    template.push('}');
                }
            }
        }
        template.push('`');
        Ok(template)
    }

    fn render_name(&self, name: &Name) -> String {
        name.to_string()
            .split("::")
            .map(|segment| segment.to_string())
            .collect::<Vec<_>>()
            .join(".")
    }

    fn render_pattern(&self, pattern: &Pattern) -> String {
        if let Some(ident) = pattern.as_ident() {
            ident.name.clone()
        } else {
            format!("/* pattern {:?} */", pattern)
        }
    }

    fn extract_struct_name(&self, expr: &Expr) -> Option<String> {
        match expr.kind() {
            ExprKind::Name(name) => name
                .to_string()
                .split("::")
                .map(|segment| segment.to_string())
                .last(),
            _ => None,
        }
    }

    fn extract_type_name(&self, expr: &Expr) -> Option<String> {
        match expr.kind() {
            ExprKind::Name(_) => self.extract_struct_name(expr),
            ExprKind::Value(value) => match value.as_ref() {
                Value::Type(ty) => match ty {
                    Ty::Struct(def) => Some(def.name.name.clone()),
                    Ty::Enum(def) => Some(def.name.name.clone()),
                    Ty::Expr(inner) => self.extract_struct_name(inner.as_ref()),
                    other => Some(format!("{other}")),
                },
                _ => None,
            },
            _ => None,
        }
    }

    fn extract_string_literal(&self, expr: &Expr) -> Option<String> {
        match expr.kind() {
            ExprKind::Value(value) => match value.as_ref() {
                Value::String(s) => Some(s.value.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    fn render_method_name(&self, ident: &Ident, arg_len: usize) -> String {
        match ident.name.as_str() {
            "to_string" if arg_len == 0 => "toString".into(),
            other => other.into(),
        }
    }

    fn finish(mut self) -> (String, Option<String>) {
        if self.saw_main && !self.invoked_main {
            self.writer.ensure_blank_line();
            self.writer.write_line("main();");
            self.invoked_main = true;
        }

        let code = self.writer.finish();

        let type_defs = match &self.flavor {
            ScriptFlavor::TypeScript {
                emit_type_defs: true,
            } if !self.type_defs.is_empty() => {
                let mut defs = self.type_defs.join("\n\n");
                defs.push('\n');
                Some(defs)
            }
            _ => None,
        };

        (code, type_defs)
    }

    fn build_interface_text(&self, struct_def: &TypeStruct) -> String {
        let mut output = String::new();
        let _ = writeln!(output, "interface {} {{", struct_def.name.name);
        for field in &struct_def.fields {
            let ty = self.ts_type_from_ty(&field.value);
            let _ = writeln!(output, "  {}: {};", field.name.name, ty);
        }
        let _ = writeln!(output, "}}");
        output
    }

    fn ts_type_from_ty(&self, ty: &Ty) -> String {
        match ty {
            Ty::Primitive(primitive) => match primitive {
                TypePrimitive::Bool => "boolean".into(),
                TypePrimitive::String | TypePrimitive::Char => "string".into(),
                TypePrimitive::Int(_) | TypePrimitive::Decimal(_) => "number".into(),
                TypePrimitive::List => "Array<any>".into(),
            },
            Ty::Vec(TypeVec { ty }) => format!("Array<{}>", self.ts_type_from_ty(ty)),
            Ty::Tuple(TypeTuple { types }) => {
                if types.is_empty() {
                    "[]".into()
                } else {
                    let members = types
                        .iter()
                        .map(|ty| self.ts_type_from_ty(ty))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("[{}]", members)
                }
            }
            Ty::Struct(struct_ty) => struct_ty.name.name.clone(),
            Ty::Enum(enum_ty) => enum_ty.name.name.clone(),
            Ty::Reference(reference) => self.ts_type_from_ty(&reference.ty),
            Ty::Expr(expr) => match expr.kind() {
                ExprKind::Name(name) => {
                    if let Some(ident) = name.as_ident() {
                        map_ident_to_ts(ident.name.as_str())
                    } else {
                        map_ident_to_ts(name.to_string().split("::").last().unwrap_or("any"))
                    }
                }
                _ => "any".into(),
            },
            Ty::Any(_) | Ty::Unknown(_) => "any".into(),
            Ty::Unit(_) => "void".into(),
            _ => "any".into(),
        }
    }
}

fn render_bin_op(kind: &fp_core::ops::BinOpKind) -> &'static str {
    use fp_core::ops::BinOpKind::*;
    match kind {
        Add => "+",
        Sub => "-",
        Mul => "*",
        Div => "/",
        Mod => "%",
        Eq => "==",
        Ne => "!=",
        Lt => "<",
        Le => "<=",
        Gt => ">",
        Ge => ">=",
        And => "&&",
        Or => "||",
        BitAnd => "&",
        BitOr => "|",
        BitXor => "^",
        Shl => "<<",
        Shr => ">>",
        _ => "/* op */",
    }
}

fn escape_template_literal(text: &str) -> String {
    text.replace('\\', "\\\\").replace('`', "\\`")
}

fn render_js_value(value: &Value) -> String {
    match value {
        Value::Int(v) => v.value.to_string(),
        Value::Decimal(v) => format_decimal(v.value),
        Value::Bool(v) => {
            if v.value {
                "true".into()
            } else {
                "false".into()
            }
        }
        Value::String(v) => serde_json::to_string(&v.value).unwrap_or_else(|_| "\"\"".into()),
        Value::List(ValueList { values }) => {
            let rendered = values.iter().map(render_js_value).collect::<Vec<_>>();
            format!("[{}]", rendered.join(", "))
        }
        Value::Map(ValueMap { entries }) => {
            let rendered = entries
                .iter()
                .map(|ValueMapEntry { key, value }| {
                    format!("{}: {}", render_js_value(key), render_js_value(value))
                })
                .collect::<Vec<_>>();
            format!("{{{}}}", rendered.join(", "))
        }
        Value::Struct(ValueStruct { structural, .. }) => {
            let rendered = structural
                .fields
                .iter()
                .map(|field| format!("{}: {}", field.name.name, render_js_value(&field.value)))
                .collect::<Vec<_>>();
            format!("{{{}}}", rendered.join(", "))
        }
        Value::Tuple(ValueTuple { values }) => {
            let rendered = values.iter().map(render_js_value).collect::<Vec<_>>();
            format!("[{}]", rendered.join(", "))
        }
        Value::Unit(_) | Value::Null(_) | Value::Undefined(_) | Value::None(_) => {
            "undefined".into()
        }
        Value::Option(option) => option
            .value
            .as_ref()
            .map(|inner| render_js_value(inner))
            .unwrap_or_else(|| "undefined".into()),
        _ => "undefined".into(),
    }
}

fn format_decimal(value: f64) -> String {
    if value == 0.0 {
        if value.is_sign_negative() {
            "-0.0".to_string()
        } else {
            "0.0".to_string()
        }
    } else {
        let mut text = value.to_string();
        if !text.contains('.') && !text.contains('e') && !text.contains('E') {
            text.push_str(".0");
        }
        text
    }
}

fn to_upper_snake(name: &str) -> String {
    let mut result = String::new();
    let mut prev_lower = false;
    for ch in name.chars() {
        if ch.is_uppercase() {
            if prev_lower {
                result.push('_');
            }
            result.push(ch);
            prev_lower = false;
        } else if ch.is_alphanumeric() {
            result.push(ch.to_ascii_uppercase());
            prev_lower = true;
        } else {
            result.push('_');
            prev_lower = false;
        }
    }
    if result.is_empty() {
        name.to_ascii_uppercase()
    } else {
        result
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

fn render_template_literal(format: &ExprStringTemplate) -> String {
    let mut literal = String::new();
    for part in &format.parts {
        match part {
            FormatTemplatePart::Literal(text) => literal.push_str(text),
            FormatTemplatePart::Placeholder(_) => literal.push_str("{...}"),
        }
    }
    serde_json::to_string(&literal).unwrap_or_else(|_| "\"\"".into())
}

fn map_ident_to_ts(name: &str) -> String {
    match name {
        "String" | "str" => "string".into(),
        "char" => "string".into(),
        "bool" => "boolean".into(),
        "u8" | "u16" | "u32" | "u64" | "usize" | "i8" | "i16" | "i32" | "i64" | "isize" => {
            "number".into()
        }
        "f32" | "f64" => "number".into(),
        other => other.into(),
    }
}
