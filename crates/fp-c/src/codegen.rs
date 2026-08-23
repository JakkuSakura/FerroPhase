//! Real C source/header emitter for FerroPhase AST — distinct from the
//! Clang-based frontend and the mislabeled `CSerializer` in `lib.rs` (which
//! despite its name emits FerroPhase pretty-print syntax, not C). This
//! module is modeled on `fp-golang/src/serializer.rs`'s `GoEmitter`/
//! `GoSerializer`/`GoBackend` trio, adapted for the fact that C needs two
//! output files (header + source) per module instead of Go's one.
//!
//! Known, deliberate limitations (permissive comment-fallback rather than a
//! hard error, matching Go's own precedent):
//! - Closures (`ExprKind::Closure`): no environment-capture/closure-conversion
//!   pass exists; emitted as a `/* unsupported */` comment placeholder.
//! - Dynamic collections (`Ty::Vec`, growable `Ty::Slice`/`Ty::Array` with a
//!   non-literal length): C has no runtime for these; lowered to `void*` with
//!   an inline comment rather than a real allocator-backed type.
//! - Generics/`Ty::Any`/`Ty::Unknown` reaching this layer (they shouldn't,
//!   post-monomorphization, but if they do): lowered to `void*`/`int`
//!   best-effort so the file still parses as C, with a comment noting the gap.
//! - `ExprKind::Match` against a non-enum, non-integer scrutinee, or with
//!   nested destructuring patterns: lowered to a best-effort `if`/`else if`
//!   chain; only simple identifier/wildcard/variant patterns are handled.
//! - Implicit tail-expression returns (a function/block's last expression
//!   with no `return`, Rust-style): emitted as a bare expression statement,
//!   not promoted to `return <expr>;` — no such normalization pass exists at
//!   this layer (matching `GoEmitter`'s own precedent for the same gap).

use std::collections::{BTreeMap, BTreeSet};

use fp_core::ast::{
    BlockStmt, BlockStmtExpr, DecimalType, Expr, ExprBlock, ExprField, ExprIf, ExprIntrinsicCall,
    ExprInvoke, ExprInvokeTarget, ExprKind, ExprLoop, ExprMatch, ExprMatchCase, ExprStruct,
    ExprWhile, File, FunctionSignature, Item, ItemDefEnum, ItemDefFunction, ItemDefStruct,
    ItemKind, Name, PatternKind, Ty, TypeEnum, TypeInt, TypePrimitive, TypeStruct, TypeTuple, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::CallKind;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::ast::package::AstPackage;
use fp_core::writer::{BraceStyle, StyledWriter, WriterConfig};

/// Public entry point used by the CLI target dispatch (`BuiltinLanguageTarget::C`).
#[derive(Clone, Debug, Default)]
pub struct CSourceSerializer;

impl CSourceSerializer {
    pub fn new() -> Self {
        Self
    }

    /// Renders one module into `(header, source)` C text.
    pub fn serialize_file(&self, file: &File) -> Result<(String, String)> {
        let mut emitter = CEmitter::new(module_header_name(file));
        emitter.emit_file(file)?;
        Ok(emitter.finish())
    }

    /// Serializes a package into one `.h`/`.c` pair per module.
    /// Returns `Vec<(relative_path, code)>` with two entries per module.
    pub fn serialize_package(&self, source: &AstPackage) -> Result<Vec<(String, String)>> {
        let mut out = Vec::new();
        for module in fp_core::ast::package::split_package_into_modules(source) {
            let rel_path = module.relative_path();
            let file = File {
                path: std::path::PathBuf::from(&rel_path),
                attrs: Vec::new(),
                collected_items: Vec::new(),
                items: module.items,
            };
            let (header, src) = self.serialize_file(&file)?;
            out.push((format!("{rel_path}.h"), header));
            out.push((format!("{rel_path}.c"), src));
        }
        Ok(out)
    }
}

pub struct CBackend {
    serializer: CSourceSerializer,
    config: fp_core::backend::BackendConfig,
}

impl CBackend {
    pub fn new(config: fp_core::backend::BackendConfig) -> Self {
        Self {
            serializer: CSourceSerializer::new(),
            config,
        }
    }
}

impl fp_core::backend::TargetBackend for CBackend {
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        fp_core::capabilities::LanguageCapabilities::NATIVE
    }

    fn emit_package_artifact(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
        package_id: &fp_core::ast::package::PackageId,
    ) -> Result<()> {
        let package = workspace.package_source(package_id)?;
        let package = &package;
        let files = self.serializer.serialize_package(package)?;
        let writer =
            fp_core::backend::PackageWriter::new(self.config.workspace_root.join(&package.name));
        for (rel_path, code) in files {
            // Every entry already carries its own `.h`/`.c` suffix (added in
            // `serialize_package`), unlike Go's single-extension-per-module case.
            writer.write_file(&rel_path, code)?;
        }
        Ok(())
    }
}

fn module_header_name(file: &File) -> String {
    file.path
        .file_stem()
        .map(|s| s.to_string_lossy().to_string())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "module".to_string())
}

/// Walks a `File`'s items and renders a header (`.h`) and source (`.c`) body
/// in tandem. Header carries type declarations + function prototypes;
/// source carries `#include "<name>.h"` + function bodies.
struct CEmitter {
    header_name: String,
    header_body: StyledWriter,
    source_body: StyledWriter,
    needs_stdint: bool,
    needs_stdbool: bool,
    needs_stdio: bool,
    /// Synthesized anonymous-tuple typedefs, keyed by synthesized name,
    /// deduped by shape (same field types -> same name -> emitted once).
    tuple_typedefs: BTreeMap<String, String>,
    /// Enum names lowered as plain C `enum` (all-unit variants) — `match`
    /// against these lowers to `switch` directly on the scrutinee's value.
    plain_enums: BTreeSet<String>,
    /// Enum names lowered as a tagged union — `match` against these lowers
    /// to `switch` on `.tag`.
    tagged_enums: BTreeSet<String>,
}

impl CEmitter {
    fn new(header_name: String) -> Self {
        let header_cfg = WriterConfig {
            brace_style: BraceStyle::NextLine,
            ..WriterConfig::default()
        };
        let source_cfg = WriterConfig {
            brace_style: BraceStyle::NextLine,
            ..WriterConfig::default()
        };
        Self {
            header_name,
            header_body: StyledWriter::new(header_cfg),
            source_body: StyledWriter::new(source_cfg),
            needs_stdint: false,
            needs_stdbool: false,
            needs_stdio: false,
            tuple_typedefs: BTreeMap::new(),
            plain_enums: BTreeSet::new(),
            tagged_enums: BTreeSet::new(),
        }
    }

    fn finish(self) -> (String, String) {
        let guard = format!("{}_H", self.header_name.to_uppercase().replace('-', "_"));
        let mut header = String::new();
        header.push_str(&format!("#ifndef {guard}\n#define {guard}\n\n"));
        if self.needs_stdint {
            header.push_str("#include <stdint.h>\n");
        }
        if self.needs_stdbool {
            header.push_str("#include <stdbool.h>\n");
        }
        if self.needs_stdint || self.needs_stdbool {
            header.push('\n');
        }
        for typedef in self.tuple_typedefs.values() {
            header.push_str(typedef);
            header.push('\n');
        }
        header.push_str(&self.header_body.finish());
        header.push_str(&format!("\n#endif /* {guard} */\n"));

        let mut source = String::new();
        source.push_str(&format!("#include \"{}.h\"\n", self.header_name));
        if self.needs_stdio {
            source.push_str("#include <stdio.h>\n");
        }
        source.push('\n');
        source.push_str(&self.source_body.finish());

        (header, source)
    }

    fn emit_file(&mut self, file: &File) -> Result<()> {
        for item in &file.items {
            self.emit_item(item)?;
        }
        Ok(())
    }

    fn emit_item(&mut self, item: &Item) -> Result<()> {
        match item.kind() {
            ItemKind::DefStruct(def) => self.emit_struct(def),
            ItemKind::DefEnum(def) => self.emit_enum(def),
            ItemKind::DefFunction(def) => self.emit_function(def),
            ItemKind::Module(module) => {
                for child in &module.items {
                    self.emit_item(child)?;
                }
                Ok(())
            }
            ItemKind::Import(_) => Ok(()),
            _ => {
                self.header_body
                    .write_line(format!("/* unsupported item in C output: {:?} */", item.kind()));
                Ok(())
            }
        }
    }

    // ---- structs ----------------------------------------------------

    fn emit_struct(&mut self, def: &ItemDefStruct) -> Result<()> {
        let name = &def.name.name;
        self.header_body.ensure_blank_line();
        self.header_body.write_line("typedef struct");
        self.header_body.write_line("{");
        self.header_body.increase_indent();
        if def.value.fields.is_empty() {
            self.header_body.write_line("char _unused; /* empty struct */");
        } else {
            for field in &def.value.fields {
                let decl = self.render_declarator(&field.name.name, &field.value);
                self.header_body.write_line(format!("{decl};"));
            }
        }
        self.header_body.decrease_indent();
        self.header_body.write_line(format!("}} {name};"));
        Ok(())
    }

    // ---- enums --------------------------------------------------------

    fn emit_enum(&mut self, def: &ItemDefEnum) -> Result<()> {
        let name = &def.name.name;
        let all_unit = def.value.variants.iter().all(|v| matches!(v.value, Ty::Unit(_)));
        self.header_body.ensure_blank_line();
        if all_unit {
            self.plain_enums.insert(name.clone());
            self.header_body.write_line("typedef enum");
            self.header_body.write_line("{");
            self.header_body.increase_indent();
            self.write_variant_tags(&def.value, name);
            self.header_body.decrease_indent();
            self.header_body.write_line(format!("}} {name};"));
        } else {
            self.tagged_enums.insert(name.clone());
            self.header_body.write_line("typedef struct");
            self.header_body.write_line("{");
            self.header_body.increase_indent();
            self.header_body.write_line("enum");
            self.header_body.write_line("{");
            self.header_body.increase_indent();
            self.write_variant_tags(&def.value, name);
            self.header_body.decrease_indent();
            self.header_body.write_line("} tag;");
            self.header_body.write_line("union");
            self.header_body.write_line("{");
            self.header_body.increase_indent();
            for variant in &def.value.variants {
                if matches!(variant.value, Ty::Unit(_)) {
                    continue;
                }
                let decl = self.render_declarator(&variant.name.name, &variant.value);
                self.header_body.write_line(format!("{decl};"));
            }
            self.header_body.decrease_indent();
            self.header_body.write_line("} data;");
            self.header_body.decrease_indent();
            self.header_body.write_line(format!("}} {name};"));
        }
        Ok(())
    }

    /// Writes `Name_Variant[ = discriminant],` lines for every variant,
    /// prefixed to avoid colliding with other enums' variant constants in
    /// C's flat enum-constant namespace.
    fn write_variant_tags(&mut self, ty: &TypeEnum, enum_name: &str) {
        let count = ty.variants.len();
        for (idx, variant) in ty.variants.iter().enumerate() {
            let tag = format!("{enum_name}_{}", variant.name.name);
            let mut line = tag;
            if let Some(discriminant) = &variant.discriminant {
                if let Some(rendered) = self.render_expr_infallible(discriminant) {
                    line = format!("{line} = {rendered}");
                }
            }
            if idx + 1 != count {
                line.push(',');
            }
            self.header_body.write_line(line);
        }
        if ty.variants.is_empty() {
            self.header_body.write_line("_UNUSED");
        }
    }

    fn variant_tag(&self, enum_name: &str, variant_name: &str) -> String {
        format!("{enum_name}_{variant_name}")
    }

    // ---- functions ------------------------------------------------------

    fn render_signature(&mut self, def: &ItemDefFunction) -> String {
        let sig: &FunctionSignature = &def.sig;
        let params = if sig.params.is_empty() {
            "void".to_string()
        } else {
            sig.params
                .iter()
                .map(|param| self.render_declarator(&param.name.name, &param.ty))
                .collect::<Vec<_>>()
                .join(", ")
        };
        let ret = sig
            .ret_ty
            .as_ref()
            .map(|ty| self.render_type(ty))
            .unwrap_or_else(|| "void".to_string());
        format!("{ret} {}({params})", def.name.name)
    }

    fn emit_function(&mut self, def: &ItemDefFunction) -> Result<()> {
        let signature = self.render_signature(def);
        self.header_body.ensure_blank_line();
        self.header_body.write_line(format!("{signature};"));

        self.source_body.ensure_blank_line();
        let header = signature.clone();
        let body = &def.body;
        let source = self.source_body.clone();
        source.block(header, |w| -> Result<()> { self.emit_block(w, body) })?;
        Ok(())
    }

    // ---- statements -------------------------------------------------------

    fn emit_block(&mut self, w: &StyledWriter, block: &ExprBlock) -> Result<()> {
        for stmt in &block.stmts {
            self.emit_stmt(w, stmt)?;
        }
        Ok(())
    }

    fn emit_stmt(&mut self, w: &StyledWriter, stmt: &BlockStmt) -> Result<()> {
        match stmt {
            BlockStmt::Expr(expr) => self.emit_stmt_expr(w, expr),
            BlockStmt::Let(stmt) => {
                let (name, declared_ty) = pattern_ident_and_type(&stmt.pat);
                let decl_ty = declared_ty
                    .map(|ty| self.render_type(ty))
                    .unwrap_or_else(|| self.render_type(&infer_placeholder_ty()));
                match stmt.init.as_ref().and_then(|e| self.render_expr_infallible(e)) {
                    Some(value) => w.write_line(format!("{decl_ty} {name} = {value};")),
                    None => w.write_line(format!("{decl_ty} {name};")),
                };
                Ok(())
            }
            BlockStmt::Item(item) => self.emit_item(item),
            BlockStmt::Defer(_) => {
                w.write_line("/* unsupported: defer statement */");
                Ok(())
            }
            BlockStmt::Noop => Ok(()),
        }
    }

    fn emit_stmt_expr(&mut self, w: &StyledWriter, expr: &BlockStmtExpr) -> Result<()> {
        self.emit_expr_stmt(w, expr.expr.as_ref())
    }

    /// Emits `expr` as a full statement — unlike `render_expr` (which
    /// produces an inline expression string), this handles the
    /// control-flow shapes (`if`/`while`/`loop`/`for`/`match`/`return`)
    /// that don't have a single-expression C equivalent.
    fn emit_expr_stmt(&mut self, w: &StyledWriter, expr: &Expr) -> Result<()> {
        match expr.kind() {
            ExprKind::Return(ret) => {
                if let Some(value) = ret.value.as_ref() {
                    if let Some(rendered) = self.render_expr_infallible(value.as_ref()) {
                        w.write_line(format!("return {rendered};"));
                    } else {
                        w.write_line("return;");
                    }
                } else {
                    w.write_line("return;");
                }
                Ok(())
            }
            ExprKind::Break(_) => {
                w.write_line("break;");
                Ok(())
            }
            ExprKind::Continue(_) => {
                w.write_line("continue;");
                Ok(())
            }
            ExprKind::If(if_expr) => self.emit_if(w, if_expr),
            ExprKind::While(while_expr) => self.emit_while(w, while_expr),
            ExprKind::Loop(loop_expr) => self.emit_loop(w, loop_expr),
            ExprKind::Block(block) => w.block("", |w| self.emit_block(w, block)),
            ExprKind::Match(match_expr) => self.emit_match(w, match_expr),
            ExprKind::Assign(assign) => {
                let target = self
                    .render_expr_infallible(assign.target.as_ref())
                    .unwrap_or_else(|| "/* unsupported: assign target */ 0".to_string());
                let value = self
                    .render_expr_infallible(assign.value.as_ref())
                    .unwrap_or_else(|| "0".to_string());
                w.write_line(format!("{target} = {value};"));
                Ok(())
            }
            ExprKind::Closure(_) => {
                w.write_line("/* unsupported: closure requires manual lowering */");
                Ok(())
            }
            _ => {
                if let Some(rendered) = self.render_expr_infallible(expr) {
                    w.write_line(format!("{rendered};"));
                } else {
                    w.write_line("/* unsupported expression statement */");
                }
                Ok(())
            }
        }
    }

    fn emit_if(&mut self, w: &StyledWriter, if_expr: &ExprIf) -> Result<()> {
        let cond = self
            .render_expr_infallible(if_expr.cond.as_ref())
            .unwrap_or_else(|| "0".to_string());
        w.block(format!("if ({cond})"), |w| self.emit_body(w, if_expr.then.as_ref()))?;
        if let Some(else_branch) = &if_expr.elze {
            // Chained `else if` reads as one `else` block wrapping a nested
            // `if` statement rather than a flattened `else if` header —
            // functionally equivalent C, simpler to generate.
            w.write_line("else");
            w.block("", |w| self.emit_body(w, else_branch.as_ref()))?;
        }
        Ok(())
    }

    fn emit_body(&mut self, w: &StyledWriter, body: &Expr) -> Result<()> {
        match body.kind() {
            ExprKind::Block(block) => self.emit_block(w, block),
            _ => self.emit_expr_stmt(w, body),
        }
    }

    fn emit_while(&mut self, w: &StyledWriter, while_expr: &ExprWhile) -> Result<()> {
        let cond = self
            .render_expr_infallible(while_expr.cond.as_ref())
            .unwrap_or_else(|| "1".to_string());
        w.block(format!("while ({cond})"), |w| self.emit_body(w, while_expr.body.as_ref()))
    }

    fn emit_loop(&mut self, w: &StyledWriter, loop_expr: &ExprLoop) -> Result<()> {
        w.block("for (;;)", |w| self.emit_body(w, loop_expr.body.as_ref()))
    }

    /// `match` on a plain/tagged enum lowers to a `switch`; anything else
    /// (integers included, best-effort) falls back to an `if`/`else if`
    /// chain comparing the scrutinee against each arm's pattern.
    fn emit_match(&mut self, w: &StyledWriter, match_expr: &ExprMatch) -> Result<()> {
        let Some(scrutinee) = match_expr.scrutinee.as_ref() else {
            w.write_line("/* unsupported: match without scrutinee */");
            return Ok(());
        };
        let scrutinee_text = self
            .render_expr_infallible(scrutinee.as_ref())
            .unwrap_or_else(|| "0".to_string());

        if let Some(enum_name) = self.match_enum_name(&match_expr.cases) {
            let is_tagged = self.tagged_enums.contains(&enum_name);
            let switch_on = if is_tagged {
                format!("{scrutinee_text}.tag")
            } else {
                scrutinee_text.clone()
            };
            w.block(format!("switch ({switch_on})"), |w| -> Result<()> {
                for case in &match_expr.cases {
                    if let Some(variant_name) = self.case_variant_name(case) {
                        let tag = self.variant_tag(&enum_name, &variant_name);
                        w.write_line(format!("case {tag}:"));
                        w.increase_indent();
                        self.emit_body(w, case.body.as_ref())?;
                        w.write_line("break;");
                        w.decrease_indent();
                    } else {
                        w.write_line("default:");
                        w.increase_indent();
                        self.emit_body(w, case.body.as_ref())?;
                        w.write_line("break;");
                        w.decrease_indent();
                    }
                }
                Ok(())
            })?;
            return Ok(());
        }

        // Best-effort `if`/`else if` fallback for non-enum scrutinees.
        let mut first = true;
        for case in &match_expr.cases {
            let is_wildcard = matches!(
                case.pat.as_deref().map(|p| p.kind()),
                Some(PatternKind::Wildcard(_)) | None
            );
            if is_wildcard {
                if first {
                    w.block("", |w| self.emit_body(w, case.body.as_ref()))?;
                } else {
                    w.write_line("else");
                    w.block("", |w| self.emit_body(w, case.body.as_ref()))?;
                }
                continue;
            }
            let Some(pat_value) = case
                .pat
                .as_deref()
                .and_then(|p| self.render_pattern_equality(&scrutinee_text, p))
            else {
                w.write_line("/* unsupported: match arm with nested destructuring */");
                continue;
            };
            let header = if first {
                format!("if ({pat_value})")
            } else {
                format!("else if ({pat_value})")
            };
            w.block(header, |w| self.emit_body(w, case.body.as_ref()))?;
            first = false;
        }
        Ok(())
    }

    /// If every arm's pattern is a plain variant/wildcard of the same enum
    /// type, returns that enum's name so `emit_match` can lower to a
    /// `switch`. Structural inference only (no type info available here) —
    /// works for the common `Enum::Variant => ...` shape.
    fn match_enum_name(&self, cases: &[ExprMatchCase]) -> Option<String> {
        for case in cases {
            if let Some(name) = case.pat.as_deref().and_then(|p| self.pattern_variant_enum(p)) {
                return Some(name);
            }
        }
        None
    }

    fn pattern_variant_enum(&self, pat: &fp_core::ast::Pattern) -> Option<String> {
        match pat.kind() {
            PatternKind::Variant(variant) => {
                // `Enum::Variant` name expr — take the path's first segment.
                match variant.name.kind() {
                    ExprKind::Name(Name::Path(path)) => path.segments.first().map(|i| i.name.clone()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn case_variant_name(&self, case: &ExprMatchCase) -> Option<String> {
        let pat = case.pat.as_deref()?;
        match pat.kind() {
            PatternKind::Variant(variant) => match variant.name.kind() {
                ExprKind::Name(Name::Path(path)) => path.segments.last().map(|i| i.name.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    /// Best-effort equality check for a non-enum match arm's pattern against
    /// the scrutinee. There's no literal-value `PatternKind` in this AST to
    /// lower directly (bindings/tuple/struct destructuring aren't simple
    /// equality checks), so this always defers to the `if`/`else if`
    /// fallback's comment-placeholder path for now.
    fn render_pattern_equality(&mut self, _scrutinee: &str, _pat: &fp_core::ast::Pattern) -> Option<String> {
        None
    }

    // ---- expressions ------------------------------------------------------

    fn render_expr_infallible(&mut self, expr: &Expr) -> Option<String> {
        self.render_expr(expr).ok().flatten()
    }

    fn render_expr(&mut self, expr: &Expr) -> Result<Option<String>> {
        match expr.kind() {
            ExprKind::Value(value) => Ok(Some(self.render_value(value))),
            ExprKind::IntrinsicCall(call) => self.render_intrinsic_call(call),
            ExprKind::Invoke(invoke) => self.render_invoke(invoke),
            ExprKind::BinOp(binop) => {
                let lhs = self.render_expr_infallible(binop.lhs.as_ref()).unwrap_or_default();
                let rhs = self.render_expr_infallible(binop.rhs.as_ref()).unwrap_or_default();
                Ok(Some(format!("({lhs} {} {rhs})", render_binop(binop.kind))))
            }
            ExprKind::UnOp(unop) => {
                let val = self.render_expr_infallible(unop.val.as_ref()).unwrap_or_default();
                Ok(Some(format!("({}{val})", render_unop(&unop.op))))
            }
            ExprKind::Paren(paren) => {
                let inner = self.render_expr_infallible(paren.expr.as_ref()).unwrap_or_default();
                Ok(Some(format!("({inner})")))
            }
            ExprKind::Select(select) => {
                // `Enum::Variant`/`Enum.Variant` selecting a known enum's
                // variant lowers to the prefixed tag constant, not a C
                // field-access expression (`Enum.Variant` isn't valid C).
                if let ExprKind::Name(name) = select.obj.kind() {
                    let base = match name {
                        Name::Ident(ident) => Some(ident.name.clone()),
                        Name::Path(path) => path.segments.last().map(|i| i.name.clone()),
                        _ => None,
                    };
                    if let Some(base) = base {
                        if self.plain_enums.contains(&base) || self.tagged_enums.contains(&base) {
                            return Ok(Some(self.variant_tag(&base, &select.field.name)));
                        }
                    }
                }
                let obj = self.render_expr_infallible(select.obj.as_ref()).unwrap_or_default();
                Ok(Some(format!("{obj}.{}", select.field.name)))
            }
            ExprKind::Reference(reference) => {
                let inner = self.render_expr_infallible(reference.referee.as_ref()).unwrap_or_default();
                Ok(Some(format!("(&{inner})")))
            }
            ExprKind::Dereference(deref) => {
                let inner = self.render_expr_infallible(deref.referee.as_ref()).unwrap_or_default();
                Ok(Some(format!("(*{inner})")))
            }
            ExprKind::Struct(struct_expr) => Ok(Some(self.render_struct_literal(struct_expr))),
            ExprKind::FormatString(template) => {
                let literal = render_format_template_literal(template);
                Ok(Some(format!("\"{}\"", escape_string(&literal))))
            }
            ExprKind::Name(Name::Ident(ident)) => Ok(Some(ident.name.clone())),
            ExprKind::Name(Name::Path(path)) => {
                // `Enum::Variant` referenced as a plain path (e.g. a call
                // argument) needs the tag-constant lowering too — C has no
                // namespaced enum constants to fall back on.
                if path.segments.len() >= 2 {
                    let base = &path.segments[path.segments.len() - 2];
                    let last = path.segments.last().unwrap();
                    if self.plain_enums.contains(&base.name) || self.tagged_enums.contains(&base.name) {
                        return Ok(Some(self.variant_tag(&base.name, &last.name)));
                    }
                }
                Ok(Some(path.join(".")))
            }
            ExprKind::Closure(_) => {
                Ok(Some("/* unsupported: closure requires manual lowering */ NULL".to_string()))
            }
            _ => Ok(None),
        }
    }

    fn render_intrinsic_call(&mut self, call: &ExprIntrinsicCall) -> Result<Option<String>> {
        match call.kind {
            CallKind::Print => {
                self.needs_stdio = true;
                let args = self.render_call_args(&call.args)?;
                Ok(Some(format!("printf({args})")))
            }
            CallKind::Println => {
                self.needs_stdio = true;
                let args = self.render_call_args(&call.args)?;
                Ok(Some(format!("printf({args} \"\\n\")")))
            }
            _ => Ok(Some("/* unsupported: intrinsic call */ 0".to_string())),
        }
    }

    fn render_invoke(&mut self, invoke: &ExprInvoke) -> Result<Option<String>> {
        let target = match &invoke.target {
            ExprInvokeTarget::Function(name) => match name {
                Name::Ident(ident) => ident.name.clone(),
                Name::Path(path) => path.segments.last().map(|i| i.name.clone()).unwrap_or_default(),
                _ => return Ok(None),
            },
            _ => return Ok(None),
        };
        let args = self.render_call_args(&invoke.args)?;
        Ok(Some(format!("{target}({args})")))
    }

    fn render_call_args(&mut self, args: &[Expr]) -> Result<String> {
        let mut rendered = Vec::new();
        for arg in args {
            rendered.push(self.render_expr_infallible(arg).unwrap_or_else(|| "0".to_string()));
        }
        Ok(rendered.join(", "))
    }

    fn render_struct_literal(&mut self, struct_expr: &ExprStruct) -> String {
        let name = match struct_expr.name.kind() {
            ExprKind::Name(Name::Ident(ident)) => ident.name.clone(),
            ExprKind::Name(Name::Path(path)) => path.segments.last().map(|i| i.name.clone()).unwrap_or_default(),
            _ => "/* unsupported struct name */".to_string(),
        };
        let fields = struct_expr
            .fields
            .iter()
            .map(|field: &ExprField| {
                let value = field
                    .value
                    .as_ref()
                    .and_then(|e| self.render_expr_infallible(e))
                    .unwrap_or_else(|| "0".to_string());
                format!(".{} = {value}", field.name.name)
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("({name}){{ {fields} }}")
    }

    fn render_value(&mut self, value: &Value) -> String {
        match value {
            Value::Int(v) => v.value.to_string(),
            Value::UInt(v) => format!("{}u", v.value),
            Value::BigInt(v) => v.value.to_string(),
            Value::Decimal(v) => v.value.to_string(),
            Value::Bool(v) => {
                self.needs_stdbool = true;
                v.value.to_string()
            }
            Value::Char(v) => format!("'{}'", v.value),
            Value::String(v) => format!("\"{}\"", escape_string(&v.value)),
            Value::Tuple(tuple) => {
                let ty = TypeTuple {
                    types: tuple.values.iter().map(|_| infer_placeholder_ty()).collect(),
                };
                let name = self.register_tuple_typedef(&ty);
                let fields = tuple
                    .values
                    .iter()
                    .enumerate()
                    .map(|(idx, v)| format!("._{idx} = {}", self.render_value(v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({name}){{ {fields} }}")
            }
            Value::Struct(value) => {
                let name = &value.ty.name.name;
                let fields = value
                    .structural
                    .fields
                    .iter()
                    .map(|field| format!(".{} = {}", field.name.name, self.render_value(&field.value)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({name}){{ {fields} }}")
            }
            Value::Unit(_) => "/* unit */ 0".to_string(),
            _ => "/* unsupported value */ 0".to_string(),
        }
    }

    // ---- type rendering -----------------------------------------------

    /// Renders `ty` as a bare type name — usable wherever a single-token
    /// type-then-name declarator suffices (return types, tuple typedef
    /// fields). See `render_declarator` for the array/slice case, which
    /// needs the variable name interleaved into the declarator.
    fn render_type(&mut self, ty: &Ty) -> String {
        match ty {
            Ty::Primitive(primitive) => self.render_primitive_type(primitive),
            Ty::Struct(TypeStruct { name, .. }) => name.name.clone(),
            Ty::Enum(TypeEnum { name, .. }) => name.name.clone(),
            Ty::Tuple(tuple) if tuple.types.is_empty() => "void".to_string(),
            Ty::Tuple(tuple) => self.register_tuple_typedef(tuple),
            Ty::Reference(reference) => format!("{}*", self.render_type(&reference.ty)),
            Ty::RawPtr(ptr) => format!("{}*", self.render_type(&ptr.ty)),
            Ty::Slice(slice) => {
                format!("/* unsupported: dynamic slice */ {}*", self.render_type(&slice.elem))
            }
            Ty::Array(array) => {
                format!("/* array length not representable inline */ {}*", self.render_type(&array.elem))
            }
            Ty::Vec(vec) => {
                format!("/* unsupported: dynamic Vec<{}> requires a runtime */ void*", self.render_type(&vec.ty))
            }
            Ty::Unit(_) => "void".to_string(),
            Ty::Any(_) | Ty::Unknown(_) | Ty::GenericVar(_) | Ty::InferVar(_) => {
                "/* unsupported: generic/dynamic type */ void*".to_string()
            }
            Ty::Value(value) => self.render_value(value.value.as_ref()),
            // Nominal type references (`Point`, `f64`, ...) can still be an
            // unresolved name expression rather than `Ty::Primitive`/`Ty::Struct`
            // post-typecheck — this AST layer doesn't always fully resolve a
            // declared field/param type to its solid `Ty`. Best-effort:
            // recognize primitive spellings textually, else assume the name
            // refers to a struct/enum defined elsewhere in this output.
            Ty::Expr(expr) => match type_name_from_expr(expr) {
                Some(name) => primitive_name_to_c(&name, self).unwrap_or(name),
                None => "/* unsupported type expression */ void*".to_string(),
            },
            _ => "/* unsupported type */ void*".to_string(),
        }
    }

    /// Renders a full `Type name;`-shaped declarator (sans trailing `;`),
    /// handling the array-with-known-literal-length case that `render_type`
    /// alone can't express as a single prefix type.
    fn render_declarator(&mut self, name: &str, ty: &Ty) -> String {
        match ty {
            Ty::Array(array) => match self.render_array_len(array.len.as_ref()) {
                Some(len) => format!("{} {name}[{len}]", self.render_type(&array.elem)),
                None => format!(
                    "/* array length not statically known */ {}* {name}",
                    self.render_type(&array.elem)
                ),
            },
            _ => format!("{} {name}", self.render_type(ty)),
        }
    }

    fn render_array_len(&self, expr: &Expr) -> Option<String> {
        match expr.kind() {
            ExprKind::Value(value) => match value.as_ref() {
                Value::Int(v) => Some(v.value.to_string()),
                Value::UInt(v) => Some(v.value.to_string()),
                _ => None,
            },
            _ => None,
        }
    }

    fn render_primitive_type(&mut self, primitive: &TypePrimitive) -> String {
        match primitive {
            TypePrimitive::Bool => {
                self.needs_stdbool = true;
                "bool".to_string()
            }
            TypePrimitive::Char => "char".to_string(),
            TypePrimitive::String => "const char*".to_string(),
            TypePrimitive::List => "/* unsupported: dynamic list */ void*".to_string(),
            TypePrimitive::Int(int_ty) => {
                self.needs_stdint = true;
                match int_ty {
                    TypeInt::I128 | TypeInt::I64 => "int64_t".to_string(),
                    TypeInt::I32 => "int32_t".to_string(),
                    TypeInt::I16 => "int16_t".to_string(),
                    TypeInt::I8 => "int8_t".to_string(),
                    TypeInt::U128 | TypeInt::U64 => "uint64_t".to_string(),
                    TypeInt::U32 => "uint32_t".to_string(),
                    TypeInt::U16 => "uint16_t".to_string(),
                    TypeInt::U8 => "uint8_t".to_string(),
                    TypeInt::BigInt => "long long".to_string(),
                }
            }
            TypePrimitive::Decimal(decimal) => match decimal {
                DecimalType::F32 => "float".to_string(),
                _ => "double".to_string(),
            },
        }
    }

    /// Registers (deduped by rendered field-type shape) a synthesized
    /// `typedef struct { T0 _0; T1 _1; ... } TupleN_T0_T1;` for an
    /// anonymous tuple type, returning its name. C has no anonymous tuple
    /// type, so this is the simplest correct representation.
    fn register_tuple_typedef(&mut self, tuple: &TypeTuple) -> String {
        let field_types = tuple.types.iter().map(|t| self.render_type(t)).collect::<Vec<_>>();
        let mangled: String = field_types
            .iter()
            .map(|t| sanitize_type_name(t))
            .collect::<Vec<_>>()
            .join("_");
        let name = format!("Tuple{}_{}", tuple.types.len(), mangled);
        if !self.tuple_typedefs.contains_key(&name) {
            let fields = field_types
                .iter()
                .enumerate()
                .map(|(idx, t)| format!("    {t} _{idx};"))
                .collect::<Vec<_>>()
                .join("\n");
            let typedef = format!("typedef struct\n{{\n{fields}\n}} {name};\n");
            self.tuple_typedefs.insert(name.clone(), typedef);
        }
        name
    }
}

fn type_name_from_expr(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Name(Name::Ident(ident)) => Some(ident.name.clone()),
        ExprKind::Name(Name::Path(path)) => path.segments.last().map(|i| i.name.clone()),
        _ => None,
    }
}

/// Maps a textual primitive type name (as it appears when a declared type
/// stayed an unresolved name expression) to its C spelling. Returns `None`
/// for anything not recognized as a primitive, so the caller falls back to
/// treating the name as a user-defined struct/enum reference.
fn primitive_name_to_c(name: &str, emitter: &mut CEmitter) -> Option<String> {
    let ty = match name {
        "i8" => TypePrimitive::Int(TypeInt::I8),
        "i16" => TypePrimitive::Int(TypeInt::I16),
        "i32" => TypePrimitive::Int(TypeInt::I32),
        "i64" | "isize" => TypePrimitive::Int(TypeInt::I64),
        "i128" => TypePrimitive::Int(TypeInt::I128),
        "u8" => TypePrimitive::Int(TypeInt::U8),
        "u16" => TypePrimitive::Int(TypeInt::U16),
        "u32" => TypePrimitive::Int(TypeInt::U32),
        "u64" | "usize" => TypePrimitive::Int(TypeInt::U64),
        "u128" => TypePrimitive::Int(TypeInt::U128),
        "f32" => TypePrimitive::Decimal(DecimalType::F32),
        "f64" => TypePrimitive::Decimal(DecimalType::F64),
        "bool" => TypePrimitive::Bool,
        "char" => TypePrimitive::Char,
        "str" | "String" => TypePrimitive::String,
        _ => return None,
    };
    Some(emitter.render_primitive_type(&ty))
}

fn sanitize_type_name(ty: &str) -> String {
    ty.chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect()
}

fn render_binop(kind: BinOpKind) -> &'static str {
    match kind {
        BinOpKind::Add | BinOpKind::AddTrait => "+",
        BinOpKind::Sub => "-",
        BinOpKind::Mul => "*",
        BinOpKind::Div => "/",
        BinOpKind::Mod => "%",
        BinOpKind::Shl => "<<",
        BinOpKind::Shr => ">>",
        BinOpKind::Gt => ">",
        BinOpKind::Lt => "<",
        BinOpKind::Ge => ">=",
        BinOpKind::Le => "<=",
        BinOpKind::Eq => "==",
        BinOpKind::Ne => "!=",
        BinOpKind::Or => "||",
        BinOpKind::And => "&&",
        BinOpKind::BitOr => "|",
        BinOpKind::BitAnd => "&",
        BinOpKind::BitXor => "^",
    }
}

fn render_unop(op: &UnOpKind) -> &'static str {
    match op {
        UnOpKind::Not => "!",
        UnOpKind::Neg => "-",
        UnOpKind::Deref => "*",
        UnOpKind::Any(_) => "/* unsupported unary op */",
    }
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
            fp_core::ast::FormatTemplatePart::Placeholder(_) => out.push_str("%d"),
        }
    }
    out
}

/// `let` bindings without a resolved declared type fall back to `Ty::Any`
/// (rendered as `void*` with an inline comment by `render_type`) — real type
/// inference isn't available at this textual-emission layer.
fn infer_placeholder_ty() -> Ty {
    Ty::any()
}

/// Unwraps a `let` binding's pattern down to its identifier name and
/// (post-typecheck) declared type — the typer wraps a plain `Ident` pattern
/// in `PatternKind::Type` carrying the resolved `Ty` rather than mutating
/// the identifier pattern itself.
fn pattern_ident_and_type(pat: &fp_core::ast::Pattern) -> (String, Option<&Ty>) {
    match pat.kind() {
        PatternKind::Ident(ident) => (ident.ident.name.clone(), None),
        PatternKind::Type(typed) => {
            let (name, _) = pattern_ident_and_type(&typed.pat);
            (name, Some(&typed.ty))
        }
        _ => ("_".to_string(), None),
    }
}
