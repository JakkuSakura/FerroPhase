use fp_core::ast::{
    AstSerializer, BlockStmt, Expr, ExprBlock, ExprKind, File, Item, ItemDefFunction, Ty,
    TypePrimitive, Value, ValueFunction,
};
use fp_core::pretty::{PrettyOptions, pretty};

/// Renders a `Ty` as real Rust type syntax — `Option<PathBuf>`, `{ a: A, b: B }`
/// for a structural type, `(A, B)` for a tuple, `()` for unit, etc. — instead
/// of `Ty`'s raw `Debug` output (`Structural(TypeStructural { fields: [..] })`).
///
/// This is *the* place this matters: `Ty::Display` (see
/// `fp-core/src/ast/value/ty.rs`) already delegates to whatever
/// `AstSerializer` is registered thread-locally (`serialize_type`) rather
/// than formatting itself, specifically so callers (this crate's own
/// `RustPackageProvider`/`FerroFrontend::parse_file`, which registers
/// `PrettyAstSerializer` via `register_threadlocal_serializer`) can just
/// call `ty.to_string()` anywhere and get real output — every caller across
/// the codebase benefits from fixing this in one place, not just whichever
/// caller happened to notice the raw-Debug output first.
///
/// `TypePrimitive`'s own `Display` (see the same file) *also* delegates
/// here — so the `Primitive` arm below must render `TypeInt`/`DecimalType`/
/// `Bool`/`Char`/`String`/`List` directly rather than calling
/// `primitive.to_string()`, or it would recurse into this function forever.
fn serialize_type_rust_shaped(ty: &Ty) -> String {
    match ty {
        Ty::Unit(_) => "()".to_string(),
        Ty::Nothing(_) => "!".to_string(),
        Ty::Wildcard(_) | Ty::InferVar(_) => "_".to_string(),
        Ty::Unknown(_) => "unknown".to_string(),
        Ty::Any(_) => "dyn Any".to_string(),
        Ty::ErrorType(_) => "<error>".to_string(),
        Ty::Primitive(primitive) => match primitive {
            TypePrimitive::Int(int) => int.to_string(),
            TypePrimitive::Decimal(decimal) => decimal.to_string(),
            TypePrimitive::Bool => "bool".to_string(),
            TypePrimitive::Char => "char".to_string(),
            TypePrimitive::String => "String".to_string(),
            TypePrimitive::List => "List".to_string(),
        },
        // A single-type reference (`Option<PathBuf>`, `PathBuf`, ...) —
        // `Name`'s own `Display` already renders `ParameterPath` generics
        // (`Ident<Arg1, Arg2>`), recursing back into this function for each
        // generic argument (`ParameterPathSegment::args: Vec<Ty>`).
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Name(name) => name.to_string(),
            ExprKind::Reference(reference) => match reference.referee.kind() {
                ExprKind::Name(name) => format!("&{name}"),
                other => format!("&{}", expr_kind_tag(other)),
            },
            // A type-position expression that isn't a plain name/reference
            // (e.g. an associated-type-bound generic like `Iterator<Item =
            // T>`, parsed as an `Assign`-shaped bound rather than a bare
            // `Name`) — a short tag, not a full recursive `Debug` dump of
            // the whole expression tree (ids/spans/nested `Expr { .. }`
            // noise), which is what this arm did before this fix.
            other => expr_kind_tag(other),
        },
        Ty::Reference(reference) => {
            let mutability = if reference.mutability == Some(true) {
                "mut "
            } else {
                ""
            };
            format!("&{}{}", mutability, serialize_type_rust_shaped(&reference.ty))
        }
        Ty::RawPtr(raw_ptr) => {
            let mutability = if raw_ptr.mutability == Some(true) {
                "mut"
            } else {
                "const"
            };
            format!("*{} {}", mutability, serialize_type_rust_shaped(&raw_ptr.ty))
        }
        Ty::Vec(vec_ty) => format!("Vec<{}>", serialize_type_rust_shaped(&vec_ty.ty)),
        Ty::Slice(slice) => format!("[{}]", serialize_type_rust_shaped(&slice.elem)),
        Ty::Array(array) => format!("[{}; _]", serialize_type_rust_shaped(&array.elem)),
        Ty::Tuple(tuple) => {
            let types: Vec<String> = tuple.types.iter().map(serialize_type_rust_shaped).collect();
            format!("({})", types.join(", "))
        }
        Ty::Structural(structural) => {
            let fields: Vec<String> = structural
                .fields
                .iter()
                .map(|field| {
                    format!(
                        "{}: {}",
                        field.name,
                        serialize_type_rust_shaped(&field.value)
                    )
                })
                .collect();
            format!("{{ {} }}", fields.join(", "))
        }
        Ty::Function(function) => {
            let params: Vec<String> = function
                .params
                .iter()
                .map(serialize_type_rust_shaped)
                .collect();
            match &function.ret_ty {
                Some(ret_ty) => format!(
                    "fn({}) -> {}",
                    params.join(", "),
                    serialize_type_rust_shaped(ret_ty)
                ),
                None => format!("fn({})", params.join(", ")),
            }
        }
        Ty::GenericVar(generic_var) => format!("T{}", generic_var.index),
        // `-> impl IntoResponse`, `-> impl Iterator<Item = T>`, etc. — a
        // real, common return-type shape (esp. in web-handler-style code),
        // not a rare one; each bound is an `Expr` (typically a bare
        // `Name`), rendered the same way `render_expr`'s `Name` arm would.
        Ty::ImplTraits(impl_traits) => format!("impl {}", render_bounds(&impl_traits.bounds)),
        Ty::TypeBounds(bounds) => render_bounds(bounds),
        // Rare in ordinary source-level field/variant-payload position —
        // an inline anonymous struct/enum definition, a meta-type, a
        // const-eval type block, and so on. Falls back to a short marker
        // rather than a full `Debug` dump, which at least doesn't explode
        // into hundreds of characters of nested `Expr { id: .., span: .. }`
        // noise the way the pre-fix behavior did for every other case too.
        other => format!("<{}>", type_variant_name(other)),
    }
}

/// Renders a `TypeBounds`'s trait list as `Trait1 + Trait2` — each bound is
/// an `Expr`, typically a bare `Name` (`IntoResponse`), occasionally a
/// generic instantiation (`Iterator<Item = T>`) via other `ExprKind` shapes;
/// falls back to `expr_kind_tag` for anything stranger rather than a full
/// recursive `Debug` dump.
fn render_bounds(bounds: &fp_core::ast::TypeBounds) -> String {
    bounds
        .bounds
        .iter()
        .map(|bound| match bound.kind() {
            fp_core::ast::ExprKind::Name(name) => name.to_string(),
            other => expr_kind_tag(other),
        })
        .collect::<Vec<_>>()
        .join(" + ")
}

/// A short `<VariantName>` tag for an `ExprKind` this module doesn't render
/// in full — the variant name extracted from `Debug` output, not the whole
/// recursive dump (ids/spans/nested `Expr { .. }` noise).
fn expr_kind_tag(kind: &fp_core::ast::ExprKind) -> String {
    let debug = format!("{kind:?}");
    let variant = debug
        .split(['(', ' ', '{'])
        .next()
        .filter(|s| !s.is_empty())
        .unwrap_or("?");
    format!("<{variant}>")
}

fn type_variant_name(ty: &Ty) -> &'static str {
    match ty {
        Ty::TokenStream(_) => "tokenstream",
        Ty::Struct(_) => "struct",
        Ty::Enum(_) => "enum",
        Ty::Value(_) => "value",
        Ty::Type(_) => "type",
        Ty::RequestedType(_) => "requested",
        Ty::ConstBlock(_) => "const",
        Ty::Quote(_) => "quote",
        Ty::TypeBinaryOp(_) => "binop",
        Ty::AnyBox(_) => "anybox",
        _ => "?",
    }
}

/// Renders a `Value` as real Rust literal syntax — `0`, `"text"`, `true`,
/// `None`, `Some(1)`, `MyStruct { a: 1 }`, `[1, 2]`, `(1, 2)` — instead of
/// `Value`'s raw `Debug` output (`Int(ValueInt { value: 0 })`).
///
/// Same reasoning as `serialize_type_rust_shaped`: `Value::Display` (see
/// `fp-core/src/ast/value/mod.rs`) delegates to whatever `AstSerializer` is
/// registered thread-locally, so fixing this one place fixes every
/// `value.to_string()` caller across the codebase. Most leaf variants
/// (`ValueInt`, `ValueBool`, `ValueChar`, ...) are built with the
/// `plain_value!` macro, which gives them their own real `Display` already
/// (safe to call `.to_string()` on directly, no recursion risk) — the
/// exceptions are the unit-like ones (`ValueUnit`/`ValueNull`/`ValueNone`/
/// `ValueUndefined`), whose `plain_value!`-generated `Display` just prints
/// their Rust type name (`"ValueUnit"`, `"ValueNull"`, ...), not real syntax,
/// so those still need an explicit mapping below rather than a bare
/// `.to_string()`.
fn serialize_value_rust_shaped(value: &Value) -> String {
    match value {
        Value::Int(int) => int.to_string(),
        Value::UInt(uint) => uint.to_string(),
        Value::BigInt(big) => big.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Decimal(d) => d.to_string(),
        Value::BigDecimal(d) => d.to_string(),
        Value::Char(c) => c.to_string(),
        Value::String(s) => format!("{:?}", s.to_string()),
        Value::Unit(_) => "()".to_string(),
        Value::Null(_) | Value::None(_) => "None".to_string(),
        Value::Undefined(_) => "undefined".to_string(),
        Value::Some(some) => format!("Some({})", serialize_value_rust_shaped(&some.value)),
        Value::Option(opt) => match &opt.value {
            Some(inner) => format!("Some({})", serialize_value_rust_shaped(inner)),
            None => "None".to_string(),
        },
        Value::List(list) => format!(
            "[{}]",
            list.values
                .iter()
                .map(serialize_value_rust_shaped)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Value::Tuple(tuple) => format!(
            "({})",
            tuple
                .values
                .iter()
                .map(serialize_value_rust_shaped)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Value::Struct(s) => serialize_value_structural(&s.ty.name.to_string(), &s.structural),
        Value::Structural(structural) => serialize_value_structural("", structural),
        Value::Type(ty) => serialize_type_rust_shaped(ty),
        // Rare in ordinary literal position — functions, quote tokens,
        // binary/unary op tags, and so on. Falls back to a short marker
        // rather than a full `Debug` dump.
        _ => format!("<{}>", value_variant_name(value)),
    }
}

fn serialize_value_structural(name: &str, structural: &fp_core::ast::ValueStructural) -> String {
    let fields: Vec<String> = structural
        .fields
        .iter()
        .map(|field| format!("{}: {}", field.name, serialize_value_rust_shaped(&field.value)))
        .collect();
    format!("{name} {{ {} }}", fields.join(", "))
}

fn value_variant_name(value: &Value) -> &'static str {
    match value {
        Value::Pointer(_) => "pointer",
        Value::Offset(_) => "offset",
        Value::Escaped(_) => "escaped",
        Value::Function(_) => "fn",
        Value::QuoteToken(_) => "quotetoken",
        Value::TokenStream(_) => "tokenstream",
        Value::Expr(_) => "expr",
        Value::BinOpKind(_) => "binop",
        Value::UnOpKind(_) => "unop",
        Value::Map(_) => "map",
        Value::Bytes(_) => "bytes",
        _ => "?",
    }
}

#[derive(Debug, Clone)]
pub struct PrettyAstSerializer {
    options: PrettyOptions,
}

impl PrettyAstSerializer {
    pub fn new() -> Self {
        Self {
            options: PrettyOptions::default(),
        }
    }

    pub fn with_options(options: PrettyOptions) -> Self {
        Self { options }
    }
}

impl Default for PrettyAstSerializer {
    fn default() -> Self {
        Self::new()
    }
}

impl AstSerializer for PrettyAstSerializer {
    fn serialize_expr(&self, node: &Expr) -> Result<String, fp_core::Error> {
        Ok(format!("{}", pretty(node, self.options.clone())))
    }

    fn serialize_item(&self, node: &Item) -> Result<String, fp_core::Error> {
        Ok(format!("{}", pretty(node, self.options.clone())))
    }

    fn serialize_value(&self, node: &Value) -> Result<String, fp_core::Error> {
        Ok(serialize_value_rust_shaped(node))
    }

    fn serialize_type(&self, node: &Ty) -> Result<String, fp_core::Error> {
        Ok(serialize_type_rust_shaped(node))
    }

    fn serialize_block(&self, node: &ExprBlock) -> Result<String, fp_core::Error> {
        let mut out = String::new();
        for (idx, stmt) in node.stmts.iter().enumerate() {
            if idx > 0 {
                out.push('\n');
            }
            out.push_str(&self.serialize_stmt(stmt)?);
        }
        Ok(out)
    }

    fn serialize_stmt(&self, node: &BlockStmt) -> Result<String, fp_core::Error> {
        Ok(format!("{node:?}"))
    }

    fn serialize_value_function(&self, node: &ValueFunction) -> Result<String, fp_core::Error> {
        Ok(format!("{node:?}"))
    }

    fn serialize_def_function(&self, node: &ItemDefFunction) -> Result<String, fp_core::Error> {
        Ok(format!("{node:?}"))
    }
}

impl PrettyAstSerializer {
    pub fn serialize_file(&self, file: &File) -> Result<String, fp_core::Error> {
        Ok(format!("{}", pretty(file, self.options.clone())))
    }

    /// Serializes a package into one pretty-printed Rust-ish source file
    /// per module. Returns `Vec<(relative_path, code)>`.
    pub fn serialize_package(
        &self,
        source: &fp_core::package::PackageSource,
    ) -> Result<Vec<(String, String)>, fp_core::Error> {
        fp_core::package::split_package_into_modules(source)
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

pub struct RustBackend {
    serializer: PrettyAstSerializer,
    config: fp_core::backend::BackendConfig,
}

impl RustBackend {
    pub fn new(config: fp_core::backend::BackendConfig) -> Self {
        Self {
            serializer: PrettyAstSerializer::new(),
            config,
        }
    }
}

impl fp_core::backend::TargetBackend for RustBackend {
    fn compile_package(
        &self,
        workspace: &fp_core::workspace::WorkspaceContext,
        package_id: &fp_core::package::PackageId,
    ) -> Result<(), fp_core::Error> {
        let package = workspace.package_source(package_id)?;
        let package = &package;
        let files = self.serializer.serialize_package(package)?;
        let writer = fp_core::backend::PackageWriter::new(self.config.workspace_root.join(&package.name));
        for (rel_path, code) in files {
            let rel = if rel_path.contains('.') {
                rel_path
            } else {
                format!("{rel_path}.rs")
            };
            writer.write_file(&rel, code)?;
        }
        Ok(())
    }
}
