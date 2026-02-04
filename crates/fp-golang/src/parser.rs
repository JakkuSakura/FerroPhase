//! Go parser built on top of `tree-sitter-go`.
//!
//! This parser maps a small subset of Go syntax into FerroPhase AST so the
//! Go backend can provide basic parsing support.

use eyre::{Result, eyre};
use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprBlock, ExprKind, ExprReturn, File, Ident, Item,
    ItemDefConst, ItemDefFunction, ItemDefStruct, ItemDefType, ItemImport, ItemImportPath,
    ItemImportRename, ItemImportTree, ItemKind, StructuralField, Ty, TypeArray, TypeFunction,
    TypePrimitive, TypeTuple, TypeVec, Value, Visibility,
};
use fp_core::span::Span;
use tree_sitter::{Node as TsNode, Parser as TsParser};
use tracing::warn;

/// High-level parser that owns a tree-sitter instance for Go.
pub struct GoParser {
    parser: TsParser,
}

impl GoParser {
    /// Create a new parser instance with the Go grammar loaded.
    pub fn new() -> Result<Self> {
        let mut parser = TsParser::new();
        parser
            .set_language(&tree_sitter_go::LANGUAGE.into())
            .map_err(|err| eyre!("Failed to load tree-sitter Go grammar: {err}"))?;
        Ok(Self { parser })
    }

    /// Parse Go source into a FerroPhase AST node.
    pub fn parse_str(&mut self, source: &str) -> Result<Node> {
        let tree = self
            .parser
            .parse(source, None)
            .ok_or_else(|| eyre!("failed to parse Go source"))?;

        let root = tree.root_node();
        let file = parse_file(root, source)?;
        Ok(Node::from(NodeKind::File(file)))
    }
}

impl Default for GoParser {
    fn default() -> Self {
        Self::new().expect("GoParser::new should succeed")
    }
}

use fp_core::ast::{Node, NodeKind};

fn parse_file(root: TsNode, source: &str) -> Result<File> {
    let mut cursor = root.walk();
    let mut items = Vec::new();

    for child in root.named_children(&mut cursor) {
        match child.kind() {
            "package_clause" => {
                let _ = parse_package_clause(child, source);
            }
            "import_declaration" => {
                items.extend(parse_import_declaration(child, source)?);
            }
            "type_declaration" => {
                items.extend(parse_type_declaration(child, source)?);
            }
            "const_declaration" => {
                items.extend(parse_const_declaration(child, source, false)?);
            }
            "var_declaration" => {
                items.extend(parse_const_declaration(child, source, true)?);
            }
            "function_declaration" => {
                items.push(parse_function(child, source)?);
            }
            kind => {
                warn!("unsupported go top-level node: {kind:?}");
            }
        }
    }

    Ok(File {
        path: Default::default(),
        items,
    })
}

fn parse_package_clause(node: TsNode, source: &str) -> Option<String> {
    if let Some(name_node) = node.child_by_field_name("name") {
        if let Ok(name) = name_node.utf8_text(source.as_bytes()) {
            return Some(name.to_string());
        }
    }
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() == "identifier" || child.kind() == "package_identifier" {
            if let Ok(name) = child.utf8_text(source.as_bytes()) {
                return Some(name.to_string());
            }
        }
    }
    warn!("package clause missing name");
    None
}

fn parse_import_declaration(node: TsNode, source: &str) -> Result<Vec<Item>> {
    let mut imports = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() != "import_spec" {
            continue;
        }
        if let Some(import) = parse_import_spec(child, source)? {
            imports.push(Item::new(ItemKind::Import(import)));
        }
    }
    Ok(imports)
}

fn parse_import_spec(node: TsNode, source: &str) -> Result<Option<ItemImport>> {
    let path_node = node.child_by_field_name("path");
    let Some(path_node) = path_node else {
        return Ok(None);
    };

    let alias_node = node.child_by_field_name("name");
    let alias = alias_node
        .and_then(|node| node.utf8_text(source.as_bytes()).ok())
        .map(|text| text.to_string());

    let path_text = parse_string_literal(path_node, source)?;
    let tree = build_import_tree(&path_text, alias.as_deref());

    Ok(Some(ItemImport {
        attrs: Vec::new(),
        visibility: Visibility::Private,
        tree,
    }))
}

fn build_import_tree(path_text: &str, alias: Option<&str>) -> ItemImportTree {
    let segments: Vec<_> = path_text
        .split('/')
        .filter(|segment| !segment.is_empty())
        .map(|segment| ItemImportTree::Ident(Ident::new(segment)))
        .collect();

    if segments.is_empty() {
        return ItemImportTree::Ident(Ident::new(path_text));
    }

    let mut segments = segments;
    if let Some(alias) = alias {
        if let Some(last) = segments.pop() {
            let from = match last {
                ItemImportTree::Ident(ident) => ident,
                _ => Ident::new(path_text),
            };
            segments.push(ItemImportTree::Rename(ItemImportRename {
                from,
                to: Ident::new(alias),
            }));
        }
    }

    if segments.len() == 1 {
        return segments
            .pop()
            .unwrap_or_else(|| ItemImportTree::Ident(Ident::new(path_text)));
    }

    ItemImportTree::Path(ItemImportPath { segments })
}

fn parse_type_declaration(node: TsNode, source: &str) -> Result<Vec<Item>> {
    let mut items = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() != "type_spec" {
            continue;
        }
        items.push(parse_type_spec(child, source)?);
    }
    Ok(items)
}

fn parse_type_spec(node: TsNode, source: &str) -> Result<Item> {
    let name_node = node
        .child_by_field_name("name")
        .ok_or_else(|| eyre!("type spec missing name"))?;
    let name = Ident::new(name_node.utf8_text(source.as_bytes())?);
    let visibility = visibility_for_ident(&name);

    let ty_node = node
        .child_by_field_name("type")
        .ok_or_else(|| eyre!("type spec missing type"))?;

    match ty_node.kind() {
        "struct_type" => {
            let fields = parse_struct_fields(ty_node, source)?;
            let mut def = ItemDefStruct::new(name.clone(), fields);
            def.visibility = visibility;
            Ok(Item::new(ItemKind::DefStruct(def)))
        }
        _ => {
            let ty = parse_type_node(ty_node, source)?;
            let def = ItemDefType {
                attrs: Vec::new(),
                visibility,
                name: name.clone(),
                value: ty,
            };
            Ok(Item::new(ItemKind::DefType(def)))
        }
    }
}

fn parse_struct_fields(node: TsNode, source: &str) -> Result<Vec<StructuralField>> {
    let mut fields = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() != "field_declaration_list" {
            continue;
        }
        fields.extend(parse_field_declaration_list(child, source)?);
    }
    Ok(fields)
}

fn parse_field_declaration_list(node: TsNode, source: &str) -> Result<Vec<StructuralField>> {
    let mut fields = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() != "field_declaration" {
            continue;
        }
        fields.extend(parse_field_declaration(child, source)?);
    }
    Ok(fields)
}

fn parse_field_declaration(node: TsNode, source: &str) -> Result<Vec<StructuralField>> {
    let ty_node = node
        .child_by_field_name("type")
        .or_else(|| node.child_by_field_name("type_identifier"))
        .or_else(|| last_named_child(node))
        .ok_or_else(|| eyre!("field declaration missing type"))?;
    let ty = parse_type_node(ty_node, source)?;

    let mut names = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.id() == ty_node.id() {
            continue;
        }
        if child.kind() == "identifier" || child.kind() == "field_identifier" {
            names.push(Ident::new(child.utf8_text(source.as_bytes())?));
        }
    }

    if names.is_empty() {
        warn!("skipping anonymous field declaration");
        return Ok(Vec::new());
    }

    Ok(names
        .into_iter()
        .map(|name| StructuralField::new(name, ty.clone()))
        .collect())
}

fn parse_const_declaration(node: TsNode, source: &str, mutable: bool) -> Result<Vec<Item>> {
    let mut items = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() != "const_spec" && child.kind() != "var_spec" {
            continue;
        }
        items.extend(parse_const_spec(child, source, mutable)?);
    }
    Ok(items)
}

fn parse_const_spec(node: TsNode, source: &str, mutable: bool) -> Result<Vec<Item>> {
    let mut names = Vec::new();
    let mut values = Vec::new();
    let mut ty: Option<Ty> = None;

    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        match child.kind() {
            "identifier" => names.push(Ident::new(child.utf8_text(source.as_bytes())?)),
            "expression_list" => {
                values = parse_expression_list(child, source)?;
            }
            "type_identifier" | "pointer_type" | "slice_type" | "array_type" | "qualified_type" => {
                ty = Some(parse_type_node(child, source)?);
            }
            _ => {}
        }
    }

    let mut items = Vec::new();
    for (idx, name) in names.into_iter().enumerate() {
        let value = values
            .get(idx)
            .cloned()
            .unwrap_or_else(|| Expr::value(Value::unit()));

        items.push(Item::new(ItemKind::DefConst(ItemDefConst {
            attrs: Vec::new(),
            mutable: Some(mutable),
            ty_annotation: None,
            visibility: visibility_for_ident(&name),
            name,
            ty: ty.clone(),
            value: value.into(),
        })));
    }

    Ok(items)
}

fn parse_function(node: TsNode, source: &str) -> Result<Item> {
    let name_node = node
        .child_by_field_name("name")
        .ok_or_else(|| eyre!("function declaration missing name"))?;
    let name = Ident::new(name_node.utf8_text(source.as_bytes())?);
    let visibility = visibility_for_ident(&name);

    let params_node = node.child_by_field_name("parameters");
    let params = params_node
        .map(|node| parse_parameter_list(node, source))
        .transpose()?
        .unwrap_or_default();

    let ret_ty = node
        .child_by_field_name("result")
        .map(|node| parse_result_type(node, source))
        .transpose()?;

    let body = node
        .child_by_field_name("body")
        .or_else(|| node.child_by_field_name("block"))
        .map(|node| parse_block(node, source))
        .transpose()?
        .unwrap_or_else(|| Expr::new(ExprKind::Block(ExprBlock::new())));

    let mut sig = fp_core::ast::FunctionSignature::unit();
    sig.name = Some(name.clone());
    sig.params = params;
    sig.ret_ty = ret_ty.clone();

    let mut func = ItemDefFunction {
        ty_annotation: None,
        attrs: Vec::new(),
        name: name.clone(),
        ty: None,
        sig,
        body: body.into(),
        visibility,
    };

    if let Some(ret_ty) = ret_ty {
        func.ty = Some(TypeFunction {
            params: func
                .sig
                .params
                .iter()
                .map(|param| param.ty.clone())
                .collect(),
            generics_params: Vec::new(),
            ret_ty: Some(Box::new(ret_ty)),
        });
    }

    Ok(Item::new(ItemKind::DefFunction(func)))
}

fn parse_parameter_list(node: TsNode, source: &str) -> Result<Vec<fp_core::ast::FunctionParam>> {
    let mut params = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() != "parameter_declaration" {
            continue;
        }
        params.extend(parse_parameter_declaration(child, source)?);
    }
    Ok(params)
}

fn parse_parameter_declaration(
    node: TsNode,
    source: &str,
) -> Result<Vec<fp_core::ast::FunctionParam>> {
    let ty_node = node
        .child_by_field_name("type")
        .or_else(|| last_named_child(node))
        .ok_or_else(|| eyre!("parameter missing type"))?;
    let ty = parse_type_node(ty_node, source)?;

    let mut names = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.id() == ty_node.id() {
            continue;
        }
        if child.kind() == "identifier" {
            names.push(Ident::new(child.utf8_text(source.as_bytes())?));
        }
    }

    if names.is_empty() {
        names.push(Ident::new("_"));
    }

    Ok(names
        .into_iter()
        .map(|name| fp_core::ast::FunctionParam::new(name, ty.clone()))
        .collect())
}

fn parse_result_type(node: TsNode, source: &str) -> Result<Ty> {
    if node.kind() == "parameter_list" {
        let mut types = Vec::new();
        let mut cursor = node.walk();
        for child in node.named_children(&mut cursor) {
            if child.kind() != "parameter_declaration" {
                continue;
            }
            let ty_node = child
                .child_by_field_name("type")
                .or_else(|| last_named_child(child))
                .ok_or_else(|| eyre!("result parameter missing type"))?;
            types.push(parse_type_node(ty_node, source)?);
        }
        return Ok(Ty::Tuple(TypeTuple { types }));
    }
    parse_type_node(node, source)
}

fn parse_block(node: TsNode, source: &str) -> Result<Expr> {
    let mut block = ExprBlock::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if child.kind() != "return_statement" {
            continue;
        }
        if let Some(expr) = parse_return_statement(child, source)? {
            block.push_stmt(BlockStmt::Expr(BlockStmtExpr::new(expr)));
        }
    }
    Ok(Expr::new(ExprKind::Block(block)))
}

fn parse_return_statement(node: TsNode, source: &str) -> Result<Option<Expr>> {
    let expr_list = node.child_by_field_name("result");
    let expr_list = expr_list.or_else(|| node.child_by_field_name("value"));
    let Some(expr_list) = expr_list else {
        return Ok(Some(Expr::new(ExprKind::Return(ExprReturn {
            span: Span::null(),
            value: None,
        }))));
    };

    let mut exprs = parse_expression_list(expr_list, source)?;
    let value = exprs.pop();
    let expr = Expr::new(ExprKind::Return(ExprReturn {
        span: Span::null(),
        value: value.map(|expr| expr.into()),
    }));
    Ok(Some(expr))
}

fn parse_expression_list(node: TsNode, source: &str) -> Result<Vec<Expr>> {
    let mut exprs = Vec::new();
    let mut cursor = node.walk();
    for child in node.named_children(&mut cursor) {
        if let Some(expr) = parse_expr(child, source)? {
            exprs.push(expr);
        }
    }
    Ok(exprs)
}

fn parse_expr(node: TsNode, source: &str) -> Result<Option<Expr>> {
    match node.kind() {
        "interpreted_string_literal" | "raw_string_literal" => {
            let text = parse_string_literal(node, source)?;
            Ok(Some(Expr::value(Value::string(text))))
        }
        "int_literal" | "float_literal" | "imaginary_literal" => {
            Ok(Some(parse_numeric_literal(node, source)?))
        }
        "true" => Ok(Some(Expr::value(Value::bool(true)))),
        "false" => Ok(Some(Expr::value(Value::bool(false)))),
        "call_expression" => parse_call_expression(node, source),
        _ => {
            warn!("unsupported go expression node: {:?}", node.kind());
            Ok(None)
        }
    }
}

fn parse_call_expression(node: TsNode, source: &str) -> Result<Option<Expr>> {
    let func_node = node
        .child_by_field_name("function")
        .ok_or_else(|| eyre!("call expression missing function"))?;
    let args_node = node.child_by_field_name("arguments");

    let mut args = Vec::new();
    if let Some(args_node) = args_node {
        let mut cursor = args_node.walk();
        for child in args_node.named_children(&mut cursor) {
            if let Some(expr) = parse_expr(child, source)? {
                args.push(expr);
            }
        }
    }

    let func_text = func_node.utf8_text(source.as_bytes())?;
    let locator = fp_core::ast::Name::Ident(Ident::new(func_text));
    let invoke = fp_core::ast::ExprInvoke {
        span: Span::null(),
        target: fp_core::ast::ExprInvokeTarget::Function(locator),
        args: args.clone(),
        kwargs: Vec::new(),
    };

    Ok(Some(Expr::new(ExprKind::Invoke(invoke))))
}

fn parse_numeric_literal(node: TsNode, source: &str) -> Result<Expr> {
    let raw = node.utf8_text(source.as_bytes())?;
    let normalized = raw.replace('_', "");

    if normalized.starts_with("0x") || normalized.starts_with("0X") {
        let value = i64::from_str_radix(normalized.trim_start_matches("0x").trim_start_matches("0X"), 16)
            .map_err(|err| eyre!("invalid hex literal {raw}: {err}"))?;
        return Ok(Expr::value(Value::int(value)));
    }
    if normalized.starts_with("0b") || normalized.starts_with("0B") {
        let value = i64::from_str_radix(normalized.trim_start_matches("0b").trim_start_matches("0B"), 2)
            .map_err(|err| eyre!("invalid binary literal {raw}: {err}"))?;
        return Ok(Expr::value(Value::int(value)));
    }
    if normalized.starts_with("0o") || normalized.starts_with("0O") {
        let value = i64::from_str_radix(normalized.trim_start_matches("0o").trim_start_matches("0O"), 8)
            .map_err(|err| eyre!("invalid octal literal {raw}: {err}"))?;
        return Ok(Expr::value(Value::int(value)));
    }

    if normalized.contains('.') || normalized.contains('e') || normalized.contains('E') {
        let value: f64 = normalized
            .parse()
            .map_err(|err| eyre!("invalid float literal {raw}: {err}"))?;
        return Ok(Expr::value(Value::decimal(value)));
    }

    let value: i64 = normalized
        .parse()
        .map_err(|err| eyre!("invalid int literal {raw}: {err}"))?;
    Ok(Expr::value(Value::int(value)))
}

fn parse_type_node(node: TsNode, source: &str) -> Result<Ty> {
    match node.kind() {
        "identifier" | "type_identifier" => {
            let text = node.utf8_text(source.as_bytes())?;
            Ok(map_builtin_type(text))
        }
        "qualified_type" => {
            let text = node.utf8_text(source.as_bytes())?;
            Ok(Ty::ident(Ident::new(text)))
        }
        "pointer_type" => {
            let inner = node
                .child_by_field_name("type")
                .or_else(|| last_named_child(node))
                .ok_or_else(|| eyre!("pointer type missing inner"))?;
            let ty = parse_type_node(inner, source)?;
            Ok(Ty::reference(ty))
        }
        "slice_type" => {
            let inner = node
                .child_by_field_name("element")
                .or_else(|| node.child_by_field_name("type"))
                .or_else(|| last_named_child(node))
                .ok_or_else(|| eyre!("slice type missing element"))?;
            let ty = parse_type_node(inner, source)?;
            Ok(Ty::Vec(TypeVec { ty: Box::new(ty) }))
        }
        "array_type" => {
            let elem_node = node
                .child_by_field_name("element")
                .or_else(|| last_named_child(node))
                .ok_or_else(|| eyre!("array type missing element"))?;
            let len_node = node.child_by_field_name("length");
            let len_expr = if let Some(len_node) = len_node {
                parse_numeric_literal(len_node, source)?
            } else {
                Expr::value(Value::int(0))
            };
            let elem_ty = parse_type_node(elem_node, source)?;
            Ok(Ty::Array(TypeArray {
                elem: Box::new(elem_ty),
                len: len_expr.into(),
            }))
        }
        "struct_type" => {
            let fields = parse_struct_fields(node, source)?;
            let name = Ident::new("anonymous_struct");
            Ok(Ty::Struct(fp_core::ast::TypeStruct {
                name,
                generics_params: Vec::new(),
                fields,
            }))
        }
        _ => {
            warn!("unsupported go type node: {:?}", node.kind());
            Ok(Ty::unknown())
        }
    }
}

fn map_builtin_type(text: &str) -> Ty {
    match text {
        "bool" => Ty::Primitive(TypePrimitive::Bool),
        "string" => Ty::Primitive(TypePrimitive::String),
        "int" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::I64)),
        "int64" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::I64)),
        "int32" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::I32)),
        "int16" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::I16)),
        "int8" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::I8)),
        "uint" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::U64)),
        "uint64" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::U64)),
        "uint32" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::U32)),
        "uint16" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::U16)),
        "uint8" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::U8)),
        "byte" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::U8)),
        "rune" => Ty::Primitive(TypePrimitive::Int(fp_core::ast::TypeInt::I32)),
        "float32" => Ty::Primitive(TypePrimitive::Decimal(fp_core::ast::DecimalType::F32)),
        "float64" => Ty::Primitive(TypePrimitive::Decimal(fp_core::ast::DecimalType::F64)),
        _ => Ty::ident(Ident::new(text)),
    }
}

fn parse_string_literal(node: TsNode, source: &str) -> Result<String> {
    let raw = node.utf8_text(source.as_bytes())?;
    if raw.starts_with('`') && raw.ends_with('`') && raw.len() >= 2 {
        return Ok(raw[1..raw.len() - 1].to_string());
    }
    if raw.starts_with('"') && raw.ends_with('"') && raw.len() >= 2 {
        let inner = &raw[1..raw.len() - 1];
        return Ok(inner.replace("\\\"", "\"").replace("\\n", "\n"));
    }
    Ok(raw.to_string())
}

fn last_named_child(node: TsNode) -> Option<TsNode> {
    let mut cursor = node.walk();
    node.named_children(&mut cursor).last()
}

fn visibility_for_ident(ident: &Ident) -> Visibility {
    let first = ident.name.chars().next();
    if matches!(first, Some(ch) if ch.is_ascii_uppercase()) {
        Visibility::Public
    } else {
        Visibility::Private
    }
}
