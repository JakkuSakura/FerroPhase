//! Zig parser built on top of `tree-sitter-zig`.
//!
//! This module converts a subset of Zig syntax into the FerroPhase AST so the
//! Zig backend can provide symmetrical parsing + printing similar to other
//! language crates.

use eyre::{eyre, Result};
use fp_core::ast::{DecimalType, TypeInt};
use fp_core::ast::{
    EnumTypeVariant, Expr, File, FunctionParam, Ident, Item, ItemDefConst, ItemDefEnum,
    ItemDefFunction, ItemDefStruct, ItemKind, Node, NodeKind, StructuralField, Ty, TypePrimitive,
    TypeVec, Visibility,
};
use tree_sitter::{Node as TsNode, Parser as TsParser};

/// High-level parser that owns a tree-sitter instance for Zig.
pub struct ZigParser {
    parser: TsParser,
}

impl ZigParser {
    /// Create a new parser instance with the Zig grammar loaded.
    pub fn new() -> Result<Self> {
        let mut parser = TsParser::new();
        parser
            .set_language(&tree_sitter_zig::LANGUAGE.into())
            .map_err(|err| eyre!("Failed to load tree-sitter Zig grammar: {err}"))?;
        Ok(Self { parser })
    }

    /// Parse Zig source into a FerroPhase AST node.
    pub fn parse_str(&mut self, source: &str) -> Result<Node> {
        let tree = self
            .parser
            .parse(source, None)
            .ok_or_else(|| eyre!("failed to parse Zig source"))?;

        let root = tree.root_node();
        let file = parse_file(root, source)?;
        Ok(Node::from(NodeKind::File(file)))
    }
}

impl Default for ZigParser {
    fn default() -> Self {
        Self::new().expect("ZigParser::new should succeed")
    }
}

fn parse_file(root: TsNode, source: &str) -> Result<File> {
    let mut cursor = root.walk();
    let mut items = Vec::new();

    for child in root.named_children(&mut cursor) {
        match child.kind() {
            "variable_declaration" => {
                items.push(parse_variable_declaration(child, source)?);
            }
            "function_declaration" => {
                items.push(parse_function(child, source)?);
            }
            _ => {}
        }
    }

    Ok(File {
        path: Default::default(),
        items,
    })
}

fn parse_variable_declaration(node: TsNode, source: &str) -> Result<Item> {
    let mut cursor = node.walk();
    let mut visibility = Visibility::Private;
    let mut name: Option<Ident> = None;
    let mut value_node: Option<TsNode> = None;

    for child in node.children(&mut cursor) {
        match child.kind() {
            "pub" => visibility = Visibility::Public,
            "const" | "var" | "comptime" => {}
            "identifier" if name.is_none() => {
                name = Some(Ident::new(child.utf8_text(source.as_bytes())?));
            }
            "=" | ";" | ":" => {}
            _ => {
                if child.is_named() && value_node.is_none() {
                    value_node = Some(child);
                }
            }
        }
    }

    let name = name.ok_or_else(|| eyre!("variable declaration missing name"))?;
    let value_node = value_node.ok_or_else(|| eyre!("variable declaration missing initializer"))?;

    match value_node.kind() {
        "struct_declaration" => parse_struct_decl(visibility, name, value_node, source),
        "enum_declaration" => parse_enum_decl(visibility, name, value_node, source),
        _ => parse_const_value(visibility, name, value_node, source),
    }
}

fn parse_struct_decl(
    visibility: Visibility,
    name: Ident,
    node: TsNode,
    source: &str,
) -> Result<Item> {
    let mut cursor = node.walk();
    let mut fields = Vec::new();

    for child in node.named_children(&mut cursor) {
        if child.kind() != "container_field" {
            continue;
        }

        let field_name_node = child
            .child_by_field_name("name")
            .ok_or_else(|| eyre!("struct field missing name"))?;
        let ty_node = child
            .child_by_field_name("type")
            .ok_or_else(|| eyre!("struct field missing type"))?;

        let field_name = Ident::new(field_name_node.utf8_text(source.as_bytes())?);
        let ty = parse_type_node(ty_node, source);
        fields.push(StructuralField::new(field_name, ty));
    }

    let mut def = ItemDefStruct::new(name.clone(), fields);
    def.visibility = visibility;
    Ok(Item::new(ItemKind::DefStruct(def)))
}

fn parse_enum_decl(
    visibility: Visibility,
    name: Ident,
    node: TsNode,
    source: &str,
) -> Result<Item> {
    let mut cursor = node.walk();
    let mut variants = Vec::new();

    for child in node.named_children(&mut cursor) {
        if child.kind() != "container_field" {
            continue;
        }
        let Some(name_node) = child.child_by_field_name("name") else {
            continue;
        };
        let variant_name = Ident::new(name_node.utf8_text(source.as_bytes())?);
        variants.push(EnumTypeVariant {
            name: variant_name,
            value: Ty::unit(),
            discriminant: None,
        });
    }

    let def = ItemDefEnum {
        visibility,
        name: name.clone(),
        value: fp_core::ast::TypeEnum { name, variants },
    };
    Ok(Item::new(ItemKind::DefEnum(def)))
}

fn parse_const_value(
    visibility: Visibility,
    name: Ident,
    value_node: TsNode,
    source: &str,
) -> Result<Item> {
    let value_expr = parse_literal_expr(value_node, source)?;
    Ok(Item::new(ItemKind::DefConst(ItemDefConst {
        ty_annotation: None,
        visibility,
        name,
        ty: None,
        value: value_expr.into(),
    })))
}

fn parse_function(node: TsNode, source: &str) -> Result<Item> {
    let mut visibility = Visibility::Private;
    let mut params_node: Option<TsNode> = None;

    {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if child.kind() == "pub" {
                visibility = Visibility::Public;
            }
            if child.kind() == "parameters" {
                params_node = Some(child);
            }
        }
    }

    let name_node = node
        .child_by_field_name("name")
        .ok_or_else(|| eyre!("function missing name"))?;

    let params_node = params_node.ok_or_else(|| eyre!("function missing parameters"))?;
    let return_type_node = node.child_by_field_name("type");

    let name = Ident::new(name_node.utf8_text(source.as_bytes())?);
    let params = parse_parameters(params_node, source)?;
    let ret_ty = return_type_node.map(|ty_node| parse_type_node(ty_node, source));

    let mut function = ItemDefFunction::new_simple(name.clone(), Expr::unit().into());
    function.visibility = visibility;
    function.sig.params = params;
    function.sig.ret_ty = ret_ty;

    Ok(Item::new(ItemKind::DefFunction(function)))
}

fn parse_parameters(node: TsNode, source: &str) -> Result<Vec<FunctionParam>> {
    let mut params = Vec::new();
    let mut cursor = node.walk();

    for child in node.named_children(&mut cursor) {
        if child.kind() != "parameter_decl" {
            continue;
        }
        let name_node = child
            .child_by_field_name("name")
            .ok_or_else(|| eyre!("parameter missing name"))?;
        let ty_node = child
            .child_by_field_name("type")
            .ok_or_else(|| eyre!("parameter missing type"))?;

        let ident = Ident::new(name_node.utf8_text(source.as_bytes())?);
        let ty = parse_type_node(ty_node, source);
        params.push(FunctionParam::new(ident, ty));
    }

    Ok(params)
}

fn parse_type_node(node: TsNode, source: &str) -> Ty {
    match node.kind() {
        "primitive_type" | "builtin_type" => {
            let text = node.utf8_text(source.as_bytes()).unwrap_or_default();
            parse_primitive_type(text)
        }
        "pointer_type" => {
            if let Some(child) = node.child_by_field_name("type") {
                parse_type_node(child, source)
            } else {
                Ty::any()
            }
        }
        "array_type" => {
            if let Some(child) = node.child_by_field_name("type") {
                Ty::Vec(TypeVec {
                    ty: Box::new(parse_type_node(child, source)),
                })
            } else {
                Ty::any()
            }
        }
        "identifier" => Ty::ident(Ident::new(
            node.utf8_text(source.as_bytes()).unwrap_or_default(),
        )),
        _ => Ty::any(),
    }
}

fn parse_primitive_type(text: &str) -> Ty {
    match text {
        "i8" => Ty::Primitive(TypePrimitive::Int(TypeInt::I8)),
        "i16" => Ty::Primitive(TypePrimitive::Int(TypeInt::I16)),
        "i32" => Ty::Primitive(TypePrimitive::Int(TypeInt::I32)),
        "i64" => Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
        "u8" => Ty::Primitive(TypePrimitive::Int(TypeInt::U8)),
        "u16" => Ty::Primitive(TypePrimitive::Int(TypeInt::U16)),
        "u32" => Ty::Primitive(TypePrimitive::Int(TypeInt::U32)),
        "u64" => Ty::Primitive(TypePrimitive::Int(TypeInt::U64)),
        "f32" => Ty::Primitive(TypePrimitive::Decimal(DecimalType::F32)),
        "f64" => Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
        "bool" => Ty::Primitive(TypePrimitive::Bool),
        "comptime_int" => Ty::Primitive(TypePrimitive::Int(TypeInt::I64)),
        "comptime_float" => Ty::Primitive(TypePrimitive::Decimal(DecimalType::F64)),
        "void" => Ty::unit(),
        "anytype" => Ty::any(),
        _ => Ty::ident(Ident::new(text)),
    }
}

fn parse_literal_expr(node: TsNode, source: &str) -> Result<Expr> {
    let text = node.utf8_text(source.as_bytes())?;

    match node.kind() {
        "integer_literal" => {
            let value = text.replace('_', "").parse::<i64>()?;
            Ok(Expr::value(fp_core::ast::Value::int(value)))
        }
        "float_literal" => {
            let value = text.replace('_', "").parse::<f64>()?;
            Ok(Expr::value(fp_core::ast::Value::decimal(value)))
        }
        "bool_literal" => {
            let value = match text {
                "true" => true,
                "false" => false,
                _ => return Err(eyre!("Invalid bool literal: {text}")),
            };
            Ok(Expr::value(fp_core::ast::Value::bool(value)))
        }
        "string_literal" => {
            let trimmed = text.trim_matches('"');
            Ok(Expr::value(fp_core::ast::Value::string(
                trimmed.to_string(),
            )))
        }
        "identifier" => Ok(Expr::ident(Ident::new(text))),
        "null_literal" => Ok(Expr::value(fp_core::ast::Value::null())),
        "undefined_literal" => Ok(Expr::value(fp_core::ast::Value::undefined())),
        kind => Err(eyre!("Unsupported literal expression kind `{kind}`")),
    }
}

#[cfg(test)]
mod tests {
    #[test]
    #[ignore]
    fn dumps_tree_for_reference() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_zig::LANGUAGE.into())
            .expect("load zig grammar");
        let source = r#"
pub const Point = struct {
    x: i32,
    y: i32,
};

const Color = enum { Red, Green };

fn helper(value: i32) i32 {
    return value;
}

pub fn exported() void {}
"#;
        let tree = parser.parse(source, None).expect("parse");
        let root = tree.root_node();
        println!("{}", root.to_sexp());
        let mut cursor = root.walk();
        for (idx, child) in root.children(&mut cursor).enumerate() {
            println!(
                "root child[{idx}]: kind={} named={}",
                child.kind(),
                child.is_named()
            );
            if child.kind() == "variable_declaration" {
                let mut var_cursor = child.walk();
                for (var_idx, var_child) in child.children(&mut var_cursor).enumerate() {
                    println!(
                        "  var child[{var_idx}]: kind={} named={} field={:?}",
                        var_child.kind(),
                        var_child.is_named(),
                        child.field_name_for_child(var_idx as u32)
                    );
                }
            }
            if child.kind() == "function_declaration" {
                let mut fn_cursor = child.walk();
                for (fn_idx, fn_child) in child.children(&mut fn_cursor).enumerate() {
                    println!(
                        "  fn child[{fn_idx}]: kind={} named={} field={:?}",
                        fn_child.kind(),
                        fn_child.is_named(),
                        child.field_name_for_child(fn_idx as u32)
                    );
                }
            }
        }
    }
}
