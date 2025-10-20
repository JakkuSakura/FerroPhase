use std::fmt::Write as _;

use fp_core::ast::{
    AstSerializer, BlockStmt, Expr, ExprKind, Item, Node, NodeKind, Ty, TypePrimitive, TypeStruct,
};

#[derive(Default)]
struct CSharpContext {
    structs: Vec<TypeStruct>,
}

pub struct CSharpSerializer;

impl AstSerializer for CSharpSerializer {
    fn serialize_node(&self, node: &Node) -> fp_core::error::Result<String> {
        let mut context = CSharpContext::default();
        collect_from_node(node, &mut context);
        Ok(render_csharp(&context))
    }
}

fn collect_from_node(node: &Node, context: &mut CSharpContext) {
    match node.kind() {
        NodeKind::File(file) => {
            for item in &file.items {
                collect_from_item(item, context);
            }
        }
        NodeKind::Item(item) => collect_from_item(item, context),
        NodeKind::Expr(expr) => collect_from_expr(expr, context),
        NodeKind::Query(_) => {}
        NodeKind::Schema(_) => {}
    }
}

fn collect_from_expr(expr: &Expr, context: &mut CSharpContext) {
    if let ExprKind::Block(block) = expr.kind() {
        for stmt in &block.stmts {
            match stmt {
                BlockStmt::Item(item) => collect_from_item(item.as_ref(), context),
                BlockStmt::Expr(inner) => collect_from_expr(inner.expr.as_ref(), context),
                _ => {}
            }
        }
    }
}

fn collect_from_item(item: &Item, context: &mut CSharpContext) {
    if let Some(struct_def) = item.as_struct() {
        context.structs.push(struct_def.value.clone());
    }

    if let Some(expr) = item.as_expr() {
        collect_from_expr(expr, context);
    }
}

fn render_csharp(context: &CSharpContext) -> String {
    let mut output = String::from("using System;\n\n");

    for struct_def in &context.structs {
        let _ = writeln!(output, "public class {} {{", struct_def.name.name);
        for field in &struct_def.fields {
            let ty = csharp_type_from_ty(&field.value);
            let _ = writeln!(
                output,
                "    public {} {} {{ get; set; }}",
                ty, field.name.name
            );
        }
        output.push_str("}\n\n");
    }

    output.push_str("public class Program {\n    public static void Main(string[] args) {\n        Console.WriteLine(\"C# output\");\n    }\n}\n");
    output
}

fn csharp_type_from_ty(ty: &Ty) -> String {
    match ty {
        Ty::Primitive(primitive) => match primitive {
            TypePrimitive::Bool => "bool".into(),
            TypePrimitive::String | TypePrimitive::Char => "string".into(),
            TypePrimitive::Int(int_ty) => match int_ty {
                fp_core::ast::TypeInt::I64 => "long".into(),
                fp_core::ast::TypeInt::I32 => "int".into(),
                fp_core::ast::TypeInt::I16 => "short".into(),
                fp_core::ast::TypeInt::I8 => "sbyte".into(),
                fp_core::ast::TypeInt::U64 => "ulong".into(),
                fp_core::ast::TypeInt::U32 => "uint".into(),
                fp_core::ast::TypeInt::U16 => "ushort".into(),
                fp_core::ast::TypeInt::U8 => "byte".into(),
                _ => "long".into(),
            },
            TypePrimitive::Decimal(decimal_ty) => match decimal_ty {
                fp_core::ast::DecimalType::F64 => "double".into(),
                fp_core::ast::DecimalType::F32 => "float".into(),
                _ => "double".into(),
            },
            TypePrimitive::List => "System.Collections.Generic.List<object>".into(),
        },
        Ty::Struct(struct_ty) => struct_ty.name.name.clone(),
        Ty::Reference(reference) => csharp_type_from_ty(&reference.ty),
        _ => "object".into(),
    }
}
