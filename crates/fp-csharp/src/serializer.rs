use std::fmt::Write as _;

use fp_core::ast::{BlockStmt, Expr, ExprKind, File, Item, Ty, TypePrimitive, TypeStruct};

#[derive(Default)]
struct CSharpContext {
    structs: Vec<TypeStruct>,
}

pub struct CSharpSerializer;

impl CSharpSerializer {
    pub fn serialize_file(&self, file: &File) -> fp_core::error::Result<String> {
        let mut context = CSharpContext::default();
        collect_from_file(file, &mut context);
        Ok(render_csharp(&context))
    }

    /// Serializes a package into one C# source file per module.
    /// Returns `Vec<(relative_path, code)>`.
    pub fn serialize_package(
        &self,
        source: &fp_core::ast::package::AstPackage,
    ) -> fp_core::error::Result<Vec<(String, String)>> {
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

pub struct CSharpBackend {
    config: fp_core::backend::BackendConfig,
}

impl CSharpBackend {
    pub fn new(config: fp_core::backend::BackendConfig) -> Self {
        Self { config }
    }
}

impl fp_core::backend::TargetBackend for CSharpBackend {
    fn capabilities(&self) -> fp_core::capabilities::LanguageCapabilities {
        fp_core::capabilities::LanguageCapabilities::NATIVE
    }

    fn emit_package_artifact(
        &self,
        workspace: &fp_core::ast::program::AstProgram,
        package_id: &fp_core::ast::package::PackageId,
    mir: &fp_core::mir::MirModule,
        lir: Option<&fp_core::lir::LirBlob>,
    ) -> fp_core::error::Result<()> {
        let package = workspace.package_source(package_id)?;
        let package = &package;
        let files = CSharpSerializer.serialize_package(package)?;
        let writer = fp_core::backend::PackageWriter::new(self.config.workspace_root.join(&package.name));
        for (rel_path, code) in files {
            let rel = if rel_path.contains('.') {
                rel_path
            } else {
                format!("{rel_path}.cs")
            };
            writer.write_file(&rel, code)?;
        }
        Ok(())
    }
}

fn collect_from_file(file: &File, context: &mut CSharpContext) {
    for item in &file.items {
        collect_from_item(item, context);
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
        Ty::Primitive(prim) => match prim {
            TypePrimitive::Bool => "bool".to_string(),
            TypePrimitive::Char => "char".to_string(),
            TypePrimitive::String => "string".to_string(),
            TypePrimitive::Int(int_ty) => match int_ty {
                fp_core::ast::TypeInt::I8 => "sbyte".to_string(),
                fp_core::ast::TypeInt::I16 => "short".to_string(),
                fp_core::ast::TypeInt::I32 => "int".to_string(),
                fp_core::ast::TypeInt::I64 => "long".to_string(),
                _ => "int".to_string(),
            },
            TypePrimitive::Decimal(_) => "double".to_string(),
            TypePrimitive::List => "List<object>".to_string(),
        },
        _ => "object".to_string(),
    }
}
