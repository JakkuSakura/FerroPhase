use fp_core::ast::{BlockStmt, Expr, ExprKind, Item, Node, NodeKind};
use fp_core::error::Result;
use std::collections::HashMap;

/// Simplified transpilation that consolidates all the duplicated language-specific code
pub struct Transpiler {
    target: TranspileTarget,
}

impl Transpiler {
    pub fn new(target: TranspileTarget) -> Self {
        Self { target }
    }

    pub fn transpile(&self, node: &Node) -> Result<String> {
        let template = self.get_template();
        let context = self.extract_context(node)?;
        Ok(template.render(&context))
    }

    fn get_template(&self) -> &dyn CodeTemplate {
        match self.target {
            TranspileTarget::TypeScript => &TypeScriptTemplate,
            TranspileTarget::JavaScript => &JavaScriptTemplate,
            TranspileTarget::CSharp => &CSharpTemplate,
            TranspileTarget::Rust => &RustTemplate,
        }
    }

    fn extract_context(&self, node: &Node) -> Result<TranspileContext> {
        // Single method to extract all needed info, removing duplication
        // across generate_typescript_with_structs, generate_javascript_with_structs, etc.
        let mut context = TranspileContext::new();
        self.visit_node(node, &mut context);
        Ok(context)
    }

    fn visit_node(&self, node: &Node, context: &mut TranspileContext) {
        match node.kind() {
            NodeKind::File(file) => {
                for item in &file.items {
                    self.visit_item(item, context);
                }
            }
            NodeKind::Item(item) => self.visit_item(item, context),
            NodeKind::Expr(expr) => self.visit_expr(expr, context),
        }
    }

    fn visit_expr(&self, expr: &Expr, context: &mut TranspileContext) {
        match expr.kind() {
            ExprKind::Block(block) => {
                for stmt in &block.stmts {
                    self.visit_stmt(stmt, context);
                }
            }
            _ => {}
        }
    }

    fn visit_stmt(&self, stmt: &BlockStmt, context: &mut TranspileContext) {
        // Consolidate all the scattered AST extraction logic
        match stmt {
            BlockStmt::Item(item) => self.visit_item(item.as_ref(), context),
            BlockStmt::Expr(expr) => self.visit_expr(expr.expr.as_ref(), context),
            _ => {}
        }
    }

    fn visit_item(&self, item: &Item, context: &mut TranspileContext) {
        if let Some(struct_def) = item.as_struct() {
            context.structs.push(struct_def.value.clone());
        } else if let Some(enum_def) = item.as_enum() {
            context.enums.push(enum_def.value.clone());
        }

        if let Some(expr) = item.as_expr() {
            self.visit_expr(expr, context);
        }
    }
}

#[derive(Debug)]
pub enum TranspileTarget {
    TypeScript,
    JavaScript,
    CSharp,
    Rust,
}

/// Single context structure instead of scattered HashMap<String, Value> everywhere
#[derive(Debug)]
pub struct TranspileContext {
    pub structs: Vec<fp_core::ast::TypeStruct>,
    pub enums: Vec<fp_core::ast::TypeEnum>,
    pub constants: HashMap<String, fp_core::ast::Value>,
}

impl TranspileContext {
    pub fn new() -> Self {
        Self {
            structs: Vec::new(),
            enums: Vec::new(),
            constants: HashMap::new(),
        }
    }
}

/// Template trait to eliminate the massive duplication in language-specific generation
trait CodeTemplate {
    fn render(&self, context: &TranspileContext) -> String;
    fn type_name<'a>(&self, rust_type: &'a str) -> &'a str;
    #[allow(dead_code)]
    fn default_value(&self, type_name: &str) -> &str;
}

struct TypeScriptTemplate;
impl CodeTemplate for TypeScriptTemplate {
    fn render(&self, context: &TranspileContext) -> String {
        let mut output = String::new();

        // Generate interfaces
        for struct_def in &context.structs {
            output.push_str(&format!("interface {} {{\n", struct_def.name.name));
            for field in &struct_def.fields {
                let field_value_str = field.value.to_string();
                let ts_type = self.type_name(&field_value_str);
                output.push_str(&format!("  {}: {};\n", field.name.name, ts_type));
            }
            output.push_str("}\n\n");
        }

        // Generate main function
        output.push_str("function main(): void {\n");
        output.push_str("  console.log('TypeScript output');\n");
        output.push_str("}\n\nmain();\n");

        output
    }

    fn type_name<'a>(&self, rust_type: &'a str) -> &'a str {
        match rust_type {
            "f64" | "f32" | "i64" | "i32" | "u64" | "u32" => "number",
            "bool" => "boolean",
            "String" => "string",
            _ => "any",
        }
    }

    fn default_value(&self, type_name: &str) -> &str {
        match type_name {
            "number" => "0",
            "boolean" => "false",
            "string" => "\"\"",
            _ => "null",
        }
    }
}

struct JavaScriptTemplate;
impl CodeTemplate for JavaScriptTemplate {
    fn render(&self, context: &TranspileContext) -> String {
        let mut output = String::new();

        // Generate factory functions
        for struct_def in &context.structs {
            output.push_str(&format!("function create{}(", struct_def.name.name));
            let params: Vec<_> = struct_def
                .fields
                .iter()
                .map(|f| f.name.name.as_str())
                .collect();
            output.push_str(&params.join(", "));
            output.push_str(") {\n  return {\n");

            for field in &struct_def.fields {
                output.push_str(&format!("    {}: {},\n", field.name.name, field.name.name));
            }
            output.push_str("  };\n}\n\n");
        }

        output.push_str("function main() {\n");
        output.push_str("  console.log('JavaScript output');\n");
        output.push_str("}\n\nmain();\n");

        output
    }

    fn type_name<'a>(&self, _rust_type: &'a str) -> &'a str {
        "any"
    }
    fn default_value(&self, _type_name: &str) -> &str {
        "null"
    }
}

struct CSharpTemplate;
impl CodeTemplate for CSharpTemplate {
    fn render(&self, context: &TranspileContext) -> String {
        let mut output = String::new();
        output.push_str("using System;\n\n");

        // Generate classes
        for struct_def in &context.structs {
            output.push_str(&format!("public class {} {{\n", struct_def.name.name));
            for field in &struct_def.fields {
                let field_value_str = field.value.to_string();
                let cs_type = self.type_name(&field_value_str);
                output.push_str(&format!(
                    "    public {} {} {{ get; set; }}\n",
                    cs_type, field.name.name
                ));
            }
            output.push_str("}\n\n");
        }

        output.push_str("public class Program {\n");
        output.push_str("    public static void Main(string[] args) {\n");
        output.push_str("        Console.WriteLine(\"C# output\");\n");
        output.push_str("    }\n}\n");

        output
    }

    fn type_name<'a>(&self, rust_type: &'a str) -> &'a str {
        match rust_type {
            "f64" | "f32" => "double",
            "i64" => "long",
            "i32" => "int",
            "u32" => "uint",
            "bool" => "bool",
            "String" => "string",
            _ => "object",
        }
    }

    fn default_value(&self, _type_name: &str) -> &str {
        "default"
    }
}

struct RustTemplate;
impl CodeTemplate for RustTemplate {
    fn render(&self, context: &TranspileContext) -> String {
        let mut output = String::new();

        // Generate structs
        for struct_def in &context.structs {
            output.push_str(&format!(
                "#[derive(Debug, Clone)]\npub struct {} {{\n",
                struct_def.name.name
            ));
            for field in &struct_def.fields {
                output.push_str(&format!("    pub {}: {},\n", field.name.name, field.value));
            }
            output.push_str("}\n\n");
        }

        output.push_str("fn main() {\n");
        output.push_str("    println!(\"Rust output\");\n");
        output.push_str("}\n");

        output
    }

    fn type_name<'a>(&self, rust_type: &'a str) -> &'a str {
        rust_type
    }
    fn default_value(&self, _type_name: &str) -> &str {
        "Default::default()"
    }
}
