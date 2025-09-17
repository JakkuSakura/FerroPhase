//! C# code printer and generator

use fp_core::ast::{AstSerializer, AstType, TypeEnum, TypeStruct};
use fp_core::printer::AstSerializerConfig;
use fp_core::{Result, bail};
use itertools::Itertools;

#[derive(Debug, Clone)]
pub struct CSharpPrinter {
    config: AstSerializerConfig,
    enable_json: bool,
}

impl CSharpPrinter {
    pub fn new() -> Self {
        Self {
            config: AstSerializerConfig::standard(), // C# typically uses 4-space indentation
            enable_json: false, // Disabled by default, enabled when detected in source
        }
    }
    
    pub fn with_config(config: AstSerializerConfig) -> Self {
        Self { 
            config,
            enable_json: false,
        }
    }
    
    pub fn with_json_support(mut self, enable_json: bool) -> Self {
        self.enable_json = enable_json;
        self
    }
    
    pub fn config(&self) -> &AstSerializerConfig {
        &self.config
    }
    
    
    /// Generate indentation string for the given level
    fn indent(&self, level: usize) -> String {
        self.config.formatting.indentation.indent_string(level)
    }
    
    /// Map AST types to C# types using proper pattern matching
    fn ast_type_to_csharp(&self, ast_type: &AstType) -> String {
        match ast_type {
            AstType::Vec(type_vec) => {
                let inner_csharp = self.ast_type_to_csharp(&type_vec.ty);
                format!("List<{}>", inner_csharp)
            },
            AstType::Primitive(prim) => {
                match prim {
                    fp_core::ast::TypePrimitive::Decimal(decimal_type) => {
                        match decimal_type {
                            fp_core::ast::DecimalType::F64 => "double".to_string(),
                            fp_core::ast::DecimalType::F32 => "float".to_string(),
                            _ => format!("{:?}", decimal_type),
                        }
                    },
                    fp_core::ast::TypePrimitive::Int(int_type) => {
                        match int_type {
                            fp_core::ast::TypeInt::I64 => "long".to_string(),
                            fp_core::ast::TypeInt::I32 => "int".to_string(),
                            fp_core::ast::TypeInt::I16 => "short".to_string(),
                            fp_core::ast::TypeInt::I8 => "sbyte".to_string(),
                            fp_core::ast::TypeInt::U64 => "ulong".to_string(),
                            fp_core::ast::TypeInt::U32 => "uint".to_string(),
                            fp_core::ast::TypeInt::U16 => "ushort".to_string(),
                            fp_core::ast::TypeInt::U8 => "byte".to_string(),
                            _ => format!("{:?}", int_type),
                        }
                    },
                    fp_core::ast::TypePrimitive::Bool => "bool".to_string(),
                    fp_core::ast::TypePrimitive::String => "string".to_string(),
                    fp_core::ast::TypePrimitive::List => "List<object>".to_string(), // Generic list
                    _ => format!("{:?}", prim), // fallback for other primitives
                }
            },
            AstType::Expr(expr) => {
                // Handle expressions like paths/identifiers  
                let expr_str = format!("{}", expr);
                // Apply simple type mapping for consistency
                self.map_simple_type_to_csharp(&expr_str)
            },
            _ => {
                // Fallback to string representation for other types
                let type_str = format!("{}", ast_type);
                // Apply simple type mapping for consistency
                self.map_simple_type_to_csharp(&type_str)
            }
        }
    }

    /// Handle Vec-like strings that weren't caught by the AST pattern matching
    fn handle_vec_like_string(&self, type_str: &str) -> String {
        // Handle Vec<T> types  
        if type_str.starts_with("Vec<") && type_str.ends_with(">") {
            let inner_type = type_str[4..type_str.len()-1].trim();
            let csharp_inner = self.map_simple_type_to_csharp(inner_type);
            return format!("List<{}>", csharp_inner);
        }
        
        // Handle Vec :: < T > types (with spaces) - this is the problematic format
        if type_str.contains("Vec :: <") && type_str.contains(">") {
            if let Some(start) = type_str.find("Vec :: <") {
                if let Some(end) = type_str.rfind(">") {
                    let inner_type = type_str[start + 8..end].trim();
                    let csharp_inner = self.map_simple_type_to_csharp(inner_type);
                    return format!("List<{}>", csharp_inner);
                }
            }
        }
        
        // No Vec pattern found, return as-is
        type_str.to_string()
    }

    /// Map simple type strings to C# (for fallback cases)
    fn map_simple_type_to_csharp(&self, type_str: &str) -> String {
        // First check for Vec patterns as fallback (should be handled by AST pattern matching primarily)
        if type_str.contains("Vec") {
            return self.handle_vec_like_string(type_str);
        }
        
        match type_str {
            "f64" => "double".to_string(),
            "f32" => "float".to_string(), 
            "i64" => "long".to_string(),
            "i32" => "int".to_string(),
            "i16" => "short".to_string(),
            "i8" => "sbyte".to_string(),
            "u64" => "ulong".to_string(), 
            "u32" => "uint".to_string(),
            "u16" => "ushort".to_string(),
            "u8" => "byte".to_string(),
            "usize" => "nuint".to_string(),
            "isize" => "nint".to_string(),
            "bool" => "bool".to_string(),
            "String" | "str" => "string".to_string(),
            _ => type_str.to_string(), // Keep custom types as-is
        }
    }

    
    /// Generate C# interface from TypeStruct
    pub fn generate_interface(&self, struct_def: &TypeStruct) -> Result<String> {
        let mut output = String::new();
        
        output.push_str(&format!("public interface I{}\n", struct_def.name.name));
        output.push_str("{\n");
        
        for field in &struct_def.fields {
            let csharp_type = self.ast_type_to_csharp(&field.value);
            output.push_str(&format!("{}    {} {} {{ get; set; }}\n", 
                self.indent(1), csharp_type, field.name.name));
        }
        
        output.push_str("}\n");
        
        Ok(output)
    }
    
    /// Generate C# class from TypeStruct
    pub fn generate_class(&self, struct_def: &TypeStruct) -> Result<String> {
        let mut output = String::new();
        
        output.push_str(&format!("public class {}\n", struct_def.name.name));
        output.push_str("{\n");
        
        // Generate properties
        for field in &struct_def.fields {
            let csharp_type = self.ast_type_to_csharp(&field.value);
            
            // Add JSON property name attribute if JSON support is enabled
            if self.enable_json {
                output.push_str(&format!("{}[JsonPropertyName(\"{}\")]\n", 
                    self.indent(1), field.name.name));
            }
            
            output.push_str(&format!("{}public {} {} {{ get; set; }}\n", 
                self.indent(1), csharp_type, field.name.name));
        }
        
        if !struct_def.fields.is_empty() {
            output.push('\n');
        }
        
        // Generate constructor
        output.push_str(&format!("{}public {}()\n", self.indent(1), struct_def.name.name));
        output.push_str(&format!("{}{{\n", self.indent(1)));
        output.push_str(&format!("{}}}\n", self.indent(1)));
        
        // Generate parameterized constructor
        if !struct_def.fields.is_empty() {
            output.push('\n');
            
            let params = struct_def.fields.iter()
                .map(|field| {
                    let csharp_type = self.ast_type_to_csharp(&field.value);
                    format!("{} {}", csharp_type, field.name.name.to_lowercase())
                })
                .join(", ");
                
            output.push_str(&format!("{}public {}({})\n", 
                self.indent(1), struct_def.name.name, params));
            output.push_str(&format!("{}{{\n", self.indent(1)));
            
            for field in &struct_def.fields {
                output.push_str(&format!("{}    this.{} = {};\n", 
                    self.indent(1), field.name.name, field.name.name.to_lowercase()));
            }
            
            output.push_str(&format!("{}}}\n", self.indent(1)));
        }
        
        // Add JSON serialization methods if JSON support is enabled
        if self.enable_json {
            output.push('\n');
            output.push_str(&format!("{}// JSON Serialization Methods\n", self.indent(1)));
            output.push_str(&format!("{}public string ToJson()\n", self.indent(1)));
            output.push_str(&format!("{}{{\n", self.indent(1)));
            output.push_str(&format!("{}    return JsonSerializer.Serialize(this);\n", self.indent(1)));
            output.push_str(&format!("{}}}\n", self.indent(1)));
            
            output.push('\n');
            output.push_str(&format!("{}public static {}? FromJson(string json)\n", 
                self.indent(1), struct_def.name.name));
            output.push_str(&format!("{}{{\n", self.indent(1)));
            output.push_str(&format!("{}    return JsonSerializer.Deserialize<{}>(json);\n", 
                self.indent(1), struct_def.name.name));
            output.push_str(&format!("{}}}\n", self.indent(1)));
        }
        
        output.push_str("}\n");
        
        Ok(output)
    }
    
    /// Generate C# enum from TypeEnum
    pub fn generate_enum(&self, enum_def: &TypeEnum) -> Result<String> {
        let mut output = String::new();
        
        output.push_str(&format!("public enum {}\n", enum_def.name.name));
        output.push_str("{\n");
        
        for (i, variant) in enum_def.variants.iter().enumerate() {
            let comma = if i < enum_def.variants.len() - 1 { "," } else { "" };
            output.push_str(&format!("{}{}{}\n", 
                self.indent(1), variant.name.name, comma));
        }
        
        output.push_str("}\n");
        
        Ok(output)
    }
    
    /// Generate multiple types and combine with main code
    pub fn generate_types_and_code(&self, 
        structs: &[TypeStruct], 
        enums: &[TypeEnum], 
        main_code: &str,
        _use_classes: bool
    ) -> Result<String> {
        let mut output = String::new();
        
        // Add using statements
        output.push_str("using System;\n");
        output.push_str("using System.Collections.Generic;\n");
        if self.enable_json {
            output.push_str("using System.Text.Json;\n");
            output.push_str("using System.Text.Json.Serialization;\n");
        }
        if self.config.formatting.blank_lines_between_declarations {
            output.push('\n');
        }
        
        // Generate enums first
        for enum_def in enums {
            output.push_str(&self.generate_enum(enum_def)?);
            if self.config.formatting.blank_lines_between_declarations {
                output.push('\n');
            }
        }
        
        // Generate structs as classes
        for struct_def in structs {
            output.push_str(&self.generate_class(struct_def)?);
            if self.config.formatting.blank_lines_between_declarations {
                output.push('\n');
            }
        }
        
        // Add main code
        output.push_str(main_code);
        
        // Add trailing newline if configured
        if self.config.formatting.trailing_newline && !output.ends_with('\n') {
            output.push('\n');
        }
        
        Ok(output)
    }
    
    /// Generate a default value for a C# type
    fn default_value_for_type(&self, csharp_type: &str) -> String {
        match csharp_type {
            "int" | "long" | "short" | "sbyte" | 
            "uint" | "ulong" | "ushort" | "byte" |
            "nint" | "nuint" => "0".to_string(),
            "float" => "0.0f".to_string(),
            "double" => "0.0".to_string(),
            "bool" => "false".to_string(),
            "string" | "String" => "\"\"".to_string(),
            _ => {
                // Handle List types and other reference types
                if csharp_type.starts_with("List<") {
                    format!("new {}{{}}", csharp_type)
                } else {
                    "null".to_string()
                }
            }
        }
    }
    
    /// Generate main method with example usage
    pub fn generate_main_with_examples(&self, 
        structs: &[TypeStruct], 
        const_values: &std::collections::HashMap<String, String>
    ) -> Result<String> {
        let mut output = String::new();
        
        output.push_str("public class Program\n");
        output.push_str("{\n");
        output.push_str(&format!("{}public static void Main(string[] args)\n", self.indent(1)));
        output.push_str(&format!("{}{{\n", self.indent(1)));
        
        // Add const values
        for (name, value) in const_values {
            output.push_str(&format!("{}const int {} = {};\n", 
                self.indent(2), name, value));
        }
        
        if !const_values.is_empty() && !structs.is_empty() {
            output.push('\n');
        }
        
        // Add struct instantiation examples
        for struct_def in structs {
            output.push_str(&format!("{}// Example {} instantiation\n", 
                self.indent(2), struct_def.name.name));
                
            output.push_str(&format!("{}var {}_instance = new {}()\n", 
                self.indent(2), 
                struct_def.name.name.to_lowercase(), 
                struct_def.name.name));
            output.push_str(&format!("{}{{\n", self.indent(2)));
            
            for field in &struct_def.fields {
                let csharp_type = self.ast_type_to_csharp(&field.value);
                let default_val = self.default_value_for_type(&csharp_type);
                output.push_str(&format!("{}{} = {},\n", 
                    self.indent(3), field.name.name, default_val));
            }
            
            output.push_str(&format!("{}}};\n", self.indent(2)));
            output.push('\n');
        }
        
        // Add output statements
        output.push_str(&format!("{}// Generated output\n", self.indent(2)));
        output.push_str(&format!("{}Console.WriteLine(\"Transpilation Example\");\n", self.indent(2)));
        
        for (name, _) in const_values {
            output.push_str(&format!("{}Console.WriteLine($\"{}: {{{}}}\");\n", 
                self.indent(2), name, name));
        }
        
        output.push_str(&format!("{}}}\n", self.indent(1)));
        output.push_str("}\n");
        
        Ok(output)
    }
}

impl AstSerializer for CSharpPrinter {
    fn serialize_type(&self, node: &AstType) -> Result<String> {
        match node {
            AstType::Enum(enum_def) => self.generate_enum(enum_def),
            AstType::Struct(struct_def) => self.generate_class(struct_def),
            _ => bail!("Type serialization not implemented for: {:?}", node),
        }
    }
}

impl Default for CSharpPrinter {
    fn default() -> Self {
        Self::new()
    }
}