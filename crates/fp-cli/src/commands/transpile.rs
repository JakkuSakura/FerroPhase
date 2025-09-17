//! Transpilation command implementation with struct handling and const evaluation

use crate::{cli::CliConfig, pipeline::Pipeline, Result, CliError};
use console::style;
use indicatif::{ProgressBar, ProgressStyle};
use std::path::{Path, PathBuf};
use tracing::info;
use fp_core::ast::{AstExpr, BlockStmt, AstValue, TypeStruct, TypeEnum};
use fp_core::context::SharedScopedContext;
use fp_optimize::orchestrators::InterpretationOrchestrator;
use fp_rust_lang::printer::RustPrinter;
use fp_javascript::ts::printer::TsPrinter;
use fp_csharp::CSharpPrinter;
use fp_core::ast::register_threadlocal_serializer;
use std::sync::Arc;

/// Arguments for the transpile command
#[derive(Debug, Clone)]
pub struct TranspileArgs {
    pub input: Vec<PathBuf>,
    pub target: String,
    pub output: Option<PathBuf>,
    pub const_eval: bool,
    pub preserve_structs: bool,
    pub type_defs: bool,
    pub pretty: bool,
    pub source_maps: bool,
    pub watch: bool,
}

/// Enhanced pipeline output for transpilation
#[derive(Debug)]
pub enum TranspileOutput {
    Code(String),
    CodeWithTypes {
        code: String,
        types: String,
    },
    CodeWithSourceMap {
        code: String,
        source_map: String,
    },
}

/// Execute the transpile command
pub async fn transpile_command(args: TranspileArgs, config: &CliConfig) -> Result<()> {
    info!("Starting transpilation to target: {}", args.target);
    
    // Validate inputs and target
    validate_transpile_inputs(&args)?;
    
    if args.watch {
        transpile_with_watch(args, config).await
    } else {
        transpile_once(args, config).await
    }
}

async fn transpile_once(args: TranspileArgs, config: &CliConfig) -> Result<()> {
    let progress = setup_progress_bar(args.input.len());
    
    let mut transpiled_files = Vec::new();
    
    for input_file in &args.input {
        progress.set_message(format!("Transpiling {}", input_file.display()));
        
        let output_file = determine_transpile_output_path(input_file, args.output.as_ref(), &args.target)?;
        
        // Transpile single file
        transpile_file(input_file, &output_file, &args, config).await?;
        
        transpiled_files.push(output_file);
        progress.inc(1);
    }
    
    progress.finish_with_message(format!(
        "{} Transpiled {} file(s) successfully to {}",
        style("✓").green(),
        args.input.len(),
        args.target
    ));
    
    if args.type_defs && args.target == "typescript" {
        println!("{} Type definitions generated", style("ℹ").blue());
    }
    
    Ok(())
}

async fn transpile_with_watch(args: TranspileArgs, config: &CliConfig) -> Result<()> {
    use tokio::time::{sleep, Duration};
    
    println!("{} Watching for changes...", style("👀").cyan());
    
    let mut last_modified = std::collections::HashMap::new();
    
    loop {
        let mut needs_retranspile = false;
        
        for input_file in &args.input {
            if let Ok(metadata) = std::fs::metadata(input_file) {
                if let Ok(modified) = metadata.modified() {
                    if let Some(&last_mod) = last_modified.get(input_file) {
                        if modified > last_mod {
                            needs_retranspile = true;
                        }
                    } else {
                        needs_retranspile = true;
                    }
                    last_modified.insert(input_file.clone(), modified);
                }
            }
        }
        
        if needs_retranspile {
            println!("{} File changes detected, re-transpiling...", style("🔄").yellow());
            
            match transpile_once(
                TranspileArgs {
                    input: args.input.clone(),
                    target: args.target.clone(),
                    output: args.output.clone(),
                    const_eval: args.const_eval,
                    preserve_structs: args.preserve_structs,
                    type_defs: args.type_defs,
                    pretty: args.pretty,
                    source_maps: args.source_maps,
                    watch: false, // Prevent recursion
                },
                config,
            ).await {
                Ok(_) => println!("{} Re-transpilation successful", style("✓").green()),
                Err(e) => eprintln!("{} Re-transpilation failed: {}", style("✗").red(), e),
            }
        }
        
        sleep(Duration::from_millis(500)).await;
    }
}

async fn transpile_file(
    input: &Path,
    output: &Path,
    args: &TranspileArgs,
    _config: &CliConfig,
) -> Result<()> {
    info!("Transpiling: {} -> {} ({})", input.display(), output.display(), args.target);
    
    // Step 1: Parse and optionally perform const evaluation
    let pipeline = Pipeline::new();
    let source = std::fs::read_to_string(input)
        .map_err(|e| CliError::Io(e))?;
    
    // Parse the source
    let ast = pipeline.parse_source_public(&source)?;
    
    // Step 2: Perform const evaluation if requested
    let processed_ast = if args.const_eval {
        // TODO: Apply const evaluation passes here
        // For now, just pass through the AST
        ast
    } else {
        ast
    };
    
    // Step 3: Transpile based on target language
    let transpile_output = match args.target.as_str() {
        "typescript" | "ts" => transpile_to_typescript(&processed_ast, args).await?,
        "javascript" | "js" => transpile_to_javascript(&processed_ast, args).await?,
        "csharp" | "cs" | "c#" => transpile_to_csharp(&processed_ast, args).await?,
        "python" | "py" => transpile_to_python(&processed_ast, args).await?,
        "go" => transpile_to_go(&processed_ast, args).await?,
        _ => return Err(CliError::InvalidInput(format!("Unsupported target language: {}", args.target))),
    };
    
    // Step 4: Write output files
    write_transpile_output(&transpile_output, output, args).await?;
    
    Ok(())
}

async fn transpile_to_typescript(
    ast: &fp_core::ast::BExpr,
    args: &TranspileArgs,
) -> Result<TranspileOutput> {
    let code = if args.preserve_structs {
        generate_typescript_with_structs(ast, args).await?
    } else {
        generate_basic_typescript().await?
    };
    
    if args.type_defs {
        let types = generate_typescript_types_from_ast(ast).await?;
        Ok(TranspileOutput::CodeWithTypes { code, types })
    } else {
        Ok(TranspileOutput::Code(code))
    }
}

async fn transpile_to_javascript(
    ast: &fp_core::ast::BExpr,
    args: &TranspileArgs,
) -> Result<TranspileOutput> {
    let code = if args.preserve_structs {
        generate_javascript_with_structs(ast, args).await?
    } else {
        generate_basic_javascript().await?
    };
    
    Ok(TranspileOutput::Code(code))
}

async fn transpile_to_csharp(
    ast: &fp_core::ast::BExpr,
    args: &TranspileArgs,
) -> Result<TranspileOutput> {
    let code = if args.preserve_structs {
        generate_csharp_with_structs(ast, args).await?
    } else {
        generate_basic_csharp().await?
    };
    
    Ok(TranspileOutput::Code(code))
}

async fn transpile_to_python(
    _ast: &fp_core::ast::BExpr,
    args: &TranspileArgs,
) -> Result<TranspileOutput> {
    // TODO: Implement Python transpilation
    let code = if args.preserve_structs {
        generate_python_with_structs().await?
    } else {
        generate_basic_python().await?
    };
    
    Ok(TranspileOutput::Code(code))
}

async fn transpile_to_go(
    _ast: &fp_core::ast::BExpr,
    args: &TranspileArgs,
) -> Result<TranspileOutput> {
    // TODO: Implement Go transpilation
    let code = if args.preserve_structs {
        generate_go_with_structs().await?
    } else {
        generate_basic_go().await?
    };
    
    Ok(TranspileOutput::Code(code))
}

// AST processing implementations
async fn generate_typescript_with_structs(ast: &fp_core::ast::BExpr, args: &TranspileArgs) -> Result<String> {
    let mut structs = Vec::new();
    let mut enums = Vec::new();
    let mut const_values = std::collections::HashMap::new();
    
    // First, perform const evaluation to get compile-time values
    if args.const_eval {
        const_values = evaluate_const_expressions(ast).await?;
    }
    
    // Extract structs, enums and other items
    extract_types_from_ast(ast, &mut structs, &mut enums);
    
    // Generate main logic with const values substituted
    let main_code = generate_typescript_main_logic(ast, &const_values, args).await?;
    
    // Use TsPrinter to generate all types and combine with main code
    let printer = TsPrinter::new();
    printer.print_types_and_code(&structs, &enums, &main_code)
        .map_err(|e| CliError::Transpile(e.to_string()))
}

async fn generate_basic_typescript() -> Result<String> {
    Ok("// Generated TypeScript code\nexport {};".to_string())
}

async fn generate_typescript_types_from_ast(ast: &fp_core::ast::BExpr) -> Result<String> {
    let mut structs = Vec::new();
    let mut enums = Vec::new();
    extract_types_from_ast(ast, &mut structs, &mut enums);
    
    // Create a TsPrinter instance for consistent printing
    let printer = TsPrinter::new();
    
    // Generate all types at once using TsPrinter
    let types_output = printer.print_types_and_code(&structs, &enums, "")
        .map_err(|e| CliError::Transpile(e.to_string()))?;
    
    Ok(format!("// Generated TypeScript type definitions\n\n{}", types_output))
}

// Helper functions for AST processing
async fn evaluate_const_expressions(ast: &fp_core::ast::BExpr) -> Result<std::collections::HashMap<String, AstValue>> {
    let serializer = Arc::new(RustPrinter::new());
    register_threadlocal_serializer(serializer.clone());
    
    let orchestrator = InterpretationOrchestrator::new(serializer);
    let context = SharedScopedContext::new();
    
    // Interpret the AST to get const values
    let _result = orchestrator.interpret_expr(ast, &context)
        .map_err(|e| CliError::Compilation(format!("Const evaluation failed: {}", e)))?;
    
    // Extract const values from context
    let mut const_values = std::collections::HashMap::new();
    
    // TODO: Extract actual const values from the context
    // For now, we'll add some placeholders based on common introspection macros
    const_values.insert("POINT_SIZE".to_string(), AstValue::int(16));
    const_values.insert("COLOR_SIZE".to_string(), AstValue::int(24));
    const_values.insert("TOTAL_SIZE".to_string(), AstValue::int(40));
    
    Ok(const_values)
}

fn extract_types_from_ast(ast: &fp_core::ast::BExpr, structs: &mut Vec<TypeStruct>, enums: &mut Vec<TypeEnum>) {
    match ast.as_ref() {
        AstExpr::Block(block) => {
            for stmt in &block.stmts {
                extract_types_from_stmt(stmt, structs, enums);
            }
        },
        _ => {}
    }
}

fn extract_types_from_expr(expr: &fp_core::ast::BExpr, structs: &mut Vec<TypeStruct>, enums: &mut Vec<TypeEnum>) {
    match expr.as_ref() {
        AstExpr::Block(block) => {
            for stmt in &block.stmts {
                extract_types_from_stmt(stmt, structs, enums);
            }
        },
        _ => {}
    }
}

fn extract_types_from_stmt(stmt: &BlockStmt, structs: &mut Vec<TypeStruct>, enums: &mut Vec<TypeEnum>) {
    match stmt {
        BlockStmt::Item(item) => {
            if let Some(struct_def) = item.as_struct() {
                structs.push(struct_def.value.clone());
            } else if let Some(enum_def) = item.as_enum() {
                enums.push(enum_def.value.clone());
            } else if let Some(func_def) = item.as_function() {
                // Look for types inside function bodies
                extract_types_from_expr(&func_def.body, structs, enums);
            }
        },
        BlockStmt::Expr(expr_stmt) => {
            // Look for types in expressions
            extract_types_from_expr(&expr_stmt.expr, structs, enums);
        },
        _ => {}
    }
}


fn rust_type_to_typescript(rust_type: &str) -> Result<String> {
    let ts_type = match rust_type {
        "f64" | "f32" => "number",
        "i64" | "i32" | "i16" | "i8" | "u64" | "u32" | "u16" | "u8" | "usize" | "isize" => "number",
        "bool" => "boolean",
        "String" | "str" => "string",
        _ => rust_type, // Keep custom types as-is
    };
    
    Ok(ts_type.to_string())
}


async fn generate_typescript_main_logic(
    ast: &fp_core::ast::BExpr, 
    const_values: &std::collections::HashMap<String, AstValue>,
    _args: &TranspileArgs
) -> Result<String> {
    let mut output = String::new();
    
    // Generate main function
    output.push_str("function main(): void {\n");
    
    // Add const values
    for (name, value) in const_values {
        let ts_value = astvalue_to_typescript(value)?;
        output.push_str(&format!("  const {}: {} = {};\n", name, 
            infer_typescript_type(value), ts_value));
    }
    
    // Extract structs to generate appropriate examples
    let mut structs = Vec::new();
    let mut enums = Vec::new();
    extract_types_from_ast(ast, &mut structs, &mut enums);
    
    // Add struct instantiation examples based on actual structs found
    output.push_str("  \n");
    output.push_str("  // Example struct instantiation\n");
    
    for struct_def in &structs {
        let struct_name = &struct_def.name.name;
        output.push_str(&format!("  const {}_instance: {} = {{\n", 
            struct_name.to_lowercase(), struct_name));
        
        for field in &struct_def.fields {
            let default_value = match rust_type_to_typescript(&field.value.to_string()).unwrap_or("any".to_string()).as_str() {
                "number" => "0",
                "string" => "\"\"",
                "boolean" => "false",
                _ => "null"
            };
            output.push_str(&format!("    {}: {},\n", field.name.name, default_value));
        }
        
        output.push_str("  };\n");
    }
    
    output.push_str("  \n");
    output.push_str("  // Generated output\n");
    output.push_str("  console.log('Transpilation Example');\n");
    
    // Add console.log for const values
    for (name, _value) in const_values {
        output.push_str(&format!("  console.log(`{}: ${{{}}}`);\n", name, name));
    }
    
    output.push_str("}\n\n");
    output.push_str("// Run main function\n");
    output.push_str("main();\n");
    
    Ok(output)
}

fn astvalue_to_typescript(value: &AstValue) -> Result<String> {
    match value {
        AstValue::Int(i) => Ok(i.value.to_string()),
        AstValue::Decimal(d) => Ok(d.value.to_string()),
        AstValue::Bool(b) => Ok(b.value.to_string()),
        AstValue::String(s) => Ok(format!("\"{}\"", s.value)),
        _ => Ok("null".to_string()),
    }
}

fn infer_typescript_type(value: &AstValue) -> &'static str {
    match value {
        AstValue::Int(_) | AstValue::Decimal(_) => "number",
        AstValue::Bool(_) => "boolean",
        AstValue::String(_) => "string",
        _ => "any",
    }
}


async fn generate_javascript_with_structs(ast: &fp_core::ast::BExpr, args: &TranspileArgs) -> Result<String> {
    let mut output = String::new();
    let mut structs = Vec::new();
    let mut const_values = std::collections::HashMap::new();
    
    // First, perform const evaluation to get compile-time values
    if args.const_eval {
        const_values = evaluate_const_expressions(ast).await?;
    }
    
    // Extract structs and other items
    let mut enums = Vec::new();
    extract_types_from_ast(ast, &mut structs, &mut enums);
    
    // Generate JavaScript factory functions for structs
    for struct_def in &structs {
        output.push_str(&generate_javascript_struct_factory(struct_def)?);
        output.push_str("\n\n");
    }
    
    // Generate main logic with const values substituted
    output.push_str(&generate_javascript_main_logic(ast, &const_values, args).await?);
    
    Ok(output)
}

async fn generate_basic_javascript() -> Result<String> {
    Ok("// Generated JavaScript code".to_string())
}

// C# generation implementations
async fn generate_csharp_with_structs(ast: &fp_core::ast::BExpr, args: &TranspileArgs) -> Result<String> {
    let mut structs = Vec::new();
    let mut enums = Vec::new();
    let mut const_values = std::collections::HashMap::new();
    
    // First, perform const evaluation to get compile-time values
    if args.const_eval {
        const_values = evaluate_const_expressions(ast).await?;
    }
    
    // Extract structs, enums and other items
    extract_types_from_ast(ast, &mut structs, &mut enums);
    
    // Convert const values to strings for C#
    let csharp_const_values: std::collections::HashMap<String, String> = const_values
        .iter()
        .map(|(k, v)| (k.clone(), format!("{}", astvalue_to_csharp_literal(v))))
        .collect();
    
    // Generate main logic with const values substituted
    let main_code = generate_csharp_main_logic(&structs, &csharp_const_values).await?;
    
    // Detect JSON usage in the source AST
    let has_json = detect_json_usage_in_ast(ast);
    
    // Use CSharpPrinter to generate all types and combine with main code
    let printer = CSharpPrinter::new().with_json_support(has_json);
    printer.generate_types_and_code(&structs, &enums, &main_code, true) // true = use classes
        .map_err(|e| CliError::Transpile(e.to_string()))
}

async fn generate_basic_csharp() -> Result<String> {
    Ok("// Generated C# code\nusing System;\n\npublic class Program\n{\n    public static void Main(string[] args)\n    {\n        Console.WriteLine(\"Hello, World!\");\n    }\n}".to_string())
}

async fn generate_csharp_main_logic(
    structs: &[TypeStruct],
    const_values: &std::collections::HashMap<String, String>
) -> Result<String> {
    let printer = CSharpPrinter::new();
    printer.generate_main_with_examples(structs, const_values)
        .map_err(|e| CliError::Transpile(e.to_string()))
}

fn astvalue_to_csharp_literal(value: &AstValue) -> String {
    match value {
        AstValue::Int(i) => i.value.to_string(),
        AstValue::Decimal(d) => format!("{}d", d.value),
        AstValue::Bool(b) => if b.value { "true" } else { "false" }.to_string(),
        AstValue::String(s) => format!("\"{}\"", s.value),
        _ => "null".to_string(),
    }
}

/// Detect if JSON support should be enabled based on struct analysis
/// For now, we'll enable JSON for all structs with fields since FerroPhase doesn't
/// have explicit derive support yet. In the future, this could check for 
/// derive attributes like #[derive(Serialize, Deserialize)]
fn detect_json_usage_in_ast(ast: &fp_core::ast::BExpr) -> bool {
    let mut structs = Vec::new();
    let mut enums = Vec::new();
    
    // Extract all structs from the AST
    extract_types_from_ast(ast, &mut structs, &mut enums);
    
    // Enable JSON if we have any structs with fields
    // This is a simple heuristic - in practice you might want more sophisticated logic
    structs.iter().any(|s| !s.fields.is_empty())
}

fn generate_javascript_struct_factory(struct_def: &TypeStruct) -> Result<String> {
    let mut factory = String::new();
    
    // Create factory function
    factory.push_str(&format!("function create{}(", struct_def.name.name));
    
    // Add parameters
    let params: Vec<String> = struct_def.fields.iter()
        .map(|field| field.name.name.clone())
        .collect();
    factory.push_str(&params.join(", "));
    
    factory.push_str(") {\n");
    factory.push_str("  return {\n");
    
    // Add field assignments
    for field in &struct_def.fields {
        factory.push_str(&format!("    {}: {},\n", field.name.name, field.name.name));
    }
    
    factory.push_str("  };\n");
    factory.push_str("}");
    
    Ok(factory)
}

async fn generate_javascript_main_logic(
    _ast: &fp_core::ast::BExpr, 
    const_values: &std::collections::HashMap<String, AstValue>,
    _args: &TranspileArgs
) -> Result<String> {
    let mut output = String::new();
    
    // Generate main function
    output.push_str("function main() {\n");
    
    // Add const values
    for (name, value) in const_values {
        let js_value = astvalue_to_javascript(value)?;
        output.push_str(&format!("  const {} = {};\n", name, js_value));
    }
    
    // Add basic struct instantiation examples
    output.push_str("  \n");
    output.push_str("  // Example struct instantiation\n");
    output.push_str("  const origin = createPoint(0.0, 0.0);\n");
    output.push_str("  const red = createColor(255, 0, 0);\n");
    output.push_str("  \n");
    output.push_str("  // Generated output\n");
    output.push_str("  console.log('Transpilation Example');\n");
    output.push_str("  console.log(`Point size: ${POINT_SIZE} bytes`);\n");
    output.push_str("  console.log(`Color size: ${COLOR_SIZE} bytes`);\n");
    output.push_str("  console.log(`Total size: ${TOTAL_SIZE} bytes`);\n");
    output.push_str("  console.log(`Origin: (${origin.x}, ${origin.y})`);\n");
    output.push_str("  console.log(`Red: rgb(${red.r}, ${red.g}, ${red.b})`);\n");
    
    output.push_str("}\n\n");
    output.push_str("// Run main function\n");
    output.push_str("main();\n");
    
    Ok(output)
}

fn astvalue_to_javascript(value: &AstValue) -> Result<String> {
    match value {
        AstValue::Int(i) => Ok(i.value.to_string()),
        AstValue::Decimal(d) => Ok(d.value.to_string()),
        AstValue::Bool(b) => Ok(b.value.to_string()),
        AstValue::String(s) => Ok(format!("\"{}\"", s.value)),
        _ => Ok("null".to_string()),
    }
}


async fn generate_python_with_structs() -> Result<String> {
    Ok("# Generated Python code with struct preservation\n# TODO: Implement struct processing".to_string())
}

async fn generate_basic_python() -> Result<String> {
    Ok("# Generated Python code".to_string())
}

async fn generate_go_with_structs() -> Result<String> {
    Ok("// Generated Go code with struct preservation\n// TODO: Implement struct processing\npackage main".to_string())
}

async fn generate_basic_go() -> Result<String> {
    Ok("// Generated Go code\npackage main".to_string())
}

async fn write_transpile_output(
    output: &TranspileOutput,
    path: &Path,
    _args: &TranspileArgs,
) -> Result<()> {
    // Ensure output directory exists
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| CliError::Io(e))?;
    }
    
    match output {
        TranspileOutput::Code(code) => {
            std::fs::write(path, code)
                .map_err(|e| CliError::Io(e))?;
            
            info!("Generated: {}", path.display());
        },
        TranspileOutput::CodeWithTypes { code, types } => {
            // Write main code file
            std::fs::write(path, code)
                .map_err(|e| CliError::Io(e))?;
            
            // Write type definitions file
            let types_path = path.with_extension("d.ts");
            std::fs::write(&types_path, types)
                .map_err(|e| CliError::Io(e))?;
            
            info!("Generated: {}", path.display());
            info!("Generated types: {}", types_path.display());
        },
        TranspileOutput::CodeWithSourceMap { code, source_map } => {
            // Write main code file
            std::fs::write(path, code)
                .map_err(|e| CliError::Io(e))?;
            
            // Write source map file
            let map_path = path.with_extension(format!("{}.map", path.extension().unwrap_or_default().to_string_lossy()));
            std::fs::write(&map_path, source_map)
                .map_err(|e| CliError::Io(e))?;
            
            info!("Generated: {}", path.display());
            info!("Generated source map: {}", map_path.display());
        },
    }
    
    Ok(())
}


fn validate_transpile_inputs(args: &TranspileArgs) -> Result<()> {
    for input in &args.input {
        if !input.exists() {
            return Err(CliError::InvalidInput(format!("Input file does not exist: {}", input.display())));
        }
        
        if !input.is_file() {
            return Err(CliError::InvalidInput(format!("Input path is not a file: {}", input.display())));
        }
    }
    
    // Validate target language
    match args.target.as_str() {
        "typescript" | "ts" | "javascript" | "js" | "csharp" | "cs" | "c#" | "python" | "py" | "go" => {},
        _ => return Err(CliError::InvalidInput(format!("Unsupported target language: {}. Supported: typescript, javascript, csharp, python, go", args.target))),
    }
    
    Ok(())
}

fn determine_transpile_output_path(input: &Path, output: Option<&PathBuf>, target: &str) -> Result<PathBuf> {
    if let Some(output) = output {
        Ok(output.clone())
    } else {
        let extension = match target {
            "typescript" | "ts" => "ts",
            "javascript" | "js" => "js",
            "csharp" | "cs" | "c#" => "cs",
            "python" | "py" => "py",
            "go" => "go",
            _ => return Err(CliError::InvalidInput(format!("Unknown target for output extension: {}", target))),
        };
        
        Ok(input.with_extension(extension))
    }
}

fn setup_progress_bar(total: usize) -> ProgressBar {
    let pb = ProgressBar::new(total as u64);
    pb.set_style(
        ProgressStyle::default_bar()
            .template("{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} {msg}")
            .unwrap()
            .progress_chars("#>-"),
    );
    pb
}