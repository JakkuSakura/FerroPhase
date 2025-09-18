mod attr;
mod expr;
mod item;
pub mod macros;
mod pat;
mod ty;

use crate::parser::expr::parse_block;

use crate::parser::item::parse_fn_sig;
use itertools::Itertools;
use fp_core::ast::*;
use fp_core::id::{Ident, Locator, ParameterPath, ParameterPathSegment, Path};
use fp_core::{bail};

use fp_core::error::Result;
use eyre::{eyre, ensure, Context};
use std::path::PathBuf;
use syn::parse_str;
use syn_inline_mod::InlinerBuilder;

pub fn parse_ident(i: syn::Ident) -> Ident {
    Ident::new(i.to_string())
}
pub fn parse_path(p: syn::Path) -> Result<Path> {
    Ok(Path {
        segments: p
            .segments
            .into_iter()
            .map(|x| {
                let ident = parse_ident(x.ident);
                ensure!(
                    x.arguments.is_none(),
                    "Does not support path arguments: {:?}",
                    x.arguments
                );
                Ok(ident)
            })
            .try_collect()?,
    })
}
pub fn parse_parameter_path(p: syn::Path) -> Result<ParameterPath> {
    Ok(ParameterPath {
        segments: p
            .segments
            .into_iter()
            .map(|x| {
                let args = match x.arguments {
                    syn::PathArguments::AngleBracketed(a) => {
                        a.args
                            .into_iter()
                            .map(|x| match x {
                                syn::GenericArgument::Type(t) => ty::parse_type(t),
                                syn::GenericArgument::Const(c) => expr::parse_expr(c)
                                    .map(|x| AstType::value(AstValue::expr(x.get()))),
                                _ => bail!("Does not support path arguments: {:?}", x),
                            })
                            .try_collect()?
                    }
                    _ => bail!("Does not support path arguments: {:?}", x),
                };
                let ident = parse_ident(x.ident);
                Ok::<_, fp_core::Error>(ParameterPathSegment { ident, args })
            })
            .try_collect()?,
    })
}
fn parse_locator(p: syn::Path) -> Result<Locator> {
    if let Ok(path) = parse_path(p.clone()) {
        return Ok(Locator::path(path));
    }
    let path = parse_parameter_path(p.clone())?;
    Ok(Locator::parameter_path(path))
}

fn parse_vis(v: syn::Visibility) -> Visibility {
    match v {
        syn::Visibility::Public(_) => Visibility::Public,
        syn::Visibility::Restricted(_) => Visibility::Public,
        syn::Visibility::Inherited => Visibility::Private,
    }
}
pub fn parse_file(path: PathBuf, file: syn::File) -> Result<AstFile> {
    let items = file.items.into_iter().map(item::parse_item).try_collect()?;
    Ok(AstFile { path, items })
}
pub fn parse_module(m: syn::ItemMod) -> Result<AstModule> {
    Ok(AstModule {
        name: parse_ident(m.ident),
        items: m
            .content
            .unwrap()
            .1
            .into_iter()
            .map(item::parse_item)
            .try_collect()?,
        visibility: parse_vis(m.vis),
    })
}
pub fn parse_value_fn(f: syn::ItemFn) -> Result<ValueFunction> {
    let sig = parse_fn_sig(f.sig)?;
    let body = parse_block(*f.block)?;
    Ok(ValueFunction {
        sig,
        body: AstExpr::block(body).into(),
    })
}
#[derive(Debug, Clone, Eq, PartialEq, Copy)]
pub struct RustParser {}

impl RustParser {
    pub fn new() -> Self {
        RustParser {}
    }
    pub fn parse_file_recursively(&self, path: PathBuf) -> Result<AstFile> {
        let builder = InlinerBuilder::new();
        let path = path
            .canonicalize()
            .with_context(|| format!("Could not find file: {}", path.display()))?;
        tracing::debug!("Parsing {}", path.display());
        let module = builder
            .parse_and_inline_modules(&path)
            .with_context(|| format!("path: {}", path.display()))?;
        let (outputs, errors) = module.into_output_and_errors();
        let mut errors_str = String::new();
        for err in errors {
            errors_str.push_str(&format!("{}\n", err));
        }
        if !errors_str.is_empty() {
            bail!("Errors when parsing {}: {}", path.display(), errors_str);
        }
        let file = self.parse_file_content(path, outputs)?;
        Ok(file)
    }
    pub fn parse_value(&self, code: syn::Expr) -> Result<AstValue> {
        expr::parse_expr(code).map(|x| AstValue::expr(x.get()))
    }
    pub fn parse_expr(&self, code: syn::Expr) -> Result<AstExpr> {
        expr::parse_expr(code).map(|x| x.get())
    }
    pub fn parse_item(&self, code: syn::Item) -> Result<AstItem> {
        item::parse_item(code)
    }
    pub fn parse_items(&self, code: Vec<syn::Item>) -> Result<Vec<AstItem>> {
        code.into_iter().map(|x| self.parse_item(x)).try_collect()
    }
    pub fn parse_file_content(&self, path: PathBuf, code: syn::File) -> Result<AstFile> {
        parse_file(path, code)
    }
    pub fn parse_module(&self, code: syn::ItemMod) -> Result<AstModule> {
        parse_module(code)
    }
    pub fn parse_type(&self, code: syn::Type) -> Result<AstType> {
        ty::parse_type(code)
    }
    
    pub fn try_parse_as_file(&self, source: &str) -> Result<BExpr> {
        // Parse as a syn::File first, but be more permissive with errors
        let syn_file: syn::File = syn::parse_str(source)
            .map_err(|e| eyre!("Failed to parse as file: {}", e))?;

        // Try to parse the file, but handle errors more gracefully for transpilation
        match self.parse_file_content(PathBuf::from("input.fp"), syn_file) {
            Ok(ast_file) => {
                // Find main function and const declarations
                let mut const_items = Vec::new();
                let mut main_body = None;

                for item in ast_file.items {
                    if let Some(func) = item.as_function() {
                        if func.name.name == "main" {
                            main_body = Some(func.body.clone());
                        }
                    } else {
                        // Keep const declarations and other items
                        const_items.push(BlockStmt::Item(Box::new(item)));
                    }
                }

                // If we found a main function, create a block with const items + main body
                if let Some(body) = main_body {
                    // Add the main body as the final expression
                    const_items.push(BlockStmt::Expr(BlockStmtExpr {
                        expr: body,
                        semicolon: None,
                    }));

                    Ok(Box::new(AstExpr::Block(ExprBlock {
                        stmts: const_items,
                    })))
                } else {
                    // No main function, create a minimal structure for transpilation
                    if const_items.is_empty() {
                        // Create an empty block for transpilation purposes
                        Ok(Box::new(AstExpr::Block(ExprBlock {
                            stmts: vec![],
                        })))
                    } else {
                        // Just use all parsed items
                        Ok(Box::new(AstExpr::Block(ExprBlock {
                            stmts: const_items,
                        })))
                    }
                }
            }
            Err(_e) => {
                // For transpilation mode, if parsing fails, try to extract just the struct definitions
                self.try_parse_structs_only(source)
            }
        }
    }

    pub fn try_parse_block_expression(&self, source: &str) -> Result<BExpr> {
        let wrapped_source = format!("{{\n{}\n}}", source);
        let syn_expr: syn::Expr = syn::parse_str(&wrapped_source)
            .map_err(|e| eyre!("Failed to parse as block: {}", e))?;

        let ast_expr = self.parse_expr(syn_expr)
            .map_err(|e| eyre!("Failed to convert to AST: {}", e))?;

        Ok(Box::new(ast_expr))
    }

    pub fn try_parse_simple_expression(&self, source: &str) -> Result<BExpr> {
        let syn_expr: syn::Expr = syn::parse_str(source)
            .map_err(|e| eyre!("Failed to parse as expression: {}", e))?;

        let ast_expr = self.parse_expr(syn_expr)
            .map_err(|e| eyre!("Failed to convert to AST: {}", e))?;

        Ok(Box::new(ast_expr))
    }

    pub fn try_parse_structs_only(&self, source: &str) -> Result<BExpr> {
        // Try to parse individual items from the source, filtering out problematic ones
        let syn_file: syn::File = syn::parse_str(source)
            .map_err(|e| eyre!("Failed to parse source: {}", e))?;

        let mut parsed_items = Vec::new();

        for item in syn_file.items {
            // Filter to only struct definitions and other safe items
            match item {
                syn::Item::Struct(_) | syn::Item::Enum(_) | syn::Item::Type(_) => {
                    if let Ok(ast_item) = self.parse_item(item) {
                        parsed_items.push(BlockStmt::Item(Box::new(ast_item)));
                    }
                }
                _ => {
                    // Skip other item types that might cause issues
                }
            }
        }

        Ok(Box::new(AstExpr::Block(ExprBlock {
            stmts: parsed_items,
        })))
    }
}

impl AstDeserializer for RustParser {
    fn deserialize_node(&self, code: &str) -> fp_core::error::Result<AstNode> {
        let code: syn::File = parse_str(code).map_err(|e| eyre!(e.to_string()))?;
        let path = PathBuf::from("__file__");
        Ok(self.parse_file_content(path, code).map(AstNode::File)?)
    }

    fn deserialize_expr(&self, code: &str) -> fp_core::error::Result<AstExpr> {
        let code: syn::Expr = parse_str(code).map_err(|e| eyre!(e.to_string()))?;
        Ok(self.parse_expr(code)?)
    }

    fn deserialize_item(&self, code: &str) -> fp_core::error::Result<AstItem> {
        let code: syn::Item = parse_str(code).map_err(|e| eyre!(e.to_string()))?;
        Ok(self.parse_item(code)?)
    }

    fn deserialize_file_load(&self, path: &std::path::Path) -> fp_core::error::Result<AstFile> {
        Ok(self.parse_file_recursively(path.to_owned())?)
    }
    fn deserialize_type(&self, code: &str) -> fp_core::error::Result<AstType> {
        let code: syn::Type = parse_str(code).map_err(|e| eyre!(e.to_string()))?;
        Ok(self.parse_type(code)?)
    }
}
