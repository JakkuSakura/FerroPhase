use barebone::{Block, Ident, *};
use common::{Result, *};
use quote::*;
use syn::*;

pub struct RustSerde;
impl Serializer for RustSerde {
    fn serialize(&self, node: &AstNode) -> String {
        if let Some(n) = node.as_ast::<Block>() {
            let stmts = n.stmts.iter().map(|x| self.serialize(x)).collect_vec();
            return if n.last_value {
                quote!({
                    #(#stmts);*
                })
                .to_string()
            } else {
                quote!({
                    #(#stmts;)*
                })
                .to_string()
            };
        }
        todo!("Unable to serialize {:?}", node)
    }
}
impl Deserializer for RustSerde {
    fn deserialize(&self, code: &str) -> Result<AstNode> {
        let code: syn::File = parse_str(code)?;
        parse_file(code)
    }
}
fn parse_type(t: syn::Type) -> Result<AstNode> {
    let t = match t {
        Type::Array(_) => {
            todo!()
        }
        // Type::BareFn(_) => {}
        // Type::Group(_) => {}
        // Type::ImplTrait(_) => {}
        // Type::Infer(_) => {}
        // Type::Macro(_) => {}
        // Type::Never(_) => {}
        // Type::Paren(_) => {}
        // Type::Path(_) => {}
        // Type::Ptr(_) => {}
        // Type::Reference(_) => {}
        // Type::Slice(_) => {}
        // Type::TraitObject(_) => {}
        // Type::Tuple(_) => {}
        // Type::Verbatim(_) => {}
        _ => todo!(),
    };
    Ok(t)
}
fn parse_input(i: FnArg) -> Result<Param> {
    Ok(match i {
        FnArg::Receiver(_) => {
            todo!()
        }
        FnArg::Typed(t) => Param {
            name: parse_pat(*t.pat)?,
            ty: parse_type(*t.ty)?,
        },
    })
}
fn parse_pat(p: syn::Pat) -> Result<Ident> {
    Ok(match p {
        Pat::Ident(name) => Ident {
            name: name.ident.to_string(),
        },
        _ => todo!(),
    })
}
fn parse_fn(f: ItemFn) -> Result<Fun> {
    Ok(Fun {
        name: Some(Ident {
            name: f.sig.ident.to_string(),
        }),
        params: Params {
            params: f
                .sig
                .inputs
                .into_iter()
                .map(|x| parse_input(x))
                .try_collect()?,
        },
        ret: match f.sig.output {
            ReturnType::Default => Ident {
                name: "()".to_string(),
            }
            .into(),
            ReturnType::Type(_, t) => parse_type(*t)?,
        },
        body: Some(parse_block(*f.block)?),
    })
}
fn parse_expr(expr: syn::Expr) -> Result<AstNode> {
    Ok(match expr {
        // Expr::Array(_) => {}
        // Expr::Assign(_) => {}
        // Expr::AssignOp(_) => {}
        // Expr::Async(_) => {}
        // Expr::Await(_) => {}
        // Expr::Binary(_) => {}
        // Expr::Block(_) => {}
        // Expr::Box(_) => {}
        // Expr::Break(_) => {}
        Expr::Call(c) => {
            todo!()
        }
        // Expr::Cast(_) => {}
        // Expr::Closure(_) => {}
        // Expr::Continue(_) => {}
        // Expr::Field(_) => {}
        // Expr::ForLoop(_) => {}
        // Expr::Group(_) => {}
        // Expr::If(_) => {}
        // Expr::Index(_) => {}
        // Expr::Let(_) => {}
        // Expr::Lit(_) => {}
        // Expr::Loop(_) => {}
        // Expr::Macro(_) => {}
        // Expr::Match(_) => {}
        // Expr::MethodCall(_) => {}
        // Expr::Paren(_) => {}
        // Expr::Path(_) => {}
        // Expr::Range(_) => {}
        // Expr::Reference(_) => {}
        // Expr::Repeat(_) => {}
        // Expr::Return(_) => {}
        // Expr::Struct(_) => {}
        // Expr::Try(_) => {}
        // Expr::TryBlock(_) => {}
        // Expr::Tuple(_) => {}
        // Expr::Type(_) => {}
        // Expr::Unary(_) => {}
        // Expr::Unsafe(_) => {}
        // Expr::Verbatim(_) => {}
        // Expr::While(_) => {}
        // Expr::Yield(_) => {}
        _ => todo!(),
    })
}
fn parse_stmt(stmt: syn::Stmt) -> Result<AstNode> {
    Ok(match stmt {
        Stmt::Local(l) => Def {
            name: parse_pat(l.pat)?,
            ty: None,
            value: parse_expr(*l.init.context("No value")?.1)?,
        }
        .into(),
        Stmt::Item(_) => {
            todo!()
        }
        Stmt::Expr(_) => {
            todo!()
        }
        Stmt::Semi(_, _) => {
            todo!()
        }
    })
}
fn parse_block(block: syn::Block) -> Result<Block> {
    Ok(Block {
        stmts: block.stmts.into_iter().map(parse_stmt).try_collect()?,
        last_value: false,
    })
}
fn parse_item(item: syn::Item) -> Result<AstNode> {
    match item {
        Item::Fn(f) => parse_fn(f).map(|x| x.into()),
        _ => todo!(),
    }
}
fn parse_file(file: syn::File) -> Result<AstNode> {
    Ok(Module {
        stmts: file.items.into_iter().map(parse_item).try_collect()?,
    }
    .into())
}
