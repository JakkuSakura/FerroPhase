use common::Result;
use common::*;

use crate::{RawExpr, RawExprMacro};
use common_lang::ops::{BinOpKind, BuiltinFn, BuiltinFnName};
use common_lang::tree::*;
use common_lang::value::*;
use proc_macro2::TokenStream;
use quote::*;

pub struct RustPrinter;
impl RustPrinter {
    pub fn print_ident(&self, i: &Ident) -> TokenStream {
        match i.as_str() {
            "+" => quote!(+),
            "*" => quote!(*),
            ">" => quote!(>),
            ">=" => quote!(>=),
            "<" => quote!(<),
            "<=" => quote!(<=),
            "==" => quote!(==),
            "!=" => quote!(!=),
            "|" => quote!(|),
            "&Self" => quote!(&Self),
            "&mut Self" => quote!(&mut Self),
            "Self" => quote!(Self),
            "mut Self" => quote!(mut Self),
            "unit" => quote!(()),
            a => format_ident!("{}", a).into_token_stream(),
        }
    }
    pub fn print_define(&self, def: &Define) -> Result<TokenStream> {
        let vis = def.visibility;
        let decl = &def.value;

        match decl {
            DefValue::Function(n) => {
                return self.print_func_decl(&def.name, n, vis);
            }
            DefValue::Type(n) => {
                let name = self.print_ident(&def.name);
                let ty = self.print_type_expr(n)?;
                return Ok(quote!(
                    type #name = t!{ #ty };
                ));
            }
            DefValue::Const(n) => {
                let name = self.print_ident(&def.name);
                let ty = self.print_type_value(&def.ty.as_ref().context("No type")?.clone())?;
                let value = self.print_expr(n)?;
                return Ok(quote!(
                    const #name: #ty = #value;
                ));
            }
        }
    }
    pub fn print_field(&self, field: &FieldTypeValue) -> Result<TokenStream> {
        let name = self.print_ident(&field.name);
        let ty = self.print_type_value(&field.value)?;
        Ok(quote!(pub #name: #ty ))
    }
    pub fn print_struct_type(&self, s: &NamedStructType) -> Result<TokenStream> {
        let name = self.print_ident(&s.name);
        let fields: Vec<_> = s
            .fields
            .iter()
            .map(|x| self.print_field(&x))
            .try_collect()?;
        Ok(quote!(struct #name {
            #(#fields), *
        }))
    }
    pub fn print_unnamed_struct_type(&self, s: &UnnamedStructType) -> Result<TokenStream> {
        let fields: Vec<_> = s
            .fields
            .iter()
            .map(|x| self.print_field(&x))
            .try_collect()?;
        Ok(quote!(
            struct {
                #(#fields), *
            }
        ))
    }
    pub fn print_bin_op_kind(&self, op: &BinOpKind) -> TokenStream {
        match op {
            BinOpKind::Add => quote!(+),
            BinOpKind::Sub => quote!(-),
            BinOpKind::Mul => quote!(*),
            BinOpKind::Div => quote!(/),
            BinOpKind::Mod => quote!(%),
            BinOpKind::Gt => quote!(>),
            BinOpKind::Lt => quote!(<),
            BinOpKind::Ge => quote!(>=),
            BinOpKind::Le => quote!(<=),
            BinOpKind::Eq => quote!(==),
            BinOpKind::Ne => quote!(!=),
            BinOpKind::LogicalOr => quote!(||),
            BinOpKind::LogicalAnd => quote!(&&),
            BinOpKind::BitOr => quote!(|),
            BinOpKind::BitAnd => quote!(&),
            BinOpKind::BitXor => quote!(^),
            BinOpKind::Any(ident) => self.print_ident(ident),
        }
    }
    pub fn print_invoke_expr(&self, invoke: &Invoke) -> Result<TokenStream> {
        match &*invoke.func {
            Expr::BinOpKind(op) => {
                let op = self.print_bin_op_kind(op);
                let args: Vec<_> = invoke
                    .args
                    .iter()
                    .map(|x| self.print_expr(x))
                    .try_collect()?;
                let mut stream = quote!();
                for (i, a) in args.into_iter().enumerate() {
                    if i != 0 {
                        stream = quote!(#stream #op);
                    }
                    stream = quote!(#stream #a);
                }
                return Ok(stream);
            }
            _ => {
                let fun = self.print_expr(&invoke.func)?;
                let args: Vec<_> = invoke
                    .args
                    .iter()
                    .map(|x| self.print_expr(x))
                    .try_collect()?;
                Ok(quote!(
                    #fun(#(#args), *)
                ))
            }
        }
    }
    pub fn print_invoke_type(&self, invoke: &Invoke) -> Result<TokenStream> {
        let fun = self.print_expr(&invoke.func)?;
        let args: Vec<_> = invoke
            .args
            .iter()
            .map(|x| self.print_expr(x))
            .try_collect()?;
        Ok(quote!(
            #fun::<#(#args), *>
        ))
    }

    pub fn print_items_chunk(&self, items: &[Item]) -> Result<TokenStream> {
        let mut stmts = vec![];
        for item in items {
            let item = self.print_item(item)?;
            stmts.push(item);
        }
        Ok(quote!(#(#stmts) *))
    }
    pub fn print_let(&self, let_: &Let) -> Result<TokenStream> {
        let name = self.print_ident(&let_.name);
        let ty = if let Some(ty) = &let_.ty {
            let x = self.print_type_value(ty)?;
            quote!(: #x)
        } else {
            quote!()
        };
        let value = self.print_expr(&let_.value)?;
        Ok(quote!(
            let #name #ty = #value;
        ))
    }

    pub fn print_statement(&self, stmt: &Statement) -> Result<TokenStream> {
        match stmt {
            Statement::Item(item) => self.print_item(item),
            Statement::Let(let_) => self.print_let(let_),
            Statement::StmtExpr(stmt) => self.print_stmt_expr(&stmt),
            Statement::Expr(expr) => self.print_expr(expr),
            Statement::Any(any) => self.print_any(any),
        }
    }
    pub fn print_statement_chunk(&self, items: &[Statement]) -> Result<TokenStream> {
        let mut stmts = vec![];
        for item in items {
            let item = self.print_statement(item)?;
            stmts.push(item);
        }
        Ok(quote!(#(#stmts) *))
    }
    pub fn print_block(&self, n: &Block) -> Result<TokenStream> {
        let chunk = self.print_statement_chunk(&n.stmts)?;
        Ok(quote!({
            #chunk
        }))
    }

    pub fn print_cond(&self, cond: &Cond) -> Result<TokenStream> {
        let mut ts = vec![];
        if cond.if_style {
            for (i, c) in cond.cases.iter().enumerate() {
                let node = &c.cond;
                let co = self.print_expr(node)?;
                let node = &c.body;
                let ex = self.print_expr_optimized(node)?;
                if i == 0 {
                    ts.push(quote!(
                        if #co {
                            #ex
                        }
                    ));
                } else if i < cond.cases.len() - 1 {
                    ts.push(quote!(
                        else if #co {
                            #ex
                        }
                    ));
                } else {
                    ts.push(quote!(
                        else {
                            #ex
                        }
                    ));
                }
            }
            Ok(quote!(#(#ts)*))
        } else {
            for (_i, c) in cond.cases.iter().enumerate() {
                let node = &c.cond;
                let co = self.print_expr(node)?;
                let node = &c.body;
                let ex = self.print_expr_optimized(node)?;
                ts.push(quote!(
                    if #co => { #ex }
                ))
            }
            Ok(quote!(match () {
                () #(#ts)*
                _ => {}
            }))
        }
    }
    pub fn print_vis(&self, vis: Visibility) -> TokenStream {
        match vis {
            Visibility::Public => quote!(pub),
            Visibility::Private => quote!(),
            Visibility::Inherited => quote!(),
        }
    }
    pub fn print_func_decl(
        &self,
        name: &Ident,
        func: &FunctionValue,
        vis: Visibility,
    ) -> Result<TokenStream> {
        let name = self.print_ident(&name);
        let ret_type = &func.ret;
        let ret = self.print_return_type(ret_type)?;
        let param_names: Vec<_> = func
            .params
            .iter()
            .map(|x| self.print_ident(&x.name))
            .collect();
        let param_types: Vec<_> = func
            .params
            .iter()
            .map(|x| self.print_type_value(&x.ty))
            .try_collect()?;
        let stmts = self.print_expr_optimized(&func.body)?;
        let gg;
        if !func.generics_params.is_empty() {
            let gt: Vec<_> = func
                .generics_params
                .iter()
                .map(|x| self.print_ident(&x.name))
                .collect();
            let gb: Vec<_> = func
                .generics_params
                .iter()
                .map(|x| self.print_type_value(&x.ty))
                .try_collect()?;
            gg = quote!(<#(#gt: #gb), *>)
        } else {
            gg = quote!();
        }
        let vis = self.print_vis(vis);
        return Ok(quote!(
            #vis fn #name #gg(#(#param_names: #param_types), *) #ret {
                #stmts
            }
        ));
    }
    pub fn print_invoke(&self, node: &Invoke) -> Result<TokenStream> {
        let fun = self.print_expr(&node.func)?;
        let args: Vec<_> = node.args.iter().map(|x| self.print_expr(x)).try_collect()?;
        match &*node.func {
            Expr::Select(select) => match select.select {
                SelectType::Field => {
                    return Ok(quote!(
                        (#fun)(#(#args), *)
                    ))
                }
                _ => {}
            },
            _ => {}
        }

        let fun_str = fun.to_string();

        let code = match fun_str.as_str() {
            "+" => quote!(#(#args) + *),
            "-" => quote!(#(#args) - *),
            "/" => quote!(#(#args) / *),
            "|" => quote!(#(#args) | *),
            "*" => {
                let mut result = vec![];
                for (i, a) in args.into_iter().enumerate() {
                    if i != 0 {
                        result.push(quote!(*));
                    }
                    result.push(a);
                }
                quote!(#(#result)*)
            }
            "tuple" => quote!(
                (#(#args), *)
            ),
            _ => quote!(
                #fun(#(#args), *)
            ),
        };
        // if true {
        //     return Ok(quote!((#code)));
        // }
        Ok(code)
    }
    pub fn print_ref(&self, n: &Reference) -> Result<TokenStream> {
        let referee = self.print_expr(&n.referee)?;
        if n.mutable == Some(true) {
            Ok(quote!(&mut #referee))
        } else {
            Ok(quote!(&#referee))
        }
    }

    pub fn print_func_type_param(&self, param: &FunctionParam) -> Result<TokenStream> {
        let name = self.print_ident(&param.name);
        let ty = self.print_type_value(&param.ty)?;
        Ok(quote!(#name: #ty))
    }
    pub fn print_return_type(&self, node: &TypeValue) -> Result<TokenStream> {
        if matches!(node, TypeValue::Unit(_)) {
            return Ok(quote!());
        }
        let ty = self.print_type_value(&node)?;
        Ok(quote!(-> #ty))
    }
    pub fn print_func_value(&self, fun: &FunctionValue) -> Result<TokenStream> {
        let args: Vec<_> = fun
            .params
            .iter()
            .map(|x| self.print_func_type_param(x))
            .try_collect()?;
        let node = &fun.ret;
        let ret = self.print_return_type(node)?;
        Ok(quote!(
            fn(#(#args), *) #ret
        ))
    }
    pub fn print_func_type(&self, fun: &FunctionType) -> Result<TokenStream> {
        let args: Vec<_> = fun
            .params
            .iter()
            .map(|x| self.print_type_value(x))
            .try_collect()?;
        let node = &fun.ret;
        let ret = self.print_return_type(node)?;
        Ok(quote!(
            fn(#(#args), *) #ret
        ))
    }
    pub fn print_select(&self, select: &Select) -> Result<TokenStream> {
        let obj = self.print_expr(&select.obj)?;
        let field = self.print_ident(&select.field);
        match select.select {
            SelectType::Const => Ok(quote!(
                #obj::#field
            )),
            _ => Ok(quote!(
                #obj.#field
            )),
        }
    }
    pub fn print_args(&self, node: &Vec<Expr>) -> Result<TokenStream> {
        let args: Vec<_> = node.iter().map(|x| self.print_expr(x)).try_collect()?;
        Ok(quote!((#(#args),*)))
    }

    pub fn print_impl(&self, impl_: &Impl) -> Result<TokenStream> {
        let name = self.print_ident(&impl_.name);
        let methods: Vec<_> = impl_
            .defs
            .iter()
            .map(|x| self.print_define(x))
            .try_collect()?;
        Ok(quote!(
            impl #name {
                #(#methods)*
            }
        ))
    }
    pub fn print_module(&self, m: &Module) -> Result<TokenStream> {
        let stmts = self.print_items_chunk(&m.items)?;
        if m.name.as_str() == "__file__" {
            Ok(quote!(
                #stmts
            ))
        } else {
            let mod_name = format_ident!("{}", m.name.as_str());
            Ok(quote!(
                pub mod #mod_name {
                    #stmts
                }
            ))
        }
    }
    pub fn print_import(&self, node: &Import) -> Result<TokenStream> {
        let vis = self.print_vis(node.visibility);
        let segments = node
            .path
            .segments
            .iter()
            .map(|x| self.print_ident(x))
            .collect::<Vec<_>>();
        Ok(quote!(#vis use #(#segments)::*;))
    }
    pub fn print_field_value(&self, s: &FieldValue) -> Result<TokenStream> {
        let name = self.print_ident(&s.name);
        let value = self.print_value(&s.value)?;
        Ok(quote!(#name: #value))
    }
    pub fn print_struct_value(&self, s: &StructValue) -> Result<TokenStream> {
        let name = self.print_type_expr(&s.name)?;
        let kwargs: Vec<_> = s
            .fields
            .iter()
            .map(|x| self.print_field_value(x))
            .try_collect()?;
        Ok(quote!(#name { #(#kwargs), * }))
    }
    pub fn print_builtin_fn(&self, bt: &BuiltinFn) -> Result<TokenStream> {
        match bt.name {
            BuiltinFnName::BinOpKind(ref op) => Ok(self.print_bin_op_kind(op)),
            BuiltinFnName::Name(ref name) => Ok(self.print_ident(name)),
        }
    }
    pub fn print_int(&self, n: &IntValue) -> Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_bool(&self, n: &BoolValue) -> Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_decimal(&self, n: &DecimalValue) -> Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_char(&self, n: &CharValue) -> Result<TokenStream> {
        let n = n.value;
        Ok(quote!(#n))
    }
    pub fn print_string(&self, n: &StringValue) -> Result<TokenStream> {
        let v = &n.value;
        return if n.owned {
            Ok(quote!(
                #v.to_string()
            ))
        } else {
            Ok(quote!(
                #v
            ))
        };
    }
    pub fn print_list_expr(&self, n: &[Expr]) -> Result<TokenStream> {
        let n: Vec<_> = n.iter().map(|x| self.print_expr(x)).try_collect()?;
        Ok(quote!(vec![#(#n),*]))
    }
    pub fn print_list_value(&self, n: &ListValue) -> Result<TokenStream> {
        let n: Vec<_> = n.values.iter().map(|x| self.print_value(x)).try_collect()?;
        Ok(quote!(vec![#(#n),*]))
    }
    pub fn print_unit(&self, _n: &UnitValue) -> Result<TokenStream> {
        Ok(quote!(()))
    }
    pub fn print_type(&self, t: &TypeValue) -> Result<TokenStream> {
        match t {
            TypeValue::Function(f) => self.print_func_type(f),
            TypeValue::NamedStruct(s) => self.print_struct_type(s),
            _ => bail!("Not supported {:?}", t),
        }
    }
    pub fn print_any(&self, n: &AnyBox) -> Result<TokenStream> {
        if let Some(n) = n.downcast_ref::<RawExprMacro>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(n) = n.downcast_ref::<RawExpr>() {
            return Ok(n.raw.to_token_stream());
        }
        if let Some(f) = n.downcast_ref::<BuiltinFn>() {
            return self.print_builtin_fn(f);
        }
        bail!("Not supported {:?}", n)
    }

    pub fn print_value(&self, v: &Value) -> Result<TokenStream> {
        match v {
            Value::Function(f) => self.print_func_value(f),
            Value::Int(i) => self.print_int(i),
            Value::Bool(b) => self.print_bool(b),
            Value::Decimal(d) => self.print_decimal(d),
            Value::Char(c) => self.print_char(c),
            Value::String(s) => self.print_string(s),
            Value::List(l) => self.print_list_value(l),
            Value::Unit(u) => self.print_unit(u),
            Value::Type(t) => self.print_type(t),
            Value::Struct(s) => self.print_struct_value(s),
            Value::Any(n) => self.print_any(n),
            _ => bail!("Not supported {:?}", v),
        }
    }
    pub fn print_primitive_type(&self, ty: PrimitiveType) -> Result<TokenStream> {
        match ty {
            PrimitiveType::Int(IntType::I64) => Ok(quote!(i64)),
            PrimitiveType::Int(IntType::U64) => Ok(quote!(u64)),
            PrimitiveType::Int(IntType::I32) => Ok(quote!(i32)),
            PrimitiveType::Int(IntType::U32) => Ok(quote!(u32)),
            PrimitiveType::Int(IntType::I16) => Ok(quote!(i16)),
            PrimitiveType::Int(IntType::U16) => Ok(quote!(u16)),
            PrimitiveType::Int(IntType::I8) => Ok(quote!(i8)),
            PrimitiveType::Int(IntType::U8) => Ok(quote!(u8)),
            PrimitiveType::Decimal(DecimalType::F64) => Ok(quote!(f64)),
            PrimitiveType::Decimal(DecimalType::F32) => Ok(quote!(f32)),
            PrimitiveType::Bool => Ok(quote!(bool)),
            PrimitiveType::String => Ok(quote!(String)),
            PrimitiveType::Char => Ok(quote!(char)),
            PrimitiveType::List => Ok(quote!(Vec)),
            _ => bail!("Not supported {:?}", ty),
        }
    }
    pub fn print_impl_traits(&self, traits: &ImplTraits) -> Result<TokenStream> {
        let traits: Vec<_> = traits
            .traits
            .iter()
            .map(|x| self.print_ident(&x.name))
            .collect();
        Ok(quote!(impl #(#traits)+ *))
    }
    pub fn print_type_value(&self, v: &TypeValue) -> Result<TokenStream> {
        match v {
            TypeValue::Function(f) => self.print_func_type(f),
            TypeValue::Primitive(p) => self.print_primitive_type(*p),
            TypeValue::NamedStruct(s) => self.print_struct_type(s),
            TypeValue::UnnamedStruct(s) => self.print_unnamed_struct_type(s),
            TypeValue::Expr(e) => self.print_type_expr(e),
            TypeValue::ImplTraits(t) => self.print_impl_traits(t),
            TypeValue::Unit(_) => Ok(quote!(())),
            TypeValue::Any(_) => Ok(quote!(Box<dyn Any>)),
            TypeValue::Nothing(_) => Ok(quote!(!)),
            _ => bail!("Not supported {:?}", v),
        }
    }

    pub fn print_path(&self, path: &Path) -> TokenStream {
        let segments: Vec<_> = path.segments.iter().map(|x| self.print_ident(x)).collect();
        quote!(#(#segments)::*)
    }

    pub fn print_type_expr(&self, node: &TypeExpr) -> Result<TokenStream> {
        match node {
            TypeExpr::Ident(n) => Ok(self.print_ident(n)),
            TypeExpr::Path(n) => Ok(self.print_path(n)),
            TypeExpr::Value(n) => self.print_type_value(n),
            TypeExpr::Invoke(n) => self.print_invoke_type(n),
            TypeExpr::BinOp(TypeBinOp::Add(add)) => {
                let left = self.print_type_expr(&add.lhs)?;
                let right = self.print_type_expr(&add.rhs)?;
                Ok(quote!(#left + #right))
            }
            TypeExpr::BinOp(TypeBinOp::Sub(sub)) => {
                let left = self.print_type_expr(&sub.lhs)?;
                let right = self.print_type_expr(&sub.rhs)?;
                Ok(quote!(#left - #right))
            }
            _ => bail!("Unable to serialize {:?}", node),
        }
    }
    pub fn print_stmt_expr(&self, node: &StatementExpr) -> Result<TokenStream> {
        let expr = self.print_expr(&node.expr)?;
        Ok(quote!(#expr;))
    }
    pub fn print_expr_optimized(&self, node: &Expr) -> Result<TokenStream> {
        match node {
            Expr::Cond(n) => self.print_cond(n),
            Expr::Block(n) => self.print_statement_chunk(&n.stmts),
            Expr::Value(Value::Unit(_)) => Ok(quote!()),
            _ => self.print_expr(node),
        }
    }
    pub fn print_expr(&self, node: &Expr) -> Result<TokenStream> {
        match node {
            Expr::Ident(n) => Ok(self.print_ident(n)),
            Expr::Path(n) => Ok(self.print_path(n)),
            Expr::Value(n) => self.print_value(n),
            Expr::Invoke(n) => self.print_invoke_expr(n),
            Expr::Any(n) => self.print_any(n),
            Expr::BinOpKind(n) => Ok(self.print_bin_op_kind(n)),
            Expr::Cond(n) => self.print_cond(n),
            Expr::Block(n) => self.print_block(n),
            _ => bail!("Unable to serialize {:?}", node),
        }
    }
    pub fn print_item(&self, item: &Item) -> Result<TokenStream> {
        match item {
            Item::Def(n) => self.print_define(n),
            Item::Module(n) => self.print_module(n),
            Item::Import(n) => self.print_import(n),
            _ => bail!("Unable to serialize {:?}", item),
        }
    }
    pub fn print_tree(&self, node: &Tree) -> Result<TokenStream> {
        match node {
            Tree::Item(n) => self.print_item(n),
        }
    }
}
