//! Python frontend backed by rustpython-parser.

use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;

use fp_core::ast::{
    BlockStmt, BlockStmtExpr, Expr, ExprArray, ExprAssign, ExprBinOp, ExprBlock, ExprBreak,
    ExprContinue, ExprFor, ExprIf, ExprIndex, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget,
    ExprKind, ExprKwArg, ExprReturn, ExprSelect, ExprSelectType, ExprStringTemplate, ExprTuple,
    ExprUnOp, ExprWhile, File, FormatTemplatePart, FunctionParam, FunctionSignature, Ident, Item,
    ItemDefFunction, ItemKind, Name, Node, NodeKind, Pattern, PatternIdent, PatternKind,
    PatternTuple, Ty, Value, ValueBytes, ValueMap, ValueTuple,
};
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::frontend::{FrontendResult, LanguageFrontend};
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::Span;
use num_bigint::BigInt as NumBigInt;
use num_traits::ToPrimitive;
use rustpython_parser::ast as py_ast;
use rustpython_parser::text_size::TextRange;
use rustpython_parser::Parse;

use crate::PythonSerializer;

/// Canonical identifier for the Python frontend.
pub const PYTHON: &str = "python";

/// Frontend that converts Python source code into FerroPhase AST.
pub struct PythonFrontend {
    serializer: Arc<PythonSerializer>,
}

impl PythonFrontend {
    pub fn new() -> Self {
        Self {
            serializer: Arc::new(PythonSerializer),
        }
    }

    fn file_path(path: Option<&Path>) -> PathBuf {
        path.map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("<python>"))
    }
}

impl Default for PythonFrontend {
    fn default() -> Self {
        Self::new()
    }
}

impl LanguageFrontend for PythonFrontend {
    fn language(&self) -> &'static str {
        PYTHON
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["py"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let source_path = path.and_then(|path| path.to_str()).unwrap_or("<python>");
        let program: py_ast::Suite = py_ast::Suite::parse(source, source_path)
            .map_err(|err| CoreError::from(err.to_string()))?;
        let items = lower_suite(&program)?;
        let file = File {
            path: Self::file_path(path),
            items,
        };
        let node = Node::from(NodeKind::File(file));

        Ok(FrontendResult {
            last: node.clone(),
            ast: node,
            serializer: self.serializer.clone(),
            intrinsic_normalizer: None,
            macro_parser: None,
            snapshot: None,
            diagnostics,
        })
    }
}

type PyStmt = py_ast::Stmt<TextRange>;
type PyExpr = py_ast::Expr<TextRange>;

type PyArguments = py_ast::Arguments<TextRange>;

type PyConstant = py_ast::Constant;

type PyOperator = py_ast::Operator;
type PyBoolOp = py_ast::BoolOp;
type PyUnaryOp = py_ast::UnaryOp;
type PyCmpOp = py_ast::CmpOp;

type PyAlias = py_ast::Alias<TextRange>;

fn lower_suite(stmts: &[PyStmt]) -> CoreResult<Vec<Item>> {
    let mut items = Vec::new();
    for stmt in stmts {
        items.extend(lower_stmt_to_items(stmt)?);
    }
    Ok(items)
}

fn lower_stmt_to_items(stmt: &PyStmt) -> CoreResult<Vec<Item>> {
    match stmt {
        PyStmt::FunctionDef(def) => {
            let func = lower_function_def(def)?;
            Ok(vec![Item::from(ItemKind::DefFunction(func))])
        }
        PyStmt::Expr(expr_stmt) => {
            let expr = lower_expr(&expr_stmt.value)?;
            Ok(vec![Item::from(ItemKind::Expr(expr))])
        }
        PyStmt::Assign(assign) => {
            let expr = lower_assign(assign)?;
            Ok(vec![Item::from(ItemKind::Expr(expr))])
        }
        PyStmt::If(stmt_if) => {
            let expr = lower_if(stmt_if)?;
            Ok(vec![Item::from(ItemKind::Expr(expr))])
        }
        PyStmt::While(stmt_while) => {
            let expr = lower_while(stmt_while)?;
            Ok(vec![Item::from(ItemKind::Expr(expr))])
        }
        PyStmt::For(stmt_for) => {
            let expr = lower_for(stmt_for)?;
            Ok(vec![Item::from(ItemKind::Expr(expr))])
        }
        PyStmt::Return(stmt_return) => {
            let expr = lower_return(stmt_return)?;
            Ok(vec![Item::from(ItemKind::Expr(expr))])
        }
        PyStmt::Break(_) => {
            let expr = Expr::new(ExprKind::Break(ExprBreak {
                span: Span::null(),
                value: None,
            }));
            Ok(vec![Item::from(ItemKind::Expr(expr))])
        }
        PyStmt::Continue(_) => {
            let expr = Expr::new(ExprKind::Continue(ExprContinue { span: Span::null() }));
            Ok(vec![Item::from(ItemKind::Expr(expr))])
        }
        PyStmt::Pass(_) => Ok(vec![Item::from(ItemKind::Expr(Expr::unit()))]),
        PyStmt::Import(import) => {
            let item = lower_import(&import.names)?;
            Ok(vec![Item::from(ItemKind::Import(item))])
        }
        PyStmt::ImportFrom(import) => {
            let item = lower_import_from(import)?;
            Ok(vec![Item::from(ItemKind::Import(item))])
        }
        _ => Err(CoreError::from(format!(
            "unsupported python statement: {:?}",
            stmt
        ))),
    }
}

fn lower_stmt_to_block_stmt(stmt: &PyStmt) -> CoreResult<BlockStmt> {
    match stmt {
        PyStmt::FunctionDef(def) => {
            let func = lower_function_def(def)?;
            Ok(BlockStmt::item(Item::from(ItemKind::DefFunction(func))))
        }
        PyStmt::Expr(expr_stmt) => {
            let expr = lower_expr(&expr_stmt.value)?;
            Ok(BlockStmt::Expr(BlockStmtExpr::new(expr)))
        }
        PyStmt::Assign(assign) => {
            let expr = lower_assign(assign)?;
            Ok(BlockStmt::Expr(BlockStmtExpr::new(expr)))
        }
        PyStmt::If(stmt_if) => {
            let expr = lower_if(stmt_if)?;
            Ok(BlockStmt::Expr(BlockStmtExpr::new(expr)))
        }
        PyStmt::While(stmt_while) => {
            let expr = lower_while(stmt_while)?;
            Ok(BlockStmt::Expr(BlockStmtExpr::new(expr)))
        }
        PyStmt::For(stmt_for) => {
            let expr = lower_for(stmt_for)?;
            Ok(BlockStmt::Expr(BlockStmtExpr::new(expr)))
        }
        PyStmt::Return(stmt_return) => {
            let expr = lower_return(stmt_return)?;
            Ok(BlockStmt::Expr(BlockStmtExpr::new(expr)))
        }
        PyStmt::Break(_) => Ok(BlockStmt::Expr(BlockStmtExpr::new(Expr::new(
            ExprKind::Break(ExprBreak {
                span: Span::null(),
                value: None,
            }),
        )))),
        PyStmt::Continue(_) => Ok(BlockStmt::Expr(BlockStmtExpr::new(Expr::new(
            ExprKind::Continue(ExprContinue { span: Span::null() }),
        )))),
        PyStmt::Pass(_) => Ok(BlockStmt::Noop),
        _ => Err(CoreError::from(format!(
            "unsupported python statement in block: {:?}",
            stmt
        ))),
    }
}

fn lower_block(stmts: &[PyStmt]) -> CoreResult<ExprBlock> {
    let mut block = ExprBlock::new();
    for stmt in stmts {
        block.stmts.push(lower_stmt_to_block_stmt(stmt)?);
    }
    Ok(block)
}

fn lower_function_def(def: &py_ast::StmtFunctionDef<TextRange>) -> CoreResult<ItemDefFunction> {
    if !def.decorator_list.is_empty() {
        return Err(CoreError::from("python decorators are not supported"));
    }
    let name = Ident::new(def.name.as_str());
    let sig = lower_function_signature(&def.args, Some(name.clone()))?;
    let body = lower_block(&def.body)?;

    Ok(ItemDefFunction {
        ty_annotation: None,
        attrs: Vec::new(),
        name,
        ty: None,
        sig,
        body: Expr::block(body).into(),
        visibility: fp_core::ast::Visibility::Public,
    })
}

fn lower_function_signature(
    args: &PyArguments,
    name: Option<Ident>,
) -> CoreResult<FunctionSignature> {
    let mut sig = FunctionSignature::unit();
    sig.name = name;

    let mut params = Vec::new();

    for arg in &args.posonlyargs {
        params.push(lower_function_param(arg, true, false)?);
    }
    for arg in &args.args {
        params.push(lower_function_param(arg, false, false)?);
    }
    if let Some(vararg) = &args.vararg {
        let mut param = FunctionParam::new(Ident::new(vararg.arg.as_str()), Ty::ANY);
        param.as_tuple = true;
        params.push(param);
    }
    for arg in &args.kwonlyargs {
        params.push(lower_function_param(arg, false, true)?);
    }
    if let Some(kwarg) = &args.kwarg {
        let mut param = FunctionParam::new(Ident::new(kwarg.arg.as_str()), Ty::ANY);
        param.as_dict = true;
        params.push(param);
    }

    sig.params = params;
    sig.ret_ty = None;
    Ok(sig)
}

fn lower_function_param(
    arg: &py_ast::ArgWithDefault<TextRange>,
    positional_only: bool,
    keyword_only: bool,
) -> CoreResult<FunctionParam> {
    let mut param = FunctionParam::new(Ident::new(arg.def.arg.as_str()), Ty::ANY);
    param.positional_only = positional_only;
    param.keyword_only = keyword_only;
    if let Some(default_expr) = &arg.default {
        let expr = lower_expr(default_expr)?;
        param.default = Some(Value::expr(expr));
    }
    Ok(param)
}

fn lower_assign(assign: &py_ast::StmtAssign<TextRange>) -> CoreResult<Expr> {
    if assign.targets.len() != 1 {
        return Err(CoreError::from(
            "python assignment with multiple targets is not supported",
        ));
    }
    let target = lower_expr(&assign.targets[0])?;
    let value = lower_expr(&assign.value)?;
    Ok(Expr::new(ExprKind::Assign(ExprAssign {
        span: Span::null(),
        target: target.into(),
        value: value.into(),
    })))
}

fn lower_if(stmt_if: &py_ast::StmtIf<TextRange>) -> CoreResult<Expr> {
    let cond = lower_expr(&stmt_if.test)?;
    let then_block = lower_block(&stmt_if.body)?;
    let then_expr = Expr::block(then_block);
    let else_expr = if stmt_if.orelse.is_empty() {
        None
    } else {
        Some(Expr::block(lower_block(&stmt_if.orelse)?))
    };
    Ok(Expr::new(ExprKind::If(ExprIf {
        span: Span::null(),
        cond: cond.into(),
        then: then_expr.into(),
        elze: else_expr.map(Box::new),
    })))
}

fn lower_while(stmt_while: &py_ast::StmtWhile<TextRange>) -> CoreResult<Expr> {
    if !stmt_while.orelse.is_empty() {
        return Err(CoreError::from("python while-else is not supported"));
    }
    let cond = lower_expr(&stmt_while.test)?;
    let body = lower_block(&stmt_while.body)?;
    Ok(Expr::new(ExprKind::While(ExprWhile {
        span: Span::null(),
        cond: cond.into(),
        body: Expr::block(body).into(),
    })))
}

fn lower_for(stmt_for: &py_ast::StmtFor<TextRange>) -> CoreResult<Expr> {
    if !stmt_for.orelse.is_empty() {
        return Err(CoreError::from("python for-else is not supported"));
    }
    let pat = lower_pattern(&stmt_for.target)?;
    let iter = lower_expr(&stmt_for.iter)?;
    let body = lower_block(&stmt_for.body)?;
    Ok(Expr::new(ExprKind::For(ExprFor {
        span: Span::null(),
        pat: Box::new(pat),
        iter: iter.into(),
        body: Expr::block(body).into(),
    })))
}

fn lower_return(stmt_return: &py_ast::StmtReturn<TextRange>) -> CoreResult<Expr> {
    let value = match &stmt_return.value {
        Some(expr) => Some(lower_expr(expr)?),
        None => None,
    };
    Ok(Expr::new(ExprKind::Return(ExprReturn {
        span: Span::null(),
        value: value.map(Box::new),
    })))
}

fn lower_pattern(expr: &PyExpr) -> CoreResult<Pattern> {
    match expr {
        PyExpr::Name(name) => Ok(Pattern::from(PatternKind::Ident(PatternIdent::new(
            Ident::new(name.id.as_str()),
        )))),
        PyExpr::Tuple(tuple) => {
            let patterns = tuple
                .elts
                .iter()
                .map(lower_pattern)
                .collect::<CoreResult<Vec<_>>>()?;
            Ok(Pattern::from(PatternKind::Tuple(PatternTuple { patterns })))
        }
        _ => Err(CoreError::from(
            "python assignment target pattern is not supported",
        )),
    }
}

fn lower_expr(expr: &PyExpr) -> CoreResult<Expr> {
    match expr {
        PyExpr::Constant(constant) => lower_constant(&constant.value),
        PyExpr::Name(name) => Ok(Expr::name(Name::ident(name.id.as_str()))),
        PyExpr::Call(call) => lower_call(call),
        PyExpr::Attribute(attr) => Ok(Expr::new(ExprKind::Select(ExprSelect {
            span: Span::null(),
            obj: Box::new(lower_expr(&attr.value)?),
            field: Ident::new(attr.attr.as_str()),
            select: ExprSelectType::Field,
        }))),
        PyExpr::Subscript(subscript) => {
            let index_expr = lower_expr(&subscript.slice)?;
            Ok(Expr::new(ExprKind::Index(ExprIndex {
                span: Span::null(),
                obj: Box::new(lower_expr(&subscript.value)?),
                index: index_expr.into(),
            })))
        }
        PyExpr::BinOp(bin) => {
            let kind = lower_operator(&bin.op)?;
            Ok(Expr::new(ExprKind::BinOp(ExprBinOp {
                span: Span::null(),
                kind,
                lhs: Box::new(lower_expr(&bin.left)?),
                rhs: Box::new(lower_expr(&bin.right)?),
            })))
        }
        PyExpr::UnaryOp(unary) => {
            let op = lower_unary_op(&unary.op)?;
            Ok(Expr::new(ExprKind::UnOp(ExprUnOp {
                span: Span::null(),
                op,
                val: Box::new(lower_expr(&unary.operand)?),
            })))
        }
        PyExpr::BoolOp(bool_op) => lower_bool_op(bool_op),
        PyExpr::Compare(compare) => lower_compare(compare),
        PyExpr::IfExp(ifexp) => Ok(Expr::new(ExprKind::If(ExprIf {
            span: Span::null(),
            cond: Box::new(lower_expr(&ifexp.test)?),
            then: Box::new(lower_expr(&ifexp.body)?),
            elze: Some(Box::new(lower_expr(&ifexp.orelse)?)),
        }))),
        PyExpr::List(list) => {
            let values = list
                .elts
                .iter()
                .map(lower_expr)
                .collect::<CoreResult<Vec<_>>>()?;
            Ok(Expr::new(ExprKind::Array(ExprArray {
                span: Span::null(),
                values,
            })))
        }
        PyExpr::Tuple(tuple) => {
            let values = tuple
                .elts
                .iter()
                .map(lower_expr)
                .collect::<CoreResult<Vec<_>>>()?;
            Ok(Expr::new(ExprKind::Tuple(ExprTuple {
                span: Span::null(),
                values,
            })))
        }
        PyExpr::Dict(dict) => {
            let mut entries = Vec::new();
            for (key, value) in dict.keys.iter().zip(&dict.values) {
                let key_expr: &PyExpr = key
                    .as_ref()
                    .ok_or_else(|| CoreError::from("python dict unpack is not supported"))?;
                let key_value = Value::expr(lower_expr(key_expr)?);
                let value_value = Value::expr(lower_expr(value)?);
                entries.push((key_value, value_value));
            }
            Ok(Expr::value(Value::Map(ValueMap::from_pairs(entries))))
        }
        PyExpr::JoinedStr(joined) => lower_joined_str(joined),
        PyExpr::FormattedValue(formatted) => lower_formatted_value(formatted),
        _ => Err(CoreError::from(format!(
            "unsupported python expression: {:?}",
            expr
        ))),
    }
}

fn lower_call(call: &py_ast::ExprCall<TextRange>) -> CoreResult<Expr> {
    let func = lower_expr(&call.func)?;
    let mut args = Vec::new();
    for arg in &call.args {
        match arg {
            PyExpr::Starred(_) => {
                return Err(CoreError::from(
                    "python starred arguments are not supported",
                ))
            }
            _ => args.push(lower_expr(arg)?),
        }
    }

    let mut kwargs = Vec::new();
    for keyword in &call.keywords {
        let name = keyword
            .arg
            .as_ref()
            .ok_or_else(|| CoreError::from("python **kwargs is not supported"))?;
        let value = lower_expr(&keyword.value)?;
        kwargs.push(ExprKwArg {
            name: name.as_str().to_string(),
            value,
        });
    }

    Ok(Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Span::null(),
        target: ExprInvokeTarget::expr(func),
        args,
        kwargs,
    })))
}

fn lower_bool_op(bool_op: &py_ast::ExprBoolOp<TextRange>) -> CoreResult<Expr> {
    if bool_op.values.is_empty() {
        return Err(CoreError::from("python boolean op has no operands"));
    }
    let kind = match bool_op.op {
        PyBoolOp::And => BinOpKind::And,
        PyBoolOp::Or => BinOpKind::Or,
    };

    let mut iter = bool_op.values.iter();
    let first = lower_expr(iter.next().expect("non-empty"))?;
    iter.try_fold(first, |acc, next| {
        Ok(Expr::new(ExprKind::BinOp(ExprBinOp {
            span: Span::null(),
            kind,
            lhs: Box::new(acc),
            rhs: Box::new(lower_expr(next)?),
        })))
    })
}

fn lower_compare(compare: &py_ast::ExprCompare<TextRange>) -> CoreResult<Expr> {
    if compare.ops.len() != 1 || compare.comparators.len() != 1 {
        return Err(CoreError::from(
            "python chained comparisons are not supported",
        ));
    }
    let op = &compare.ops[0];
    let kind = match op {
        PyCmpOp::Eq => BinOpKind::Eq,
        PyCmpOp::NotEq => BinOpKind::Ne,
        PyCmpOp::Lt => BinOpKind::Lt,
        PyCmpOp::LtE => BinOpKind::Le,
        PyCmpOp::Gt => BinOpKind::Gt,
        PyCmpOp::GtE => BinOpKind::Ge,
        _ => return Err(CoreError::from("python comparison operator unsupported")),
    };
    Ok(Expr::new(ExprKind::BinOp(ExprBinOp {
        span: Span::null(),
        kind,
        lhs: Box::new(lower_expr(&compare.left)?),
        rhs: Box::new(lower_expr(&compare.comparators[0])?),
    })))
}

fn lower_operator(op: &PyOperator) -> CoreResult<BinOpKind> {
    match op {
        PyOperator::Add => Ok(BinOpKind::Add),
        PyOperator::Sub => Ok(BinOpKind::Sub),
        PyOperator::Mult => Ok(BinOpKind::Mul),
        PyOperator::Div => Ok(BinOpKind::Div),
        PyOperator::Mod => Ok(BinOpKind::Mod),
        PyOperator::LShift => Ok(BinOpKind::Shl),
        PyOperator::RShift => Ok(BinOpKind::Shr),
        PyOperator::BitOr => Ok(BinOpKind::BitOr),
        PyOperator::BitXor => Ok(BinOpKind::BitXor),
        PyOperator::BitAnd => Ok(BinOpKind::BitAnd),
        _ => Err(CoreError::from("python operator not supported")),
    }
}

fn lower_unary_op(op: &PyUnaryOp) -> CoreResult<UnOpKind> {
    match op {
        PyUnaryOp::Not => Ok(UnOpKind::Not),
        PyUnaryOp::USub => Ok(UnOpKind::Neg),
        _ => Err(CoreError::from("python unary operator not supported")),
    }
}

fn lower_constant(constant: &PyConstant) -> CoreResult<Expr> {
    let value = match constant {
        PyConstant::None => Value::null(),
        PyConstant::Bool(value) => Value::bool(*value),
        PyConstant::Str(value) => Value::string(value.clone()),
        PyConstant::Bytes(bytes) => Value::Bytes(ValueBytes::from(bytes.as_slice())),
        PyConstant::Int(value) => lower_bigint(value)?,
        PyConstant::Float(value) => Value::decimal(*value),
        PyConstant::Tuple(values) => {
            let values = values
                .iter()
                .map(|item| lower_constant(item))
                .map(|expr| expr.map(Value::expr))
                .collect::<CoreResult<Vec<_>>>()?;
            Value::Tuple(ValueTuple::new(values))
        }
        _ => return Err(CoreError::from("python constant type is not supported")),
    };
    Ok(Expr::value(value))
}

fn lower_bigint(value: &py_ast::bigint::BigInt) -> CoreResult<Value> {
    let text = value.to_string();
    let parsed =
        NumBigInt::from_str(&text).map_err(|_| CoreError::from("failed to parse python int"))?;
    if let Some(i) = parsed.to_i64() {
        Ok(Value::int(i))
    } else {
        Ok(Value::big_int(parsed))
    }
}

fn lower_joined_str(joined: &py_ast::ExprJoinedStr<TextRange>) -> CoreResult<Expr> {
    let mut parts = Vec::new();
    let mut args = Vec::new();

    for value in &joined.values {
        match value {
            PyExpr::Constant(constant) => match &constant.value {
                PyConstant::Str(text) => parts.push(FormatTemplatePart::Literal(text.clone())),
                _ => return Err(CoreError::from("python f-string literals must be strings")),
            },
            PyExpr::FormattedValue(formatted) => {
                let expr = lower_formatted_value(formatted)?;
                let index = args.len();
                parts.push(FormatTemplatePart::Placeholder(
                    fp_core::ast::FormatPlaceholder {
                        arg_ref: fp_core::ast::FormatArgRef::Positional(index),
                        format_spec: None,
                    },
                ));
                args.push(expr);
            }
            _ => return Err(CoreError::from("python f-string element unsupported")),
        }
    }

    let template_expr = Expr::new(ExprKind::FormatString(ExprStringTemplate { parts }));
    let mut call_args = Vec::with_capacity(args.len() + 1);
    call_args.push(template_expr);
    call_args.extend(args);

    Ok(Expr::new(ExprKind::IntrinsicCall(ExprIntrinsicCall {
        span: Span::null(),
        kind: IntrinsicCallKind::Format,
        args: call_args,
        kwargs: Vec::new(),
    })))
}

fn lower_formatted_value(formatted: &py_ast::ExprFormattedValue<TextRange>) -> CoreResult<Expr> {
    if formatted.format_spec.is_some() {
        return Err(CoreError::from(
            "python f-string format spec is not supported",
        ));
    }
    if formatted.conversion != py_ast::ConversionFlag::None {
        return Err(CoreError::from(
            "python f-string conversion is not supported",
        ));
    }
    lower_expr(&formatted.value)
}

fn lower_import(names: &[PyAlias]) -> CoreResult<fp_core::ast::ItemImport> {
    let mut items = Vec::new();
    for alias in names {
        items.push(lower_import_alias(alias)?);
    }

    let tree = if items.len() == 1 {
        items.remove(0)
    } else {
        fp_core::ast::ItemImportTree::Group(fp_core::ast::ItemImportGroup { items })
    };

    Ok(fp_core::ast::ItemImport {
        attrs: Vec::new(),
        visibility: fp_core::ast::Visibility::Public,
        tree,
    })
}

fn lower_import_from(
    import: &py_ast::StmtImportFrom<TextRange>,
) -> CoreResult<fp_core::ast::ItemImport> {
    if import.level.is_some() {
        return Err(CoreError::from("python relative imports are not supported"));
    }
    let module = import
        .module
        .as_ref()
        .ok_or_else(|| CoreError::from("python import-from missing module"))?;

    let base_segments = module
        .as_str()
        .split('.')
        .map(|seg| fp_core::ast::ItemImportTree::Ident(Ident::new(seg)))
        .collect::<Vec<_>>();

    let mut imported = Vec::new();
    for alias in &import.names {
        imported.push(lower_import_alias(alias)?);
    }

    let mut path = fp_core::ast::ItemImportPath::new();
    for seg in base_segments {
        path.push(seg);
    }

    if imported.len() == 1 {
        path.push(imported.remove(0));
    } else {
        path.push(fp_core::ast::ItemImportTree::Group(
            fp_core::ast::ItemImportGroup { items: imported },
        ));
    }

    Ok(fp_core::ast::ItemImport {
        attrs: Vec::new(),
        visibility: fp_core::ast::Visibility::Public,
        tree: fp_core::ast::ItemImportTree::Path(path),
    })
}

fn lower_import_alias(alias: &PyAlias) -> CoreResult<fp_core::ast::ItemImportTree> {
    let name = alias.name.as_str();
    if name == "*" {
        return Ok(fp_core::ast::ItemImportTree::Glob);
    }

    let segments = name
        .split('.')
        .map(|seg| fp_core::ast::ItemImportTree::Ident(Ident::new(seg)))
        .collect::<Vec<_>>();

    let mut path = fp_core::ast::ItemImportPath::new();
    for seg in segments {
        path.push(seg);
    }

    if let Some(asname) = &alias.asname {
        if let Some(last) = path.segments.pop() {
            let from = match last {
                fp_core::ast::ItemImportTree::Ident(ident) => ident,
                _ => return Err(CoreError::from("python import rename expects identifier")),
            };
            let rename = fp_core::ast::ItemImportRename {
                from,
                to: Ident::new(asname.as_str()),
            };
            path.push(fp_core::ast::ItemImportTree::Rename(rename));
        }
    }

    if path.segments.len() == 1 {
        Ok(path.segments.remove(0))
    } else {
        Ok(fp_core::ast::ItemImportTree::Path(path))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_function_and_call() {
        let source = "def add(a, b):\n    return a + b\n\nresult = add(1, 2)\n";
        let frontend = PythonFrontend::new();
        let result = frontend.parse(source, None).expect("parse python");
        let fp_core::ast::NodeKind::File(file) = result.ast.kind() else {
            panic!("expected file node");
        };
        assert!(matches!(file.items[0].kind(), ItemKind::DefFunction(_)));
        assert!(matches!(file.items[1].kind(), ItemKind::Expr(_)));
    }

    #[test]
    fn parses_f_string() {
        let source = "message = f\"hi {name}\"";
        let frontend = PythonFrontend::new();
        let result = frontend.parse(source, None).expect("parse python");
        let fp_core::ast::NodeKind::File(file) = result.ast.kind() else {
            panic!("expected file node");
        };
        let ItemKind::Expr(expr) = file.items[0].kind() else {
            panic!("expected expr item");
        };
        let ExprKind::Assign(assign) = expr.kind() else {
            panic!("expected assignment expr");
        };
        let ExprKind::IntrinsicCall(call) = assign.value.kind() else {
            panic!("expected format intrinsic");
        };
        assert!(matches!(call.kind, IntrinsicCallKind::Format));
    }
}
