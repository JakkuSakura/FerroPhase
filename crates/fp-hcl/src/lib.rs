//! FerroPhase frontend for HCL documents.

use std::path::Path;
use std::sync::Arc;

use fp_core::ast::{
    AstSerializer, Expr, ExprArray, ExprBinOp, ExprIf, ExprIndex, ExprIntrinsicCall, ExprInvoke,
    ExprInvokeTarget, ExprKind, ExprParen, ExprSelect, ExprSelectType, ExprStringTemplate,
    ExprUnOp, FormatArgRef, FormatPlaceholder, FormatTemplatePart, Node, Path as AstPath, Value,
    ValueList, ValueMap,
};
use fp_core::ast::{Ident, Name};
use fp_core::diagnostics::DiagnosticManager;
use fp_core::error::{Error as CoreError, Result as CoreResult};
use fp_core::frontend::{FrontendResult, FrontendSnapshot, LanguageFrontend};
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::{BinOpKind, UnOpKind};
use fp_core::span::Span;
use hcl::expr::{
    BinaryOp, BinaryOperator, Conditional, FuncCall, FuncName, ObjectKey, Operation, TemplateExpr,
    Traversal, TraversalOperator, UnaryOp, UnaryOperator,
};
use hcl::template::{Element as TemplateElement, Template};
use hcl::{Attribute, Block, BlockLabel, Body, Expression, Structure, Value as HclValue};

/// Canonical identifier for the HCL frontend.
pub const HCL: &str = "hcl";

/// Frontend that converts HCL documents into FerroPhase values.
#[derive(Debug, Default, Clone)]
pub struct HclFrontend;

impl HclFrontend {
    pub fn new() -> Self {
        Self
    }

    fn build_value(&self, source: &str) -> CoreResult<Value> {
        let body: Body = hcl::parse(source).map_err(|err| CoreError::from(err.to_string()))?;
        hcl_body_to_value(&body)
    }
}

impl LanguageFrontend for HclFrontend {
    fn language(&self) -> &'static str {
        HCL
    }

    fn extensions(&self) -> &'static [&'static str] {
        &["hcl"]
    }

    fn parse(&self, source: &str, path: Option<&Path>) -> CoreResult<FrontendResult> {
        let diagnostics = Arc::new(DiagnosticManager::new());
        let value = self.build_value(source)?;
        let expr = Expr::new(ExprKind::Value(Box::new(value.clone())));

        let serializer: Arc<dyn AstSerializer> = Arc::new(HclSerializer);
        let serialized = serializer.serialize_value(&value).ok();
        let description = match path {
            Some(path) => format!("HCL document {}", path.display()),
            None => "HCL document <stdin>".to_string(),
        };
        let snapshot = FrontendSnapshot {
            language: self.language().to_string(),
            description,
            serialized,
        };

        let node = Node::expr(expr);

        Ok(FrontendResult {
            last: node.clone(),
            ast: node,
            serializer,
            intrinsic_normalizer: None,
            macro_parser: None,
            snapshot: Some(snapshot),
            diagnostics,
        })
    }
}

struct HclSerializer;

impl AstSerializer for HclSerializer {
    fn serialize_expr(&self, node: &Expr) -> CoreResult<String> {
        match node.kind() {
            ExprKind::Value(value) => self.serialize_value(value),
            _ => Err(CoreError::from("hcl serializer expects value expression")),
        }
    }

    fn serialize_value(&self, node: &Value) -> CoreResult<String> {
        if let Ok(body) = value_to_hcl_body(node) {
            return hcl::to_string(&body).map_err(|err| CoreError::from(err.to_string()));
        }

        let value = value_to_hcl_value(node)?;
        hcl::to_string(&value).map_err(|err| CoreError::from(err.to_string()))
    }
}

fn hcl_body_to_value(body: &Body) -> CoreResult<Value> {
    let mut attributes = ValueMap::new();
    let mut blocks = Vec::new();

    for structure in &body.0 {
        match structure {
            Structure::Attribute(attribute) => {
                let name = attribute.key.as_str().to_string();
                let expr = lower_expression(&attribute.expr)?;
                attributes.insert(Value::string(name), Value::expr(expr));
            }
            Structure::Block(block) => {
                blocks.push(block_to_value(block)?);
            }
        }
    }

    let mut entries = Vec::new();
    entries.push((
        Value::string("attributes".to_string()),
        Value::Map(attributes),
    ));
    entries.push((
        Value::string("blocks".to_string()),
        Value::List(ValueList::new(blocks)),
    ));

    Ok(Value::Map(ValueMap::from_pairs(entries)))
}

fn block_to_value(block: &Block) -> CoreResult<Value> {
    let identifier = Value::string(block.identifier.as_str().to_string());
    let labels = block
        .labels
        .iter()
        .map(block_label_to_value)
        .collect::<Vec<_>>();
    let body = hcl_body_to_value(&block.body)?;

    let mut entries = Vec::new();
    entries.push((Value::string("identifier".to_string()), identifier));
    entries.push((
        Value::string("labels".to_string()),
        Value::List(ValueList::new(labels)),
    ));
    entries.push((Value::string("body".to_string()), body));

    Ok(Value::Map(ValueMap::from_pairs(entries)))
}

fn block_label_to_value(label: &BlockLabel) -> Value {
    match label {
        BlockLabel::Identifier(ident) => Value::string(ident.as_str().to_string()),
        BlockLabel::String(text) => Value::string(text.clone()),
    }
}

fn lower_expression(expr: &Expression) -> CoreResult<Expr> {
    match expr {
        Expression::Null => Ok(Expr::value(Value::null())),
        Expression::Bool(value) => Ok(Expr::value(Value::bool(*value))),
        Expression::Number(number) => Ok(Expr::value(lower_number(number))),
        Expression::String(text) => Ok(Expr::value(Value::string(text.clone()))),
        Expression::Array(items) => {
            let values = items
                .iter()
                .map(lower_expression)
                .collect::<CoreResult<Vec<_>>>()?;
            Ok(Expr::new(ExprKind::Array(ExprArray {
                span: Span::null(),
                values,
            })))
        }
        Expression::Object(object) => {
            let mut entries = Vec::with_capacity(object.len());
            for (key, value) in object {
                let key_value = match key {
                    ObjectKey::Identifier(ident) => Value::string(ident.as_str().to_string()),
                    ObjectKey::Expression(expr) => Value::expr(lower_expression(expr)?),
                    _ => return Err(CoreError::from("unsupported hcl object key expression")),
                };
                let value_expr = lower_expression(value)?;
                entries.push((key_value, Value::expr(value_expr)));
            }
            Ok(Expr::value(Value::Map(ValueMap::from_pairs(entries))))
        }
        Expression::TemplateExpr(template) => lower_template_expr(template),
        Expression::Variable(var) => Ok(Expr::name(Name::ident(var.as_str()))),
        Expression::Traversal(traversal) => lower_traversal(traversal),
        Expression::FuncCall(call) => lower_func_call(call),
        Expression::Parenthesis(inner) => Ok(Expr::new(ExprKind::Paren(ExprParen {
            span: Span::null(),
            expr: Box::new(lower_expression(inner)?),
        }))),
        Expression::Conditional(cond) => Ok(Expr::new(ExprKind::If(ExprIf {
            span: Span::null(),
            cond: Box::new(lower_expression(&cond.cond_expr)?),
            then: Box::new(lower_expression(&cond.true_expr)?),
            elze: Some(Box::new(lower_expression(&cond.false_expr)?)),
        }))),
        Expression::Operation(operation) => lower_operation(operation),
        Expression::ForExpr(_) => Err(CoreError::from("hcl for-expressions are not supported yet")),
        _ => Err(CoreError::from("unsupported hcl expression")),
    }
}

fn lower_number(number: &hcl::Number) -> Value {
    if let Some(value) = number.as_i64() {
        Value::int(value)
    } else if let Some(value) = number.as_u64() {
        if value <= i64::MAX as u64 {
            Value::int(value as i64)
        } else {
            Value::decimal(number.as_f64().unwrap_or(0.0))
        }
    } else {
        Value::decimal(number.as_f64().unwrap_or(0.0))
    }
}

fn lower_template_expr(template_expr: &TemplateExpr) -> CoreResult<Expr> {
    let template =
        Template::from_expr(template_expr).map_err(|err| CoreError::from(err.to_string()))?;

    let mut parts = Vec::new();
    let mut args = Vec::new();

    for element in template.elements() {
        match element {
            TemplateElement::Literal(text) => {
                parts.push(FormatTemplatePart::Literal(text.clone()));
            }
            TemplateElement::Interpolation(interpolation) => {
                let expr = lower_expression(&interpolation.expr)?;
                let index = args.len();
                parts.push(FormatTemplatePart::Placeholder(FormatPlaceholder {
                    arg_ref: FormatArgRef::Positional(index),
                    format_spec: None,
                }));
                args.push(expr);
            }
            TemplateElement::Directive(_) => {
                return Err(CoreError::from("hcl template directives are not supported"));
            }
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

fn lower_traversal(traversal: &Traversal) -> CoreResult<Expr> {
    let mut current = lower_expression(&traversal.expr)?;

    for op in &traversal.operators {
        current = match op {
            TraversalOperator::GetAttr(ident) => Expr::new(ExprKind::Select(ExprSelect {
                span: Span::null(),
                obj: Box::new(current),
                field: Ident::new(ident.as_str()),
                select: ExprSelectType::Field,
            })),
            TraversalOperator::Index(expr) => Expr::new(ExprKind::Index(ExprIndex {
                span: Span::null(),
                obj: Box::new(current),
                index: Box::new(lower_expression(expr)?),
            })),
            TraversalOperator::LegacyIndex(index) => Expr::new(ExprKind::Index(ExprIndex {
                span: Span::null(),
                obj: Box::new(current),
                index: Box::new(Expr::value(Value::int(*index as i64))),
            })),
            TraversalOperator::AttrSplat | TraversalOperator::FullSplat => {
                return Err(CoreError::from("hcl splat traversals are not supported"));
            }
        };
    }

    Ok(current)
}

fn lower_func_call(call: &FuncCall) -> CoreResult<Expr> {
    if call.expand_final {
        return Err(CoreError::from(
            "hcl function calls with argument expansion are not supported",
        ));
    }

    let locator = func_name_to_locator(&call.name);
    let args = call
        .args
        .iter()
        .map(lower_expression)
        .collect::<CoreResult<Vec<_>>>()?;

    Ok(Expr::new(ExprKind::Invoke(ExprInvoke {
        span: Span::null(),
        target: ExprInvokeTarget::Function(locator),
        args,
        kwargs: Vec::new(),
    })))
}

fn func_name_to_locator(name: &FuncName) -> Name {
    let mut segments = Vec::new();
    for namespace in &name.namespace {
        segments.push(Ident::new(namespace.as_str()));
    }
    segments.push(Ident::new(name.name.as_str()));

    if segments.len() == 1 {
        Name::from_ident(segments.remove(0))
    } else {
        Name::path(AstPath::plain(segments))
    }
}

fn lower_operation(operation: &Operation) -> CoreResult<Expr> {
    match operation {
        Operation::Unary(op) => lower_unary_op(op),
        Operation::Binary(op) => lower_binary_op(op),
    }
}

fn lower_unary_op(op: &UnaryOp) -> CoreResult<Expr> {
    let kind = match op.operator {
        UnaryOperator::Neg => UnOpKind::Neg,
        UnaryOperator::Not => UnOpKind::Not,
    };
    Ok(Expr::new(ExprKind::UnOp(ExprUnOp {
        span: Span::null(),
        op: kind,
        val: Box::new(lower_expression(&op.expr)?),
    })))
}

fn lower_binary_op(op: &BinaryOp) -> CoreResult<Expr> {
    let kind = match op.operator {
        BinaryOperator::Eq => BinOpKind::Eq,
        BinaryOperator::NotEq => BinOpKind::Ne,
        BinaryOperator::LessEq => BinOpKind::Le,
        BinaryOperator::GreaterEq => BinOpKind::Ge,
        BinaryOperator::Less => BinOpKind::Lt,
        BinaryOperator::Greater => BinOpKind::Gt,
        BinaryOperator::Plus => BinOpKind::Add,
        BinaryOperator::Minus => BinOpKind::Sub,
        BinaryOperator::Mul => BinOpKind::Mul,
        BinaryOperator::Div => BinOpKind::Div,
        BinaryOperator::Mod => BinOpKind::Mod,
        BinaryOperator::And => BinOpKind::And,
        BinaryOperator::Or => BinOpKind::Or,
    };

    Ok(Expr::new(ExprKind::BinOp(ExprBinOp {
        span: Span::null(),
        kind,
        lhs: Box::new(lower_expression(&op.lhs_expr)?),
        rhs: Box::new(lower_expression(&op.rhs_expr)?),
    })))
}

fn value_to_hcl_body(value: &Value) -> CoreResult<Body> {
    let Value::Map(map) = value else {
        return Err(CoreError::from("hcl serializer expects map value"));
    };

    let attributes = value_map_get(map, "attributes")
        .ok_or_else(|| CoreError::from("missing attributes section"))?;
    let blocks =
        value_map_get(map, "blocks").ok_or_else(|| CoreError::from("missing blocks section"))?;

    let attributes_map = match attributes {
        Value::Map(map) => map,
        _ => return Err(CoreError::from("attributes must be a map")),
    };
    let blocks_list = match blocks {
        Value::List(list) => list,
        _ => return Err(CoreError::from("blocks must be a list")),
    };

    let mut structures = Vec::new();

    for entry in &attributes_map.entries {
        let key = match &entry.key {
            Value::String(text) => text.value.clone(),
            _ => {
                return Err(CoreError::from(
                    "attribute keys must be strings in serialized body",
                ));
            }
        };
        let ident = hcl::Identifier::new(key).map_err(|err| CoreError::from(err.to_string()))?;
        let expr = value_to_hcl_expression(&entry.value)?;
        structures.push(Structure::Attribute(Attribute { key: ident, expr }));
    }

    for block_value in &blocks_list.values {
        structures.push(Structure::Block(value_to_hcl_block(block_value)?));
    }

    Ok(Body(structures))
}

fn value_to_hcl_block(value: &Value) -> CoreResult<Block> {
    let Value::Map(map) = value else {
        return Err(CoreError::from("block entry must be a map"));
    };

    let identifier = value_map_get(map, "identifier")
        .ok_or_else(|| CoreError::from("block identifier missing"))?;
    let labels =
        value_map_get(map, "labels").ok_or_else(|| CoreError::from("block labels missing"))?;
    let body = value_map_get(map, "body").ok_or_else(|| CoreError::from("block body missing"))?;

    let identifier = match identifier {
        Value::String(text) => text.value.clone(),
        _ => return Err(CoreError::from("block identifier must be a string")),
    };
    let identifier =
        hcl::Identifier::new(identifier).map_err(|err| CoreError::from(err.to_string()))?;

    let labels = match labels {
        Value::List(list) => list,
        _ => return Err(CoreError::from("block labels must be a list")),
    };

    let mut block_labels = Vec::new();
    for label in &labels.values {
        let text = match label {
            Value::String(text) => text.value.clone(),
            _ => return Err(CoreError::from("block label must be a string")),
        };
        if let Ok(ident) = hcl::Identifier::new(text.clone()) {
            block_labels.push(BlockLabel::Identifier(ident));
        } else {
            block_labels.push(BlockLabel::String(text));
        }
    }

    let body = value_to_hcl_body(body)?;

    Ok(Block {
        identifier,
        labels: block_labels,
        body,
    })
}

fn value_to_hcl_expression(value: &Value) -> CoreResult<Expression> {
    match value {
        Value::Expr(expr) => expr_to_hcl_expression(expr),
        other => value_to_hcl_expression_from_value(other),
    }
}

fn value_to_hcl_expression_from_value(value: &Value) -> CoreResult<Expression> {
    match value {
        Value::List(list) => {
            let items = list
                .values
                .iter()
                .map(value_to_hcl_expression)
                .collect::<CoreResult<Vec<_>>>()?;
            Ok(Expression::Array(items))
        }
        Value::Map(map) => {
            let object = value_map_to_hcl_object(map)?;
            Ok(Expression::Object(object))
        }
        _ => {
            let value = value_to_hcl_value(value)?;
            Ok(Expression::from(value))
        }
    }
}

fn value_map_to_hcl_object(map: &ValueMap) -> CoreResult<hcl::Object<ObjectKey, Expression>> {
    let mut object = hcl::Object::new();
    for entry in &map.entries {
        let key = match &entry.key {
            Value::String(text) => {
                if let Ok(ident) = hcl::Identifier::new(text.value.clone()) {
                    ObjectKey::Identifier(ident)
                } else {
                    ObjectKey::Expression(Expression::String(text.value.clone()))
                }
            }
            Value::Expr(expr) => ObjectKey::Expression(expr_to_hcl_expression(expr)?),
            _ => {
                return Err(CoreError::from(
                    "object keys must be strings or expressions",
                ));
            }
        };
        let value = value_to_hcl_expression(&entry.value)?;
        object.insert(key, value);
    }
    Ok(object)
}

fn value_to_hcl_value(value: &Value) -> CoreResult<HclValue> {
    match value {
        Value::Null(_) => Ok(HclValue::Null),
        Value::Bool(value) => Ok(HclValue::Bool(value.value)),
        Value::Int(value) => Ok(HclValue::Number(hcl::Number::from(value.value))),
        Value::Decimal(value) => {
            let number = hcl::Number::from_f64(value.value).ok_or_else(|| {
                CoreError::from("hcl serializer does not support NaN or infinity")
            })?;
            Ok(HclValue::Number(number))
        }
        Value::String(value) => Ok(HclValue::String(value.value.clone())),
        Value::List(list) => {
            let values = list
                .values
                .iter()
                .map(value_to_hcl_value)
                .collect::<CoreResult<Vec<_>>>()?;
            Ok(HclValue::Array(values))
        }
        Value::Map(map) => {
            let mut object = hcl::Map::new();
            for entry in &map.entries {
                let key = match &entry.key {
                    Value::String(text) => text.value.clone(),
                    _ => return Err(CoreError::from("hcl value serializer expects string keys")),
                };
                let value = value_to_hcl_value(&entry.value)?;
                object.insert(key, value);
            }
            Ok(HclValue::Object(object))
        }
        _ => Err(CoreError::from(
            "hcl serializer does not support this value",
        )),
    }
}

fn expr_to_hcl_expression(expr: &Expr) -> CoreResult<Expression> {
    match expr.kind() {
        ExprKind::Select(_) | ExprKind::Index(_) => {
            let mut operators = Vec::new();
            let base = peel_traversal(expr, &mut operators)?;
            operators.reverse();
            Ok(Expression::Traversal(Box::new(Traversal {
                expr: base,
                operators,
            })))
        }
        _ => expr_to_hcl_expression_base(expr),
    }
}

fn peel_traversal(expr: &Expr, operators: &mut Vec<TraversalOperator>) -> CoreResult<Expression> {
    match expr.kind() {
        ExprKind::Select(select) => {
            let ident = hcl::Identifier::new(select.field.to_string())
                .map_err(|err| CoreError::from(err.to_string()))?;
            operators.push(TraversalOperator::GetAttr(ident));
            peel_traversal(&select.obj, operators)
        }
        ExprKind::Index(index) => {
            let expr = expr_to_hcl_expression(&index.index)?;
            operators.push(TraversalOperator::Index(expr));
            peel_traversal(&index.obj, operators)
        }
        _ => expr_to_hcl_expression_base(expr),
    }
}

fn expr_to_hcl_expression_base(expr: &Expr) -> CoreResult<Expression> {
    match expr.kind() {
        ExprKind::Value(value) => value_to_hcl_expression_from_value(value),
        ExprKind::Name(locator) => {
            let name = locator.to_string();
            let var = hcl::expr::Variable::new(name.as_str())
                .map_err(|err| CoreError::from(err.to_string()))?;
            Ok(Expression::Variable(var))
        }
        ExprKind::Invoke(call) => expr_invoke_to_hcl_expression(call),
        ExprKind::BinOp(bin) => {
            let op = BinaryOp {
                lhs_expr: expr_to_hcl_expression(&bin.lhs)?,
                operator: bin_op_kind_to_hcl(bin.kind)?,
                rhs_expr: expr_to_hcl_expression(&bin.rhs)?,
            };
            Ok(Expression::Operation(Box::new(Operation::Binary(op))))
        }
        ExprKind::UnOp(unary) => {
            let op = UnaryOp {
                operator: un_op_kind_to_hcl(unary.op.clone())?,
                expr: expr_to_hcl_expression(&unary.val)?,
            };
            Ok(Expression::Operation(Box::new(Operation::Unary(op))))
        }
        ExprKind::If(expr_if) => {
            let cond = Conditional {
                cond_expr: expr_to_hcl_expression(&expr_if.cond)?,
                true_expr: expr_to_hcl_expression(&expr_if.then)?,
                false_expr: expr_to_hcl_expression(
                    expr_if
                        .elze
                        .as_ref()
                        .ok_or_else(|| CoreError::from("if expression missing else"))?,
                )?,
            };
            Ok(Expression::Conditional(Box::new(cond)))
        }
        ExprKind::Paren(paren) => Ok(Expression::Parenthesis(Box::new(expr_to_hcl_expression(
            &paren.expr,
        )?))),
        ExprKind::Array(array) => {
            let values = array
                .values
                .iter()
                .map(expr_to_hcl_expression)
                .collect::<CoreResult<Vec<_>>>()?;
            Ok(Expression::Array(values))
        }
        other => Err(CoreError::from(format!(
            "cannot serialize expression to hcl: {other:?}"
        ))),
    }
}

fn expr_invoke_to_hcl_expression(call: &ExprInvoke) -> CoreResult<Expression> {
    if !call.kwargs.is_empty() {
        return Err(CoreError::from(
            "hcl does not support keyword arguments in function calls",
        ));
    }

    let locator = match &call.target {
        ExprInvokeTarget::Function(locator) => locator,
        _ => {
            return Err(CoreError::from(
                "hcl serialization only supports function call targets",
            ));
        }
    };

    let func_name = locator_to_func_name(locator)?;
    let args = call
        .args
        .iter()
        .map(expr_to_hcl_expression)
        .collect::<CoreResult<Vec<_>>>()?;

    Ok(Expression::FuncCall(Box::new(FuncCall {
        name: func_name,
        args,
        expand_final: false,
    })))
}

fn locator_to_func_name(locator: &Name) -> CoreResult<FuncName> {
    let path = locator.to_path();
    let mut segments = path.segments.iter();
    let first = segments
        .next()
        .ok_or_else(|| CoreError::from("empty function name"))?;
    let mut namespace = Vec::new();

    if let Some(last) = path.segments.last() {
        for segment in &path.segments[..path.segments.len().saturating_sub(1)] {
            namespace.push(
                hcl::Identifier::new(segment.as_str())
                    .map_err(|err| CoreError::from(err.to_string()))?,
            );
        }
        let name =
            hcl::Identifier::new(last.as_str()).map_err(|err| CoreError::from(err.to_string()))?;
        Ok(FuncName { namespace, name })
    } else {
        let name =
            hcl::Identifier::new(first.as_str()).map_err(|err| CoreError::from(err.to_string()))?;
        Ok(FuncName::new(name))
    }
}

fn bin_op_kind_to_hcl(kind: BinOpKind) -> CoreResult<BinaryOperator> {
    match kind {
        BinOpKind::Eq => Ok(BinaryOperator::Eq),
        BinOpKind::Ne => Ok(BinaryOperator::NotEq),
        BinOpKind::Lt => Ok(BinaryOperator::Less),
        BinOpKind::Gt => Ok(BinaryOperator::Greater),
        BinOpKind::Le => Ok(BinaryOperator::LessEq),
        BinOpKind::Ge => Ok(BinaryOperator::GreaterEq),
        BinOpKind::Add => Ok(BinaryOperator::Plus),
        BinOpKind::Sub => Ok(BinaryOperator::Minus),
        BinOpKind::Mul => Ok(BinaryOperator::Mul),
        BinOpKind::Div => Ok(BinaryOperator::Div),
        BinOpKind::Mod => Ok(BinaryOperator::Mod),
        BinOpKind::And => Ok(BinaryOperator::And),
        BinOpKind::Or => Ok(BinaryOperator::Or),
        _ => Err(CoreError::from("unsupported binary operator for hcl")),
    }
}

fn un_op_kind_to_hcl(kind: UnOpKind) -> CoreResult<UnaryOperator> {
    match kind {
        UnOpKind::Neg => Ok(UnaryOperator::Neg),
        UnOpKind::Not => Ok(UnaryOperator::Not),
        _ => Err(CoreError::from("unsupported unary operator for hcl")),
    }
}

fn value_map_get<'a>(map: &'a ValueMap, key: &str) -> Option<&'a Value> {
    map.entries.iter().find_map(|entry| match &entry.key {
        Value::String(text) if text.value == key => Some(&entry.value),
        _ => None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_attribute<'a>(root: &'a Value, key: &str) -> Option<&'a Value> {
        let Value::Map(map) = root else {
            return None;
        };
        let attributes = value_map_get(map, "attributes")?;
        let Value::Map(attributes) = attributes else {
            return None;
        };
        value_map_get(attributes, key)
    }

    #[test]
    fn parses_function_call_attribute() {
        let input = "result = sum(1, 2)";
        let frontend = HclFrontend::new();
        let result = frontend.parse(input, None).expect("parse hcl");
        let fp_core::ast::NodeKind::Expr(expr) = result.ast.kind() else {
            panic!("expected expr node");
        };
        let ExprKind::Value(value) = expr.kind() else {
            panic!("expected value expression");
        };
        let attr = get_attribute(value.as_ref(), "result").expect("attribute");
        let Value::Expr(expr) = attr else {
            panic!("expected expression value");
        };
        assert!(matches!(expr.kind(), ExprKind::Invoke(_)));
    }

    #[test]
    fn parses_template_interpolation() {
        let input = "message = \"hi ${name}\"";
        let frontend = HclFrontend::new();
        let result = frontend.parse(input, None).expect("parse hcl");
        let fp_core::ast::NodeKind::Expr(expr) = result.ast.kind() else {
            panic!("expected expr node");
        };
        let ExprKind::Value(value) = expr.kind() else {
            panic!("expected value expression");
        };
        let attr = get_attribute(value.as_ref(), "message").expect("attribute");
        let Value::Expr(expr) = attr else {
            panic!("expected expression value");
        };
        let ExprKind::IntrinsicCall(call) = expr.kind() else {
            panic!("expected intrinsic call");
        };
        assert!(matches!(call.kind, IntrinsicCallKind::Format));
        assert_eq!(call.args.len(), 2);
        assert!(matches!(call.args[0].kind(), ExprKind::FormatString(_)));
    }
}
