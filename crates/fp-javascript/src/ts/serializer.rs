use std::collections::{BTreeMap, HashMap};
use std::fmt::Write as _;
use std::sync::RwLock;

use fp_core::ast::{
    AstSerializer, BlockStmt, Expr, ExprIntrinsicCollection, ExprInvokeTarget, ExprKind,
    ExprStruct, Item, Node, NodeKind, Ty, TypeEnum, TypePrimitive, TypeStruct, Value,
};
use fp_core::intrinsics::IntrinsicCallPayload;

#[derive(Default)]
struct TypeScriptContext {
    structs: Vec<TypeStruct>,
    enums: Vec<TypeEnum>,
    constants: HashMap<String, Value>,
    struct_inits: Vec<String>,
}

pub struct TypeScriptSerializer {
    emit_type_defs: bool,
    type_defs: RwLock<Option<String>>,
}

impl TypeScriptSerializer {
    pub fn new(emit_type_defs: bool) -> Self {
        Self {
            emit_type_defs,
            type_defs: RwLock::new(None),
        }
    }

    pub fn take_type_defs(&self) -> Option<String> {
        self.type_defs.write().unwrap().take()
    }
}

impl AstSerializer for TypeScriptSerializer {
    fn serialize_node(&self, node: &Node) -> fp_core::error::Result<String> {
        let mut context = TypeScriptContext::default();
        collect_from_node(node, &mut context);
        let (code, defs) = render_typescript(&context, self.emit_type_defs);
        *self.type_defs.write().unwrap() = defs;
        Ok(code)
    }
}

pub struct JavaScriptSerializer;

impl AstSerializer for JavaScriptSerializer {
    fn serialize_node(&self, node: &Node) -> fp_core::error::Result<String> {
        let mut context = TypeScriptContext::default();
        collect_from_node(node, &mut context);
        Ok(render_javascript(&context))
    }
}

fn collect_from_node(node: &Node, context: &mut TypeScriptContext) {
    match node.kind() {
        NodeKind::File(file) => {
            for item in &file.items {
                collect_from_item(item, context);
            }
        }
        NodeKind::Item(item) => collect_from_item(item, context),
        NodeKind::Expr(expr) => collect_from_expr(expr, context),
    }
}

fn collect_from_expr(expr: &Expr, context: &mut TypeScriptContext) {
    match expr.kind() {
        ExprKind::Block(block) => {
            for stmt in &block.stmts {
                match stmt {
                    BlockStmt::Item(item) => collect_from_item(item.as_ref(), context),
                    BlockStmt::Let(let_stmt) => {
                        if let Some(init) = &let_stmt.init {
                            collect_from_expr(init, context);
                        }
                        if let Some(diverge) = &let_stmt.diverge {
                            collect_from_expr(diverge, context);
                        }
                    }
                    BlockStmt::Expr(inner) => collect_from_expr(inner.expr.as_ref(), context),
                    BlockStmt::Noop => {}
                    BlockStmt::Any(_) => {}
                }
            }
        }
        ExprKind::Struct(struct_expr) => {
            record_struct_literal(struct_expr, context);
            for field in &struct_expr.fields {
                if let Some(value) = &field.value {
                    collect_from_expr(value, context);
                }
            }
        }
        ExprKind::Let(expr_let) => {
            collect_from_expr(expr_let.expr.as_ref(), context);
        }
        ExprKind::Assign(assign) => {
            collect_from_expr(assign.target.as_ref(), context);
            collect_from_expr(assign.value.as_ref(), context);
        }
        ExprKind::Invoke(invoke) => {
            if let ExprInvokeTarget::Expr(inner) = &invoke.target {
                collect_from_expr(inner.as_ref(), context);
            }
            for arg in &invoke.args {
                collect_from_expr(arg, context);
            }
        }
        ExprKind::Select(select) => {
            collect_from_expr(select.obj.as_ref(), context);
        }
        ExprKind::IntrinsicCall(call) => match &call.payload {
            IntrinsicCallPayload::Format { template } => {
                for arg in &template.args {
                    collect_from_expr(arg, context);
                }
                for kwarg in &template.kwargs {
                    collect_from_expr(&kwarg.value, context);
                }
            }
            IntrinsicCallPayload::Args { args } => {
                for arg in args {
                    collect_from_expr(arg, context);
                }
            }
        },
        ExprKind::FormatString(format) => {
            for arg in &format.args {
                collect_from_expr(arg, context);
            }
            for kwarg in &format.kwargs {
                collect_from_expr(&kwarg.value, context);
            }
        }
        ExprKind::Tuple(tuple) => {
            for value in &tuple.values {
                collect_from_expr(value, context);
            }
        }
        ExprKind::Array(array) => {
            for value in &array.values {
                collect_from_expr(value, context);
            }
        }
        ExprKind::ArrayRepeat(repeat) => {
            collect_from_expr(repeat.elem.as_ref(), context);
            collect_from_expr(repeat.len.as_ref(), context);
        }
        ExprKind::Reference(reference) => {
            collect_from_expr(reference.referee.as_ref(), context);
        }
        ExprKind::Dereference(dereference) => {
            collect_from_expr(dereference.referee.as_ref(), context);
        }
        ExprKind::Paren(paren) => {
            collect_from_expr(paren.expr.as_ref(), context);
        }
        ExprKind::Splat(splat) => {
            collect_from_expr(splat.iter.as_ref(), context);
        }
        ExprKind::SplatDict(dict) => {
            collect_from_expr(dict.dict.as_ref(), context);
        }
        ExprKind::IntrinsicCollection(collection) => match collection {
            ExprIntrinsicCollection::VecElements { elements } => {
                for element in elements {
                    collect_from_expr(element, context);
                }
            }
            ExprIntrinsicCollection::VecRepeat { elem, len } => {
                collect_from_expr(elem.as_ref(), context);
                collect_from_expr(len.as_ref(), context);
            }
            ExprIntrinsicCollection::HashMapEntries { entries } => {
                for entry in entries {
                    collect_from_expr(&entry.key, context);
                    collect_from_expr(&entry.value, context);
                }
            }
        },
        ExprKind::Match(expr_match) => {
            for case in &expr_match.cases {
                collect_from_expr(case.cond.as_ref(), context);
                collect_from_expr(case.body.as_ref(), context);
            }
        }
        ExprKind::If(expr_if) => {
            collect_from_expr(expr_if.cond.as_ref(), context);
            collect_from_expr(expr_if.then.as_ref(), context);
            if let Some(elze) = &expr_if.elze {
                collect_from_expr(elze.as_ref(), context);
            }
        }
        ExprKind::While(expr_while) => {
            collect_from_expr(expr_while.cond.as_ref(), context);
            collect_from_expr(expr_while.body.as_ref(), context);
        }
        ExprKind::Loop(expr_loop) => {
            collect_from_expr(expr_loop.body.as_ref(), context);
        }
        ExprKind::Closure(closure) => {
            collect_from_expr(closure.body.as_ref(), context);
        }
        ExprKind::Try(expr_try) => {
            collect_from_expr(expr_try.expr.as_ref(), context);
        }
        ExprKind::Any(_) | ExprKind::Id(_) | ExprKind::Value(_) | ExprKind::Locator(_) => {}
        _ => {}
    }
}

fn collect_from_item(item: &Item, context: &mut TypeScriptContext) {
    if let Some(struct_def) = item.as_struct() {
        context.structs.push(struct_def.value.clone());
    } else if let Some(enum_def) = item.as_enum() {
        context.enums.push(enum_def.value.clone());
    }

    if let Some(const_def) = item.as_const() {
        if let ExprKind::Value(value) = const_def.value.as_ref().kind() {
            context
                .constants
                .insert(const_def.name.name.clone(), *value.clone());
        }
    }

    if let Some(expr) = item.as_expr() {
        collect_from_expr(expr, context);
    }
}

fn record_struct_literal(struct_expr: &ExprStruct, context: &mut TypeScriptContext) {
    if let Some(call) = build_struct_factory_call(struct_expr) {
        if !context
            .struct_inits
            .iter()
            .any(|existing| existing == &call)
        {
            context.struct_inits.push(call);
        }
    }
}

fn build_struct_factory_call(struct_expr: &ExprStruct) -> Option<String> {
    let struct_name = extract_struct_name(struct_expr.name.as_ref())?;
    let mut args = Vec::with_capacity(struct_expr.fields.len());
    for field in &struct_expr.fields {
        let rendered = field
            .value
            .as_ref()
            .and_then(render_literal_expr)
            .unwrap_or_else(|| "undefined".to_string());
        args.push(rendered);
    }
    Some(format!("create{}({})", struct_name, args.join(", ")))
}

fn extract_struct_name(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Locator(locator) => Some(locator.to_string()),
        _ => None,
    }
}

fn render_literal_expr(expr: &Expr) -> Option<String> {
    match expr.kind() {
        ExprKind::Value(value) => Some(render_js_value(value.as_ref())),
        ExprKind::Struct(struct_expr) => build_struct_factory_call(struct_expr),
        ExprKind::Tuple(tuple) => {
            let mut rendered = Vec::with_capacity(tuple.values.len());
            for value in &tuple.values {
                rendered.push(render_literal_expr(value)?);
            }
            Some(format!("[{}]", rendered.join(", ")))
        }
        ExprKind::Array(array) => {
            let mut rendered = Vec::with_capacity(array.values.len());
            for value in &array.values {
                rendered.push(render_literal_expr(value)?);
            }
            Some(format!("[{}]", rendered.join(", ")))
        }
        ExprKind::ArrayRepeat(repeat) => {
            let elem = render_literal_expr(repeat.elem.as_ref())?;
            let len = render_literal_expr(repeat.len.as_ref())?;
            Some(format!("Array({}).fill({})", len, elem))
        }
        ExprKind::Select(select) => {
            let base = render_literal_expr(select.obj.as_ref())?;
            Some(format!("{}.{}", base, select.field.name))
        }
        ExprKind::Locator(locator) => Some(locator.to_string()),
        ExprKind::Paren(paren) => render_literal_expr(paren.expr.as_ref()),
        ExprKind::Block(block) => {
            let mut last = None;
            for stmt in &block.stmts {
                if let BlockStmt::Expr(expr_stmt) = stmt {
                    last = render_literal_expr(expr_stmt.expr.as_ref());
                }
            }
            last
        }
        _ => None,
    }
}

fn render_typescript(context: &TypeScriptContext, emit_defs: bool) -> (String, Option<String>) {
    let mut code_sections: Vec<String> = Vec::new();
    let mut definition_sections: Vec<String> = Vec::new();

    for struct_def in &context.structs {
        let interface = render_ts_interface(struct_def);
        code_sections.push(interface.clone());
        if emit_defs {
            definition_sections.push(interface);
        }
    }

    for enum_def in &context.enums {
        let enum_block = render_ts_enum(enum_def);
        code_sections.push(enum_block.clone());
        if emit_defs {
            definition_sections.push(enum_block);
        }
    }

    if !context.structs.is_empty() {
        let mut const_block = String::new();
        let mut defs_block = String::new();
        for struct_def in &context.structs {
            let const_name = to_upper_snake(struct_def.name.name.as_str()) + "_SIZE";
            let _ = writeln!(
                const_block,
                "const {} = {};",
                const_name,
                struct_def.fields.len()
            );
            if emit_defs {
                let _ = writeln!(defs_block, "declare const {}: number;", const_name);
            }
        }
        code_sections.push(const_block);
        if emit_defs && !defs_block.is_empty() {
            definition_sections.push(defs_block);
        }
    }

    let mut sorted_constants = BTreeMap::new();
    for (name, value) in &context.constants {
        sorted_constants.insert(name.clone(), value.clone());
    }

    if !sorted_constants.is_empty() {
        let mut const_block = String::new();
        let mut defs_block = String::new();
        for (name, value) in sorted_constants {
            let rendered = render_ts_value(&value);
            let _ = writeln!(const_block, "const {} = {};", name, rendered);
            if emit_defs {
                let ty = infer_ts_type_from_value(&value);
                let _ = writeln!(defs_block, "declare const {}: {};", name, ty);
            }
        }
        code_sections.push(const_block);
        if emit_defs && !defs_block.is_empty() {
            definition_sections.push(defs_block);
        }
    }

    let mut main_section = String::from("function main(): void {\n");
    if !context.struct_inits.is_empty() {
        for call in &context.struct_inits {
            let _ = writeln!(main_section, "  {};", call);
        }
        main_section.push('\n');
    }
    main_section.push_str("  console.log('TypeScript output');\n}\n\nmain();\n");
    code_sections.push(main_section);

    let mut code = code_sections.join("\n");
    if !code.ends_with('\n') {
        code.push('\n');
    }

    let type_defs = if emit_defs {
        if definition_sections.is_empty() {
            None
        } else {
            let mut defs = definition_sections.join("\n");
            if !defs.ends_with('\n') {
                defs.push('\n');
            }
            Some(defs)
        }
    } else {
        None
    };

    (code, type_defs)
}

fn render_javascript(context: &TypeScriptContext) -> String {
    let mut output = String::new();

    for struct_def in &context.structs {
        let params: Vec<_> = struct_def
            .fields
            .iter()
            .map(|field| field.name.name.as_str())
            .collect();

        let _ = writeln!(
            output,
            "function create{}({}) {{",
            struct_def.name.name,
            params.join(", ")
        );
        output.push_str("  return {\n");
        for field in &struct_def.fields {
            let _ = writeln!(output, "    {}: {},", field.name.name, field.name.name);
        }
        output.push_str("  };\n}\n\n");
    }

    if !context.structs.is_empty() {
        for struct_def in &context.structs {
            let const_name = to_upper_snake(struct_def.name.name.as_str()) + "_SIZE";
            let _ = writeln!(
                output,
                "const {} = {};",
                const_name,
                struct_def.fields.len()
            );
        }
        output.push('\n');
    }

    let mut sorted_constants = BTreeMap::new();
    for (name, value) in &context.constants {
        sorted_constants.insert(name.clone(), value.clone());
    }
    if !sorted_constants.is_empty() {
        for (name, value) in sorted_constants {
            let rendered = render_js_value(&value);
            let _ = writeln!(output, "const {} = {};", name, rendered);
        }
        output.push('\n');
    }

    output.push_str("function main() {\n");
    if !context.struct_inits.is_empty() {
        for call in &context.struct_inits {
            let _ = writeln!(output, "  {};", call);
        }
        output.push('\n');
    }
    output.push_str("  console.log('JavaScript output');\n}\n\nmain();\n");
    output
}

fn render_ts_interface(struct_def: &TypeStruct) -> String {
    let mut interface = String::new();
    let _ = writeln!(interface, "interface {} {{", struct_def.name.name);
    for field in &struct_def.fields {
        let ty = ts_type_from_ty(&field.value);
        let _ = writeln!(interface, "  {}: {};", field.name.name, ty);
    }
    interface.push_str("}\n");
    interface
}

fn render_ts_enum(enum_def: &TypeEnum) -> String {
    let mut block = String::new();
    let _ = writeln!(block, "enum {} {{", enum_def.name.name);
    for variant in &enum_def.variants {
        let _ = writeln!(block, "  {},", variant.name.name);
    }
    block.push_str("}\n");
    block
}

fn ts_type_from_ty(ty: &Ty) -> String {
    match ty {
        Ty::Primitive(primitive) => match primitive {
            TypePrimitive::Bool => "boolean".into(),
            TypePrimitive::String | TypePrimitive::Char => "string".into(),
            TypePrimitive::Int(_) | TypePrimitive::Decimal(_) => "number".into(),
            TypePrimitive::List => "Array<any>".into(),
        },
        Ty::Vec(vec_ty) => format!("Array<{}>", ts_type_from_ty(&vec_ty.ty)),
        Ty::Tuple(tuple_ty) => {
            let elements: Vec<_> = tuple_ty.types.iter().map(ts_type_from_ty).collect();
            format!("[{}]", elements.join(", "))
        }
        Ty::Struct(struct_ty) => struct_ty.name.name.clone(),
        Ty::Enum(enum_ty) => enum_ty.name.name.clone(),
        Ty::Reference(reference) => ts_type_from_ty(&reference.ty),
        Ty::Expr(expr) => match expr.kind() {
            ExprKind::Locator(locator) => {
                if let Some(ident) = locator.as_ident() {
                    map_ident_to_ts(ident.name.as_str()).into()
                } else {
                    let path = locator.to_path();
                    path.segments
                        .last()
                        .map(|segment| map_ident_to_ts(segment.name.as_str()).into())
                        .unwrap_or_else(|| "any".into())
                }
            }
            _ => "any".into(),
        },
        Ty::Any(_) | Ty::Unknown(_) => "any".into(),
        Ty::Unit(_) => "void".into(),
        _ => "any".into(),
    }
}

fn render_ts_value(value: &Value) -> String {
    match value {
        Value::Int(v) => v.value.to_string(),
        Value::Decimal(v) => format_decimal(v.value),
        Value::Bool(v) => if v.value { "true" } else { "false" }.into(),
        Value::String(v) => serde_json::to_string(&v.value).unwrap_or_else(|_| "\"\"".into()),
        Value::List(list) => {
            let elements: Vec<_> = list.values.iter().map(render_ts_value).collect();
            format!("[{}]", elements.join(", "))
        }
        Value::Map(map) => {
            let entries: Vec<_> = map
                .entries
                .iter()
                .map(|entry| {
                    format!(
                        "{}: {}",
                        render_ts_value(&entry.key),
                        render_ts_value(&entry.value)
                    )
                })
                .collect();
            format!("{{{}}}", entries.join(", "))
        }
        Value::Struct(struct_value) => {
            let assignments: Vec<_> = struct_value
                .structural
                .fields
                .iter()
                .map(|field| format!("{}: {}", field.name.name, render_ts_value(&field.value)))
                .collect();
            format!("{{{}}}", assignments.join(", "))
        }
        Value::Tuple(tuple_value) => {
            let entries: Vec<_> = tuple_value.values.iter().map(render_ts_value).collect();
            format!("[{}]", entries.join(", "))
        }
        _ => "undefined".into(),
    }
}

fn infer_ts_type_from_value(value: &Value) -> String {
    match value {
        Value::Int(_) | Value::Decimal(_) => "number".into(),
        Value::Bool(_) => "boolean".into(),
        Value::String(_) => "string".into(),
        Value::List(values) => {
            if let Some(first) = values.values.first() {
                format!("Array<{}>", infer_ts_type_from_value(first))
            } else {
                "Array<any>".into()
            }
        }
        Value::Map(_) => "Record<string, any>".into(),
        Value::Struct(struct_value) => struct_value.ty.name.name.clone(),
        Value::Tuple(tuple_value) => {
            let elements: Vec<_> = tuple_value
                .values
                .iter()
                .map(infer_ts_type_from_value)
                .collect();
            format!("[{}]", elements.join(", "))
        }
        Value::Unit(_) | Value::Null(_) | Value::Undefined(_) | Value::None(_) => {
            "undefined".into()
        }
        _ => "any".into(),
    }
}

fn render_js_value(value: &Value) -> String {
    match value {
        Value::Int(v) => v.value.to_string(),
        Value::Decimal(v) => format_decimal(v.value),
        Value::Bool(v) => if v.value { "true" } else { "false" }.into(),
        Value::String(v) => serde_json::to_string(&v.value).unwrap_or_else(|_| "\"\"".into()),
        Value::List(list) => {
            let elements: Vec<_> = list.values.iter().map(render_js_value).collect();
            format!("[{}]", elements.join(", "))
        }
        Value::Map(map) => {
            let entries: Vec<_> = map
                .entries
                .iter()
                .map(|entry| {
                    format!(
                        "{}: {}",
                        render_js_value(&entry.key),
                        render_js_value(&entry.value)
                    )
                })
                .collect();
            format!("{{{}}}", entries.join(", "))
        }
        Value::Struct(struct_value) => {
            let assignments: Vec<_> = struct_value
                .structural
                .fields
                .iter()
                .map(|field| format!("{}: {}", field.name.name, render_js_value(&field.value)))
                .collect();
            format!("{{{}}}", assignments.join(", "))
        }
        Value::Tuple(tuple_value) => {
            let entries: Vec<_> = tuple_value.values.iter().map(render_js_value).collect();
            format!("[{}]", entries.join(", "))
        }
        _ => "undefined".into(),
    }
}

fn format_decimal(value: f64) -> String {
    if value == 0.0 {
        if value.is_sign_negative() {
            "-0.0".to_string()
        } else {
            "0.0".to_string()
        }
    } else {
        let mut text = value.to_string();
        if !text.contains('.') && !text.contains('e') && !text.contains('E') {
            text.push_str(".0");
        }
        text
    }
}

fn to_upper_snake(name: &str) -> String {
    let mut result = String::new();
    let mut prev_lower = false;
    for ch in name.chars() {
        if ch.is_uppercase() {
            if prev_lower {
                result.push('_');
            }
            result.push(ch);
            prev_lower = false;
        } else if ch.is_alphanumeric() {
            result.push(ch.to_ascii_uppercase());
            prev_lower = true;
        } else {
            result.push('_');
            prev_lower = false;
        }
    }
    if result.is_empty() {
        name.to_ascii_uppercase()
    } else {
        result
    }
}

fn map_ident_to_ts(name: &str) -> &str {
    match name {
        "String" | "str" => "string",
        "char" => "string",
        "bool" => "boolean",
        "u8" | "u16" | "u32" | "u64" | "usize" | "i8" | "i16" | "i32" | "i64" | "isize" => "number",
        "f32" | "f64" => "number",
        other => other,
    }
}
