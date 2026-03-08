use std::collections::{BTreeMap, HashMap};

use eyre::{Result as EyreResult, bail};
use fp_core::ast::{
    BlockStmt, Expr, ExprBinOp, ExprBlock, ExprIf, ExprIntrinsicCall, ExprInvoke, ExprInvokeTarget,
    ExprKind, ExprLet, ExprReturn, FunctionParam, Item, ItemDefFunction, ItemKind, Name, Node,
    NodeKind, Pattern, PatternKind, Ty, TypeInt, TypePrimitive, Value,
};
use fp_core::error::Result;
use fp_core::intrinsics::IntrinsicCallKind;
use fp_core::ops::{BinOpKind, UnOpKind};

pub fn emit_cil(node: &Node) -> Result<String> {
    let program = CilProgram::from_node(node).map_err(fp_core::error::Error::from)?;
    program.render().map_err(fp_core::error::Error::from)
}


struct CilProgram<'a> {
    methods: Vec<CollectedFunction<'a>>,
}

struct CollectedFunction<'a> {
    qualified_name: String,
    emitted_name: String,
    function: &'a ItemDefFunction,
}

impl<'a> CilProgram<'a> {
    fn from_node(node: &'a Node) -> EyreResult<Self> {
        let mut methods = Vec::new();
        collect_functions(node, &mut Vec::new(), &mut methods)?;
        Ok(Self { methods })
    }

    fn render(&self) -> EyreResult<String> {
        let mut out = String::new();
        out.push_str("// FerroPhase .NET backend (experimental)\n");
        out.push_str("// Artifact kind: textual Common Intermediate Language (CIL)\n");
        out.push_str("// Use ilasm or a future PE writer to assemble this artifact.\n\n");
        out.push_str(".assembly extern mscorlib {}\n");
        out.push_str(".assembly 'FerroPhase.Generated' {}\n");
        out.push_str(".module 'FerroPhase.Generated.dll'\n\n");
        out.push_str(".class public abstract sealed auto ansi FerroPhaseProgram extends [mscorlib]System.Object\n");
        out.push_str("{\n");

        for method in &self.methods {
            let rendered = MethodEmitter::new(method, &self.methods).emit()?;
            for line in rendered.lines() {
                out.push_str("  ");
                out.push_str(line);
                out.push('\n');
            }
            out.push('\n');
        }

        if let Some(main) = self
            .methods
            .iter()
            .find(|method| method.qualified_name == "main")
        {
            out.push_str("  .method public hidebysig static void __fp_entrypoint(string[] args) cil managed\n");
            out.push_str("  {\n");
            out.push_str("    .entrypoint\n");
            out.push_str("    .maxstack 8\n");
            if main.function.sig.params.is_empty() {
                out.push_str(&format!(
                    "    call {} FerroPhaseProgram::{}()\n",
                    cil_type(main.function.sig.ret_ty.as_ref())?,
                    quote_method_name(&main.emitted_name)
                ));
                if !is_void(main.function.sig.ret_ty.as_ref()) {
                    out.push_str("    pop\n");
                }
            }
            out.push_str("    ret\n");
            out.push_str("  }\n");
        }

        out.push_str("}\n");
        Ok(out)
    }
}

fn collect_functions<'a>(
    node: &'a Node,
    module_path: &mut Vec<String>,
    methods: &mut Vec<CollectedFunction<'a>>,
) -> EyreResult<()> {
    match node.kind() {
        NodeKind::File(file) => {
            for item in &file.items {
                collect_from_item(item, module_path, methods)?;
            }
        }
        NodeKind::Item(item) => collect_from_item(item, module_path, methods)?,
        NodeKind::Expr(expr) => collect_from_expr(expr, module_path, methods)?,
        NodeKind::Query(_) => bail!("CIL backend does not support query documents"),
        NodeKind::Schema(_) => bail!("CIL backend does not support schema documents"),
        NodeKind::Workspace(_) => bail!("CIL backend does not support workspace documents"),
    }
    Ok(())
}

fn collect_from_expr<'a>(
    expr: &'a Expr,
    module_path: &mut Vec<String>,
    methods: &mut Vec<CollectedFunction<'a>>,
) -> EyreResult<()> {
    if let ExprKind::Block(block) = expr.kind() {
        for stmt in &block.stmts {
            if let BlockStmt::Item(item) = stmt {
                collect_from_item(item, module_path, methods)?;
            }
        }
    }
    Ok(())
}

fn collect_from_item<'a>(
    item: &'a Item,
    module_path: &mut Vec<String>,
    methods: &mut Vec<CollectedFunction<'a>>,
) -> EyreResult<()> {
    match item.kind() {
        ItemKind::DefFunction(function) => {
            if module_path.first().is_some_and(|segment| segment == "std") {
                return Ok(());
            }
            let qualified_name = qualify_name(module_path, function.name.as_str());
            methods.push(CollectedFunction {
                emitted_name: emitted_method_name(module_path, function.name.as_str()),
                qualified_name,
                function,
            });
        }
        ItemKind::Module(module) => {
            module_path.push(module.name.as_str().to_string());
            for item in &module.items {
                collect_from_item(item, module_path, methods)?;
            }
            module_path.pop();
        }
        ItemKind::Expr(expr) => collect_from_expr(expr, module_path, methods)?,
        _ => {}
    }
    Ok(())
}

fn qualify_name(module_path: &[String], name: &str) -> String {
    if module_path.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", module_path.join("::"), name)
    }
}

fn emitted_method_name(module_path: &[String], name: &str) -> String {
    if module_path.is_empty() {
        name.to_string()
    } else {
        format!("{}__{}", module_path.join("__"), name)
    }
}

#[derive(Clone)]
struct LocalSlot {
    index: usize,
    ty: Ty,
}

struct MethodEmitter<'a> {
    method: &'a CollectedFunction<'a>,
    method_names: HashMap<String, String>,
    method_signatures: HashMap<String, &'a ItemDefFunction>,
    locals: BTreeMap<String, LocalSlot>,
    params: HashMap<String, usize>,
    instructions: Vec<String>,
    next_label: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum EmitMode {
    Value,
    Statement,
}

impl<'a> MethodEmitter<'a> {
    fn new(method: &'a CollectedFunction<'a>, methods: &'a [CollectedFunction<'a>]) -> Self {
        let method_names = methods
            .iter()
            .map(|item| (item.qualified_name.clone(), item.emitted_name.clone()))
            .collect();
        let method_signatures = methods
            .iter()
            .map(|item| (item.qualified_name.clone(), item.function))
            .collect();
        let params = method
            .function
            .sig
            .params
            .iter()
            .enumerate()
            .map(|(index, param)| (param.name.as_str().to_string(), index))
            .collect();
        Self {
            method,
            method_names,
            method_signatures,
            locals: collect_locals(method.function),
            params,
            instructions: Vec::new(),
            next_label: 0,
        }
    }

    fn emit(mut self) -> EyreResult<String> {
        let ret_ty = cil_type(self.method.function.sig.ret_ty.as_ref())?;
        let params = self
            .method
            .function
            .sig
            .params
            .iter()
            .map(render_param)
            .collect::<EyreResult<Vec<_>>>()?
            .join(", ");

        let mut out = String::new();
        out.push_str(&format!(
            ".method public hidebysig static {} {}({}) cil managed\n",
            ret_ty,
            quote_method_name(&self.method.emitted_name),
            params
        ));
        out.push_str("{\n");
        out.push_str("  .maxstack 16\n");
        if !self.locals.is_empty() {
            let locals = self
                .locals
                .iter()
                .map(|(name, slot)| {
                    format!(
                        "[{}] {} {}",
                        slot.index,
                        cil_type(Some(&slot.ty)).unwrap(),
                        sanitize_ident(name)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            out.push_str(&format!("  .locals init ({})\n", locals));
        }

        let expects_value = !is_void(self.method.function.sig.ret_ty.as_ref());
        self.emit_expr(
            self.method.function.body.as_ref(),
            if expects_value {
                EmitMode::Value
            } else {
                EmitMode::Statement
            },
        )?;
        if !self.ends_with_terminal() {
            self.push("ret");
        }

        for instruction in &self.instructions {
            if instruction.ends_with(':') {
                out.push_str(" ");
                out.push_str(instruction);
                out.push('\n');
            } else {
                out.push_str("  ");
                out.push_str(instruction);
                out.push('\n');
            }
        }
        out.push_str("}\n");
        Ok(out)
    }

    fn emit_expr(&mut self, expr: &Expr, mode: EmitMode) -> EyreResult<()> {
        match expr.kind() {
            ExprKind::Value(value) => {
                self.emit_value(value)?;
                if mode == EmitMode::Statement && !value.is_unit() {
                    self.push("pop");
                }
            }
            ExprKind::Name(name) => {
                self.emit_name(name)?;
                if mode == EmitMode::Statement {
                    self.push("pop");
                }
            }
            ExprKind::Block(block) => self.emit_block(block, mode)?,
            ExprKind::Return(ret) => self.emit_return(ret)?,
            ExprKind::BinOp(binop) => {
                self.emit_binop(binop)?;
                if mode == EmitMode::Statement {
                    self.push("pop");
                }
            }
            ExprKind::UnOp(unop) => {
                self.emit_unop(unop.op.clone(), unop.val.as_ref())?;
                if mode == EmitMode::Statement {
                    self.push("pop");
                }
            }
            ExprKind::If(expr_if) => self.emit_if(expr_if, mode)?,
            ExprKind::Invoke(invoke) => {
                self.emit_invoke(invoke)?;
                if mode == EmitMode::Statement && !is_void(expr.ty()) {
                    self.push("pop");
                }
            }
            ExprKind::IntrinsicCall(call) => {
                self.emit_intrinsic_call(call)?;
                if mode == EmitMode::Statement && !is_void(expr.ty()) {
                    self.push("pop");
                }
            }
            ExprKind::Let(expr_let) => {
                self.emit_expr_let(expr_let)?;
            }
            ExprKind::Assign(assign) => {
                let name = match assign.target.kind() {
                    ExprKind::Name(name) => name,
                    _ => bail!("only local or parameter assignment is supported in CIL output"),
                };
                self.emit_expr(assign.value.as_ref(), EmitMode::Value)?;
                self.store_name(name)?;
                if mode == EmitMode::Value {
                    self.emit_name(name)?;
                }
            }
            ExprKind::Paren(paren) => self.emit_expr(paren.expr.as_ref(), mode)?,
            ExprKind::Cast(cast) => {
                self.emit_expr(cast.expr.as_ref(), EmitMode::Value)?;
                self.emit_numeric_cast(&cast.ty)?;
                if mode == EmitMode::Statement {
                    self.push("pop");
                }
            }
            other => bail!("unsupported expression for CIL backend: {:?}", other),
        }
        Ok(())
    }

    fn emit_block(&mut self, block: &ExprBlock, mode: EmitMode) -> EyreResult<()> {
        let mut tail_expr = None;
        let mut last_expr = None;
        for stmt in &block.stmts {
            match stmt {
                BlockStmt::Let(stmt_let) => {
                    self.emit_stmt_let(&stmt_let.pat, stmt_let.init.as_ref())?
                }
                BlockStmt::Expr(stmt_expr) => {
                    last_expr = Some(stmt_expr.expr.as_ref());
                    if stmt_expr.has_value() {
                        tail_expr = Some(stmt_expr.expr.as_ref());
                    } else {
                        self.emit_expr(stmt_expr.expr.as_ref(), EmitMode::Statement)?;
                    }
                }
                BlockStmt::Item(_) | BlockStmt::Noop => {}
                BlockStmt::Any(_) => {
                    bail!("opaque block statements are not supported in CIL output")
                }
            }
        }

        let value_expr = tail_expr.or(last_expr);

        match (value_expr, mode) {
            (Some(expr), EmitMode::Value) => self.emit_expr(expr, EmitMode::Value)?,
            (Some(expr), EmitMode::Statement) => self.emit_expr(expr, EmitMode::Statement)?,
            (None, EmitMode::Value) => bail!("block expression requires a value for CIL output"),
            (None, EmitMode::Statement) => {}
        }
        Ok(())
    }

    fn emit_return(&mut self, ret: &ExprReturn) -> EyreResult<()> {
        if let Some(value) = &ret.value {
            self.emit_expr(value.as_ref(), EmitMode::Value)?;
        }
        self.push("ret");
        Ok(())
    }

    fn emit_binop(&mut self, binop: &ExprBinOp) -> EyreResult<()> {
        match binop.kind {
            BinOpKind::And => {
                self.emit_short_circuit(binop.lhs.as_ref(), binop.rhs.as_ref(), false)?;
                return Ok(());
            }
            BinOpKind::Or => {
                self.emit_short_circuit(binop.lhs.as_ref(), binop.rhs.as_ref(), true)?;
                return Ok(());
            }
            _ => {}
        }

        self.emit_expr(binop.lhs.as_ref(), EmitMode::Value)?;
        self.emit_expr(binop.rhs.as_ref(), EmitMode::Value)?;
        match binop.kind {
            BinOpKind::Add | BinOpKind::AddTrait => self.push("add"),
            BinOpKind::Sub => self.push("sub"),
            BinOpKind::Mul => self.push("mul"),
            BinOpKind::Div => self.push("div"),
            BinOpKind::Mod => self.push("rem"),
            BinOpKind::BitAnd => self.push("and"),
            BinOpKind::BitOr => self.push("or"),
            BinOpKind::BitXor => self.push("xor"),
            BinOpKind::Shl => self.push("shl"),
            BinOpKind::Shr => self.push("shr"),
            BinOpKind::Eq => self.push("ceq"),
            BinOpKind::Lt => self.push("clt"),
            BinOpKind::Gt => self.push("cgt"),
            BinOpKind::Ne => {
                self.push("ceq");
                self.push("ldc.i4.0");
                self.push("ceq");
            }
            BinOpKind::Le => {
                self.push("cgt");
                self.push("ldc.i4.0");
                self.push("ceq");
            }
            BinOpKind::Ge => {
                self.push("clt");
                self.push("ldc.i4.0");
                self.push("ceq");
            }
            BinOpKind::And | BinOpKind::Or => unreachable!(),
        }
        Ok(())
    }

    fn emit_short_circuit(&mut self, lhs: &Expr, rhs: &Expr, is_or: bool) -> EyreResult<()> {
        let short_label = self.new_label("logic_short");
        let end_label = self.new_label("logic_end");
        self.emit_expr(lhs, EmitMode::Value)?;
        self.push("dup");
        self.push(if is_or {
            format!("brtrue {}", short_label)
        } else {
            format!("brfalse {}", short_label)
        });
        self.push("pop");
        self.emit_expr(rhs, EmitMode::Value)?;
        self.push(format!("br {}", end_label));
        self.push_label(&short_label);
        self.push(format!("ldc.i4.{}", if is_or { 1 } else { 0 }));
        self.push_label(&end_label);
        Ok(())
    }

    fn emit_unop(&mut self, op: UnOpKind, expr: &Expr) -> EyreResult<()> {
        self.emit_expr(expr, EmitMode::Value)?;
        match op {
            UnOpKind::Neg => self.push("neg"),
            UnOpKind::Not => {
                self.push("ldc.i4.0");
                self.push("ceq");
            }
            UnOpKind::Deref | UnOpKind::Any(_) => {
                bail!("unsupported unary operator for CIL output: {}", op)
            }
        }
        Ok(())
    }

    fn emit_if(&mut self, expr_if: &ExprIf, mode: EmitMode) -> EyreResult<()> {
        let else_label = self.new_label("else");
        let end_label = self.new_label("endif");
        self.emit_expr(expr_if.cond.as_ref(), EmitMode::Value)?;
        self.push(format!("brfalse {}", else_label));
        self.emit_expr(expr_if.then.as_ref(), mode)?;
        self.push(format!("br {}", end_label));
        self.push_label(&else_label);
        if let Some(elze) = &expr_if.elze {
            self.emit_expr(elze.as_ref(), mode)?;
        } else if mode == EmitMode::Value {
            bail!("if expressions without else are not supported in value position for CIL output");
        }
        self.push_label(&end_label);
        Ok(())
    }

    fn emit_invoke(&mut self, invoke: &ExprInvoke) -> EyreResult<()> {
        for arg in &invoke.args {
            self.emit_expr(arg, EmitMode::Value)?;
        }

        let (qualified_name, method_name) = match &invoke.target {
            ExprInvokeTarget::Function(name) => {
                let qualified = resolve_name(name);
                let emitted = self
                    .method_names
                    .get(&qualified)
                    .cloned()
                    .or_else(|| self.method_names.get(&name.to_string()).cloned())
                    .unwrap_or_else(|| sanitize_ident(&qualified.replace("::", "__")));
                (qualified, emitted)
            }
            _ => bail!("only direct function invocation is currently supported for CIL output"),
        };

        let target = self
            .method_names
            .keys()
            .find(|candidate| {
                *candidate == &qualified_name
                    || candidate.ends_with(&format!("::{}", qualified_name))
            })
            .and_then(|candidate| self.method_names.get(candidate))
            .cloned()
            .unwrap_or(method_name);

        let signature = self
            .find_method_signature(&qualified_name)
            .or_else(|| self.find_method_signature(&target.replace("__", "::")))
            .ok_or_else(|| {
                eyre::eyre!("unknown function call `{}` in CIL output", qualified_name)
            })?;
        self.push(format!(
            "call {} FerroPhaseProgram::{}({})",
            cil_type(signature.sig.ret_ty.as_ref())?,
            quote_method_name(&target),
            signature
                .sig
                .params
                .iter()
                .map(|param| cil_type(Some(&param.ty)).unwrap())
                .collect::<Vec<_>>()
                .join(", ")
        ));
        Ok(())
    }

    fn emit_intrinsic_call(&mut self, call: &ExprIntrinsicCall) -> EyreResult<()> {
        match call.kind {
            IntrinsicCallKind::Print | IntrinsicCallKind::Println => {
                if call.args.len() != 1 {
                    bail!("print intrinsics require exactly one argument in CIL output");
                }
                self.emit_expr(&call.args[0], EmitMode::Value)?;
                let console_method = if call.kind == IntrinsicCallKind::Println {
                    "WriteLine"
                } else {
                    "Write"
                };
                let arg_ty = cil_type(call.args[0].ty())?;
                self.push(format!(
                    "call void [mscorlib]System.Console::{}({})",
                    console_method, arg_ty
                ));
                Ok(())
            }
            IntrinsicCallKind::Format => {
                bail!("format intrinsic is not yet supported in CIL output")
            }
            other => bail!("unsupported intrinsic for CIL output: {:?}", other),
        }
    }

    fn emit_expr_let(&mut self, expr_let: &ExprLet) -> EyreResult<()> {
        self.emit_expr(expr_let.expr.as_ref(), EmitMode::Value)?;
        self.store_pattern(expr_let.pat.as_ref())?;
        Ok(())
    }

    fn emit_stmt_let(&mut self, pattern: &Pattern, init: Option<&Expr>) -> EyreResult<()> {
        if let Some(init) = init {
            self.emit_expr(init, EmitMode::Value)?;
            self.store_pattern(pattern)?;
        }
        Ok(())
    }

    fn emit_value(&mut self, value: &Value) -> EyreResult<()> {
        match value {
            Value::Int(value) => self.push(format!("ldc.i8 {}", value.value)),
            Value::Bool(value) => self.push(format!("ldc.i4.{}", if value.value { 1 } else { 0 })),
            Value::String(value) => {
                self.push(format!("ldstr {}", quote_string(value.value.as_ref())))
            }
            Value::Char(value) => self.push(format!("ldc.i4 {}", value.value as u32)),
            Value::Unit(_) => {}
            Value::Decimal(value) => self.push(format!("ldc.r8 {}", value.value)),
            other => bail!("unsupported literal for CIL output: {:?}", other),
        }
        Ok(())
    }

    fn emit_name(&mut self, name: &Name) -> EyreResult<()> {
        let ident = resolve_name(name);
        if let Some(index) = self.params.get(&ident) {
            self.push(format!("ldarg {}", index));
            return Ok(());
        }
        if let Some(local) = self.locals.get(&ident) {
            self.push(format!("ldloc {}", local.index));
            return Ok(());
        }
        bail!("unknown local or parameter `{}` in CIL output", ident)
    }

    fn store_name(&mut self, name: &Name) -> EyreResult<()> {
        let ident = resolve_name(name);
        if let Some(index) = self.params.get(&ident) {
            self.push(format!("starg {}", index));
            return Ok(());
        }
        if let Some(local) = self.locals.get(&ident) {
            self.push(format!("stloc {}", local.index));
            return Ok(());
        }
        bail!("unknown local or parameter `{}` in CIL output", ident)
    }

    fn store_pattern(&mut self, pattern: &Pattern) -> EyreResult<()> {
        let name = pattern_ident(pattern)
            .ok_or_else(|| eyre::eyre!("only identifier patterns are supported in CIL output"))?;
        self.store_name(&Name::ident(name))
    }

    fn emit_numeric_cast(&mut self, ty: &Ty) -> EyreResult<()> {
        let opcode = match ty {
            Ty::Primitive(TypePrimitive::Int(TypeInt::I8)) => "conv.i1",
            Ty::Primitive(TypePrimitive::Int(TypeInt::I16)) => "conv.i2",
            Ty::Primitive(TypePrimitive::Int(TypeInt::I32)) => "conv.i4",
            Ty::Primitive(TypePrimitive::Int(TypeInt::I64)) => "conv.i8",
            Ty::Primitive(TypePrimitive::Int(TypeInt::U8)) => "conv.u1",
            Ty::Primitive(TypePrimitive::Int(TypeInt::U16)) => "conv.u2",
            Ty::Primitive(TypePrimitive::Int(TypeInt::U32)) => "conv.u4",
            Ty::Primitive(TypePrimitive::Int(TypeInt::U64)) => "conv.u8",
            Ty::Primitive(TypePrimitive::Decimal(_)) => "conv.r8",
            _ => bail!("unsupported cast target for CIL output: {:?}", ty),
        };
        self.push(opcode);
        Ok(())
    }

    fn find_method_signature(&self, name: &str) -> Option<&'a ItemDefFunction> {
        self.method_signatures
            .get(name)
            .copied()
            .or_else(|| {
                self.method_signatures
                    .get(&name.replace("__", "::"))
                    .copied()
            })
            .or_else(|| {
                self.method_signatures
                    .iter()
                    .find(|(qualified, _)| qualified.ends_with(&format!("::{}", name)))
                    .map(|(_, function)| *function)
            })
    }

    fn push(&mut self, instruction: impl Into<String>) {
        self.instructions.push(instruction.into());
    }

    fn push_label(&mut self, label: &str) {
        self.instructions.push(format!("{}:", label));
    }

    fn new_label(&mut self, prefix: &str) -> String {
        let label = format!("{}_{}", prefix, self.next_label);
        self.next_label += 1;
        label
    }

    fn ends_with_terminal(&self) -> bool {
        self.instructions.last().is_some_and(|line| line == "ret")
    }
}

fn collect_locals(function: &ItemDefFunction) -> BTreeMap<String, LocalSlot> {
    let mut names = Vec::new();
    collect_locals_expr(function.body.as_ref(), &mut names);
    let mut locals = BTreeMap::new();
    for (index, (name, ty)) in names.into_iter().enumerate() {
        locals.entry(name).or_insert(LocalSlot { index, ty });
    }
    locals
}

fn collect_locals_expr(expr: &Expr, locals: &mut Vec<(String, Ty)>) {
    match expr.kind() {
        ExprKind::Block(block) => {
            for stmt in &block.stmts {
                match stmt {
                    BlockStmt::Let(stmt_let) => {
                        if let Some(name) = pattern_ident(&stmt_let.pat) {
                            locals.push((
                                name.to_string(),
                                pattern_ty(&stmt_let.pat, stmt_let.init.as_ref()),
                            ));
                        }
                        if let Some(init) = &stmt_let.init {
                            collect_locals_expr(init, locals);
                        }
                    }
                    BlockStmt::Expr(stmt_expr) => {
                        collect_locals_expr(stmt_expr.expr.as_ref(), locals)
                    }
                    BlockStmt::Item(item) => collect_locals_item(item, locals),
                    BlockStmt::Noop | BlockStmt::Any(_) => {}
                }
            }
        }
        ExprKind::Let(expr_let) => {
            if let Some(name) = pattern_ident(expr_let.pat.as_ref()) {
                locals.push((
                    name.to_string(),
                    pattern_ty(expr_let.pat.as_ref(), Some(expr_let.expr.as_ref())),
                ));
            }
            collect_locals_expr(expr_let.expr.as_ref(), locals);
        }
        ExprKind::If(expr_if) => {
            collect_locals_expr(expr_if.cond.as_ref(), locals);
            collect_locals_expr(expr_if.then.as_ref(), locals);
            if let Some(elze) = &expr_if.elze {
                collect_locals_expr(elze.as_ref(), locals);
            }
        }
        ExprKind::Return(ret) => {
            if let Some(value) = &ret.value {
                collect_locals_expr(value.as_ref(), locals);
            }
        }
        ExprKind::Invoke(invoke) => {
            for arg in &invoke.args {
                collect_locals_expr(arg, locals);
            }
        }
        ExprKind::IntrinsicCall(call) => {
            for arg in &call.args {
                collect_locals_expr(arg, locals);
            }
        }
        ExprKind::BinOp(binop) => {
            collect_locals_expr(binop.lhs.as_ref(), locals);
            collect_locals_expr(binop.rhs.as_ref(), locals);
        }
        ExprKind::UnOp(unop) => collect_locals_expr(unop.val.as_ref(), locals),
        ExprKind::Assign(assign) => collect_locals_expr(assign.value.as_ref(), locals),
        ExprKind::Paren(paren) => collect_locals_expr(paren.expr.as_ref(), locals),
        ExprKind::Cast(cast) => collect_locals_expr(cast.expr.as_ref(), locals),
        _ => {}
    }
}

fn collect_locals_item(item: &Item, locals: &mut Vec<(String, Ty)>) {
    match item.kind() {
        ItemKind::Expr(expr) => collect_locals_expr(expr, locals),
        ItemKind::Module(module) => {
            for item in &module.items {
                collect_locals_item(item, locals);
            }
        }
        _ => {}
    }
}

fn pattern_ident(pattern: &Pattern) -> Option<&str> {
    match pattern.kind() {
        PatternKind::Ident(ident) => Some(ident.ident.as_str()),
        PatternKind::Type(pattern_type) => pattern_ident(pattern_type.pat.as_ref()),
        _ => None,
    }
}

fn pattern_ty(pattern: &Pattern, init: Option<&Expr>) -> Ty {
    match pattern.kind() {
        PatternKind::Type(pattern_type) => pattern_type.ty.clone(),
        _ => init
            .and_then(|expr| expr.ty().cloned())
            .unwrap_or(Ty::Primitive(TypePrimitive::Int(TypeInt::I64))),
    }
}

fn render_param(param: &FunctionParam) -> EyreResult<String> {
    let _ = param;
    Ok(cil_type(Some(&param.ty))?)
}

fn cil_type(ty: Option<&Ty>) -> EyreResult<String> {
    let ty = match ty {
        Some(ty) => ty,
        None => return Ok("void".to_string()),
    };
    match ty {
        Ty::Primitive(TypePrimitive::Bool) => Ok("bool".to_string()),
        Ty::Primitive(TypePrimitive::Char) => Ok("char".to_string()),
        Ty::Primitive(TypePrimitive::String) => Ok("string".to_string()),
        Ty::Primitive(TypePrimitive::Int(kind)) => Ok(match kind {
            TypeInt::I8 => "int8",
            TypeInt::I16 => "int16",
            TypeInt::I32 => "int32",
            TypeInt::I64 => "int64",
            TypeInt::U8 => "uint8",
            TypeInt::U16 => "uint16",
            TypeInt::U32 => "uint32",
            TypeInt::U64 => "uint64",
            TypeInt::BigInt => "int64",
        }
        .to_string()),
        Ty::Primitive(TypePrimitive::Decimal(kind)) => Ok(match kind {
            fp_core::ast::DecimalType::F32 => "float32",
            fp_core::ast::DecimalType::F64 => "float64",
            _ => "float64",
        }
        .to_string()),
        Ty::Reference(reference) => cil_type(Some(reference.ty.as_ref())),
        Ty::Expr(expr) => cil_type_from_expr(expr),
        Ty::Any(_) | Ty::Unknown(_) | Ty::Value(_) => Ok("object".to_string()),
        Ty::Vec(_) | Ty::Array(_) | Ty::Slice(_) | Ty::RawPtr(_) => Ok("object".to_string()),
        Ty::Unit(_) => Ok("void".to_string()),
        _ => Ok("object".to_string()),
    }
}

fn cil_type_from_expr(expr: &Expr) -> EyreResult<String> {
    match expr.kind() {
        ExprKind::Name(name) => match name.to_string().as_str() {
            "bool" => Ok("bool".to_string()),
            "char" => Ok("char".to_string()),
            "str" | "string" => Ok("string".to_string()),
            "i8" => Ok("int8".to_string()),
            "i16" => Ok("int16".to_string()),
            "i32" => Ok("int32".to_string()),
            "i64" => Ok("int64".to_string()),
            "u8" => Ok("uint8".to_string()),
            "u16" => Ok("uint16".to_string()),
            "u32" => Ok("uint32".to_string()),
            "u64" => Ok("uint64".to_string()),
            "f32" => Ok("float32".to_string()),
            "f64" => Ok("float64".to_string()),
            _ => Ok("object".to_string()),
        },
        ExprKind::Value(value) => match value.as_ref() {
            Value::Type(inner) => cil_type(Some(inner)),
            _ => Ok("object".to_string()),
        },
        _ => Ok("object".to_string()),
    }
}

fn is_void(ty: Option<&Ty>) -> bool {
    match ty {
        None => true,
        Some(Ty::Unit(_)) => true,
        _ => false,
    }
}

fn resolve_name(name: &Name) -> String {
    match name {
        Name::Ident(ident) => ident.as_str().to_string(),
        _ => name.to_string(),
    }
}

fn sanitize_ident(name: &str) -> String {
    name.replace("::", "__")
}

fn quote_string(value: &str) -> String {
    format!(
        "\"{}\"",
        value
            .replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    )
}

fn quote_method_name(name: &str) -> String {
    format!("'{}'", name.replace('"', "").replace('\'', "''"))
}
