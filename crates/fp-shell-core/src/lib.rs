use fp_core::ast::{
    BlockStmt, Expr, ExprBlock, ExprInvokeTarget, ExprKind, ExprMatch, ExprMatchCase,
    ItemDefFunction, PatternKind,
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Default)]
pub struct ShellInventory {
    pub groups: HashMap<String, Vec<String>>,
    pub hosts: HashMap<String, InventoryHost>,
}

#[derive(Debug, Clone)]
pub struct InventoryHost {
    pub transport: TransportKind,
    pub ssh: Option<SshInventory>,
    pub docker: Option<DockerInventory>,
    pub kubectl: Option<KubectlInventory>,
    pub winrm: Option<WinRmInventory>,
}

impl Default for InventoryHost {
    fn default() -> Self {
        Self {
            transport: TransportKind::Ssh,
            ssh: Some(SshInventory::default()),
            docker: None,
            kubectl: None,
            winrm: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TransportKind {
    Local,
    Ssh,
    Docker,
    Kubectl,
    Winrm,
}

#[derive(Debug, Clone, Default)]
pub struct SshInventory {
    pub address: Option<String>,
    pub user: Option<String>,
    pub port: Option<u16>,
}

#[derive(Debug, Clone, Default)]
pub struct DockerInventory {
    pub container: String,
    pub user: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub struct KubectlInventory {
    pub pod: String,
    pub namespace: Option<String>,
    pub container: Option<String>,
    pub context: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub struct WinRmInventory {
    pub address: String,
    pub user: String,
    pub password: Option<String>,
    pub port: Option<u16>,
    pub scheme: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScriptTarget {
    Bash,
    PowerShell,
}

impl ScriptTarget {
    pub fn extension(self) -> &'static str {
        match self {
            Self::Bash => "sh",
            Self::PowerShell => "ps1",
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScriptProgram {
    pub externs: HashMap<String, ExternalFunction>,
    pub items: Vec<ScriptItem>,
}

impl Default for ScriptProgram {
    fn default() -> Self {
        Self {
            externs: HashMap::new(),
            items: Vec::new(),
        }
    }
}

impl ScriptProgram {
    pub fn validate_externs(&self, target: ScriptTarget) -> Result<(), String> {
        for name in self.used_externs() {
            let Some(external) = self.externs.get(name) else {
                continue;
            };
            external.validate_for_target(target)?;
        }
        Ok(())
    }

    pub fn used_externs(&self) -> HashSet<&str> {
        let functions = self
            .items
            .iter()
            .filter_map(|item| match item {
                ScriptItem::Function(def) => Some((def.name.as_str(), def)),
                ScriptItem::Expr(_) => None,
            })
            .collect::<HashMap<_, _>>();
        let mut used = HashSet::new();
        let mut visited_functions = HashSet::new();
        for item in &self.items {
            if let ScriptItem::Expr(expr) = item {
                collect_expr_externs(
                    expr,
                    &functions,
                    &self.externs,
                    &mut visited_functions,
                    &mut used,
                );
            }
        }
        used
    }
}

#[derive(Debug, Clone)]
pub struct ExternalFunction {
    pub name: String,
    pub abi: String,
    pub param_count: usize,
    pub command: Option<String>,
}

impl ExternalFunction {
    pub fn validate_for_target(&self, target: ScriptTarget) -> Result<(), String> {
        let expected_abi = match target {
            ScriptTarget::Bash => "bash",
            ScriptTarget::PowerShell => "pwsh",
        };
        if self.abi != expected_abi {
            return Err(format!(
                "extern `{}` uses ABI `{}`, but shell target requires `{}`",
                self.name, self.abi, expected_abi
            ));
        }
        if self.command.is_none() && !is_runtime_primitive(&self.name) {
            return Err(format!(
                "extern `{}` is missing #[command = \"...\"] for {} shell target",
                self.name, expected_abi
            ));
        }
        if self
            .command
            .as_deref()
            .is_some_and(|command| command.trim().is_empty())
        {
            return Err(format!(
                "extern `{}` has an empty #[command] annotation",
                self.name
            ));
        }
        Ok(())
    }

    pub fn runtime_requirements(&self, target: ScriptTarget) -> Vec<String> {
        if let Some(command) = &self.command {
            return command
                .split_whitespace()
                .next()
                .map(|tool| vec![tool.to_string()])
                .unwrap_or_default();
        }
        match (target, self.name.as_str()) {
            (ScriptTarget::Bash, "runtime_temp_path") => vec!["mktemp".to_string()],
            _ => Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ScriptItem {
    Function(ItemDefFunction),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueExpr {
    String(StringExpr),
    Int(IntExpr),
    Bool(BoolExpr),
    StringList(Vec<StringExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConditionExpr {
    Bool(BoolExpr),
    Command(StringExpr),
    StringTruthy(StringExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BoolExpr {
    Literal(bool),
    Variable(String),
    IntComparison {
        lhs: IntExpr,
        op: ComparisonOp,
        rhs: IntExpr,
    },
    StringComparison {
        lhs: StringExpr,
        op: StringComparisonOp,
        rhs: StringExpr,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp {
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
    Ne,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringComparisonOp {
    Eq,
    Ne,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntExpr {
    Literal(i64),
    Variable(String),
    UnaryNeg(Box<IntExpr>),
    Binary {
        lhs: Box<IntExpr>,
        op: ArithmeticOp,
        rhs: Box<IntExpr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringExpr {
    Literal(String),
    Variable(String),
    Call { name: String, args: Vec<ValueExpr> },
    Interpolated(Vec<StringPart>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    Literal(String),
    Variable(String),
    Call { name: String, args: Vec<ValueExpr> },
}

pub trait ScriptRenderer {
    type Error;

    fn render(
        &self,
        program: &ScriptProgram,
        inventory: &ShellInventory,
    ) -> Result<String, Self::Error>;
}

fn is_runtime_primitive(name: &str) -> bool {
    matches!(
        name,
        "runtime_host_transport" | "runtime_temp_path" | "runtime_fail" | "runtime_set_changed"
    )
}

fn collect_block_externs<'a>(
    block: &'a ExprBlock,
    functions: &HashMap<&'a str, &'a ItemDefFunction>,
    externs: &HashMap<String, ExternalFunction>,
    visited_functions: &mut HashSet<&'a str>,
    used: &mut HashSet<&'a str>,
) {
    for statement in &block.stmts {
        collect_block_stmt_externs(statement, functions, externs, visited_functions, used);
    }
}

fn collect_block_stmt_externs<'a>(
    statement: &'a BlockStmt,
    functions: &HashMap<&'a str, &'a ItemDefFunction>,
    externs: &HashMap<String, ExternalFunction>,
    visited_functions: &mut HashSet<&'a str>,
    used: &mut HashSet<&'a str>,
) {
    match statement {
        BlockStmt::Expr(expr) => {
            collect_expr_externs(&expr.expr, functions, externs, visited_functions, used)
        }
        BlockStmt::Let(stmt) => {
            if let Some(init) = &stmt.init {
                collect_expr_externs(init, functions, externs, visited_functions, used);
            }
            if let Some(diverge) = &stmt.diverge {
                collect_expr_externs(diverge, functions, externs, visited_functions, used);
            }
        }
        BlockStmt::Any(_) | BlockStmt::Item(_) | BlockStmt::Noop => {}
    }
}

fn collect_condition_externs<'a>(
    condition: &'a ConditionExpr,
    functions: &HashMap<&'a str, &'a ItemDefFunction>,
    externs: &HashMap<String, ExternalFunction>,
    visited_functions: &mut HashSet<&'a str>,
    used: &mut HashSet<&'a str>,
) {
    match condition {
        ConditionExpr::Bool(BoolExpr::IntComparison { lhs, rhs, .. }) => {
            collect_int_externs(lhs, used);
            collect_int_externs(rhs, used);
        }
        ConditionExpr::Bool(BoolExpr::StringComparison { lhs, rhs, .. }) => {
            collect_string_externs(lhs, functions, externs, visited_functions, used);
            collect_string_externs(rhs, functions, externs, visited_functions, used);
        }
        ConditionExpr::Command(expr) | ConditionExpr::StringTruthy(expr) => {
            collect_string_externs(expr, functions, externs, visited_functions, used);
        }
        ConditionExpr::Bool(BoolExpr::Literal(_) | BoolExpr::Variable(_)) => {}
    }
}

fn collect_int_externs<'a>(expr: &'a IntExpr, used: &mut HashSet<&'a str>) {
    match expr {
        IntExpr::UnaryNeg(inner) => collect_int_externs(inner, used),
        IntExpr::Binary { lhs, rhs, .. } => {
            collect_int_externs(lhs, used);
            collect_int_externs(rhs, used);
        }
        IntExpr::Literal(_) | IntExpr::Variable(_) => {}
    }
}

fn collect_value_externs<'a>(
    value: &'a ValueExpr,
    functions: &HashMap<&'a str, &'a ItemDefFunction>,
    externs: &HashMap<String, ExternalFunction>,
    visited_functions: &mut HashSet<&'a str>,
    used: &mut HashSet<&'a str>,
) {
    match value {
        ValueExpr::String(expr) => {
            collect_string_externs(expr, functions, externs, visited_functions, used)
        }
        ValueExpr::StringList(values) => {
            for value in values {
                collect_string_externs(value, functions, externs, visited_functions, used);
            }
        }
        ValueExpr::Int(expr) => collect_int_externs(expr, used),
        ValueExpr::Bool(BoolExpr::StringComparison { lhs, rhs, .. }) => {
            collect_string_externs(lhs, functions, externs, visited_functions, used);
            collect_string_externs(rhs, functions, externs, visited_functions, used);
        }
        ValueExpr::Bool(BoolExpr::IntComparison { lhs, rhs, .. }) => {
            collect_int_externs(lhs, used);
            collect_int_externs(rhs, used);
        }
        ValueExpr::Bool(BoolExpr::Literal(_) | BoolExpr::Variable(_)) => {}
    }
}

fn collect_string_externs<'a>(
    expr: &'a StringExpr,
    functions: &HashMap<&'a str, &'a ItemDefFunction>,
    externs: &HashMap<String, ExternalFunction>,
    visited_functions: &mut HashSet<&'a str>,
    used: &mut HashSet<&'a str>,
) {
    match expr {
        StringExpr::Call { name, args } => {
            collect_call_externs(name, functions, externs, visited_functions, used);
            for arg in args {
                collect_value_externs(arg, functions, externs, visited_functions, used);
            }
        }
        StringExpr::Interpolated(parts) => {
            for part in parts {
                if let StringPart::Call { name, args } = part {
                    collect_call_externs(name, functions, externs, visited_functions, used);
                    for arg in args {
                        collect_value_externs(arg, functions, externs, visited_functions, used);
                    }
                }
            }
        }
        StringExpr::Literal(_) | StringExpr::Variable(_) => {}
    }
}

fn collect_call_externs<'a>(
    name: &'a str,
    functions: &HashMap<&'a str, &'a ItemDefFunction>,
    externs: &HashMap<String, ExternalFunction>,
    visited_functions: &mut HashSet<&'a str>,
    used: &mut HashSet<&'a str>,
) {
    if externs.contains_key(name) {
        used.insert(name);
        return;
    }
    let Some(def) = functions.get(name) else {
        return;
    };
    if visited_functions.insert(name) {
        collect_expr_externs(&def.body, functions, externs, visited_functions, used);
    }
}

fn collect_expr_externs<'a>(
    expr: &'a Expr,
    functions: &HashMap<&'a str, &'a ItemDefFunction>,
    externs: &HashMap<String, ExternalFunction>,
    visited_functions: &mut HashSet<&'a str>,
    used: &mut HashSet<&'a str>,
) {
    match expr.kind() {
        ExprKind::Block(block) => {
            collect_block_externs(block, functions, externs, visited_functions, used)
        }
        ExprKind::If(expr_if) => {
            collect_expr_externs(&expr_if.cond, functions, externs, visited_functions, used);
            collect_expr_externs(&expr_if.then, functions, externs, visited_functions, used);
            if let Some(elze) = &expr_if.elze {
                collect_expr_externs(elze, functions, externs, visited_functions, used);
            }
        }
        ExprKind::While(expr_while) => {
            collect_expr_externs(
                &expr_while.cond,
                functions,
                externs,
                visited_functions,
                used,
            );
            collect_expr_externs(
                &expr_while.body,
                functions,
                externs,
                visited_functions,
                used,
            );
        }
        ExprKind::For(expr_for) => {
            collect_expr_externs(&expr_for.iter, functions, externs, visited_functions, used);
            collect_expr_externs(&expr_for.body, functions, externs, visited_functions, used);
        }
        ExprKind::Let(expr_let) => {
            collect_expr_externs(&expr_let.expr, functions, externs, visited_functions, used)
        }
        ExprKind::Invoke(invoke) => {
            match &invoke.target {
                ExprInvokeTarget::Function(name) => {
                    if let Some(ident) = name.as_ident() {
                        collect_call_externs(
                            ident.as_str(),
                            functions,
                            externs,
                            visited_functions,
                            used,
                        );
                    }
                }
                ExprInvokeTarget::Expr(target) => {
                    collect_expr_externs(target, functions, externs, visited_functions, used);
                }
                ExprInvokeTarget::Method(method) => {
                    collect_expr_externs(&method.obj, functions, externs, visited_functions, used);
                }
                ExprInvokeTarget::Type(_)
                | ExprInvokeTarget::Closure(_)
                | ExprInvokeTarget::BinOp(_) => {}
            }
            for arg in &invoke.args {
                collect_expr_externs(arg, functions, externs, visited_functions, used);
            }
            for kwarg in &invoke.kwargs {
                collect_expr_externs(&kwarg.value, functions, externs, visited_functions, used);
            }
        }
        ExprKind::Match(ExprMatch {
            scrutinee, cases, ..
        }) => {
            if let Some(scrutinee) = scrutinee {
                collect_expr_externs(scrutinee, functions, externs, visited_functions, used);
            }
            for ExprMatchCase {
                pat,
                cond,
                guard,
                body,
                ..
            } in cases
            {
                if let Some(pat) = pat {
                    if let PatternKind::Variant(variant) = pat.kind() {
                        collect_expr_externs(
                            &variant.name,
                            functions,
                            externs,
                            visited_functions,
                            used,
                        );
                    }
                }
                collect_expr_externs(cond, functions, externs, visited_functions, used);
                if let Some(guard) = guard {
                    collect_expr_externs(guard, functions, externs, visited_functions, used);
                }
                collect_expr_externs(body, functions, externs, visited_functions, used);
            }
        }
        ExprKind::Any(any) => {
            if let Some(value) = any.downcast_ref::<ValueExpr>() {
                collect_value_externs(value, functions, externs, visited_functions, used);
            } else if let Some(condition) = any.downcast_ref::<ConditionExpr>() {
                collect_condition_externs(condition, functions, externs, visited_functions, used);
            }
        }
        _ => {}
    }
}
