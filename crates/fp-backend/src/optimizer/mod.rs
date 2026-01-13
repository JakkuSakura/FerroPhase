use std::collections::HashMap;

use fp_core::error::Error;
use fp_core::mir;
use fp_core::query::{QueryDocument, QueryKind, SqlDialect};

use crate::error::optimization_error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirPassName {
    ConstFold,
    RemoveNop,
}

impl MirPassName {
    pub fn as_str(&self) -> &'static str {
        match self {
            MirPassName::ConstFold => "const_fold",
            MirPassName::RemoveNop => "remove_nop",
        }
    }

    pub fn from_ident(ident: &str) -> Option<Self> {
        match ident.trim().to_ascii_lowercase().as_str() {
            "const_fold" | "const-fold" => Some(MirPassName::ConstFold),
            "remove_nop" | "remove-nop" => Some(MirPassName::RemoveNop),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct OptimizationPlan {
    pub passes: Vec<MirPassName>,
    pub query: Option<QueryDocument>,
}

impl OptimizationPlan {
    pub fn empty() -> Self {
        Self {
            passes: Vec::new(),
            query: None,
        }
    }

    pub fn for_level(level: u8) -> Self {
        let query = match level {
            0 => None,
            1 => Some("SELECT const_fold FROM mir"),
            _ => Some("SELECT const_fold, remove_nop FROM mir"),
        };

        match query {
            Some(text) => {
                let query = QueryDocument::sql(text, SqlDialect::Generic);
                Self::from_query(query).unwrap_or_else(|_| Self::empty())
            }
            None => Self::empty(),
        }
    }

    pub fn from_query(query: QueryDocument) -> Result<Self, Error> {
        let passes = parse_passes_from_query(&query)?;
        Ok(Self {
            passes,
            query: Some(query),
        })
    }

    pub fn is_empty(&self) -> bool {
        self.passes.is_empty()
    }
}

#[derive(Debug, Default, Clone)]
pub struct OptimizationReport {
    pub total_changes: usize,
    pub per_pass: Vec<(MirPassName, usize)>,
}

pub trait MirPass {
    fn name(&self) -> MirPassName;
    fn run(&self, program: &mut mir::Program) -> Result<usize, Error>;
}

#[derive(Default)]
pub struct MirOptimizer {
    registry: HashMap<MirPassName, Box<dyn MirPass>>,
}

impl MirOptimizer {
    pub fn new() -> Self {
        let mut optimizer = Self {
            registry: HashMap::new(),
        };
        optimizer.register(Box::new(ConstFoldPass));
        optimizer.register(Box::new(RemoveNopPass));
        optimizer
    }

    pub fn register(&mut self, pass: Box<dyn MirPass>) {
        self.registry.insert(pass.name(), pass);
    }

    pub fn apply_plan(
        &self,
        program: &mut mir::Program,
        plan: &OptimizationPlan,
    ) -> Result<OptimizationReport, Error> {
        let mut report = OptimizationReport::default();

        for pass_name in &plan.passes {
            let pass = self.registry.get(pass_name).ok_or_else(|| {
                optimization_error(format!("unknown MIR optimization pass: {}", pass_name.as_str()))
            })?;
            let changes = pass.run(program)?;
            report.total_changes += changes;
            report.per_pass.push((*pass_name, changes));
        }

        Ok(report)
    }
}

fn parse_passes_from_query(query: &QueryDocument) -> Result<Vec<MirPassName>, Error> {
    let mut passes = Vec::new();

    match &query.kind {
        QueryKind::Sql(sql) => {
            for statement in &sql.statements {
                parse_sql_statement(&statement.text, &mut passes)?;
            }
        }
        QueryKind::Prql(prql) => {
            parse_prql_pipeline(&prql.pipeline, &mut passes)?;
        }
        QueryKind::Any(_) => {
            return Err(optimization_error(
                "unsupported query payload for MIR optimizer",
            ));
        }
    }

    Ok(passes)
}

fn parse_sql_statement(statement: &str, passes: &mut Vec<MirPassName>) -> Result<(), Error> {
    let trimmed = statement.trim().trim_end_matches(';').trim();
    if trimmed.is_empty() {
        return Ok(());
    }

    let lower = trimmed.to_ascii_lowercase();
    if !lower.starts_with("select") {
        return Err(optimization_error(format!(
            "unsupported MIR optimization query: {trimmed}"
        )));
    }

    let select_len = "select".len();
    let from_idx = lower.find(" from ");
    let selection = match from_idx {
        Some(idx) => &trimmed[select_len..idx],
        None => &trimmed[select_len..],
    };

    parse_pass_list(selection, passes)
}

fn parse_prql_pipeline(pipeline: &str, passes: &mut Vec<MirPassName>) -> Result<(), Error> {
    parse_pass_list(pipeline, passes)
}

fn parse_pass_list(raw: &str, passes: &mut Vec<MirPassName>) -> Result<(), Error> {
    for token in raw.split(|ch: char| ch == ',' || ch.is_whitespace() || ch == '|') {
        let ident = token.trim().trim_matches('"').trim_matches('`');
        if ident.is_empty() || ident == "*" {
            continue;
        }
        let pass = MirPassName::from_ident(ident).ok_or_else(|| {
            optimization_error(format!("unknown MIR optimization pass: {ident}"))
        })?;
        passes.push(pass);
    }
    Ok(())
}

struct ConstFoldPass;

impl MirPass for ConstFoldPass {
    fn name(&self) -> MirPassName {
        MirPassName::ConstFold
    }

    fn run(&self, program: &mut mir::Program) -> Result<usize, Error> {
        let mut changes = 0;

        for body in program.bodies.values_mut() {
            for block in &mut body.basic_blocks {
                for stmt in &mut block.statements {
                    let mir::StatementKind::Assign(_, rvalue) = &mut stmt.kind else {
                        continue;
                    };
                    let Some(new_rvalue) = fold_rvalue(rvalue, stmt.source_info) else {
                        continue;
                    };
                    *rvalue = new_rvalue;
                    changes += 1;
                }
            }
        }

        Ok(changes)
    }
}

struct RemoveNopPass;

impl MirPass for RemoveNopPass {
    fn name(&self) -> MirPassName {
        MirPassName::RemoveNop
    }

    fn run(&self, program: &mut mir::Program) -> Result<usize, Error> {
        let mut changes = 0;

        for body in program.bodies.values_mut() {
            for block in &mut body.basic_blocks {
                let before = block.statements.len();
                block
                    .statements
                    .retain(|stmt| !matches!(stmt.kind, mir::StatementKind::Nop));
                changes += before - block.statements.len();
            }
        }

        Ok(changes)
    }
}

fn fold_rvalue(rvalue: &mir::Rvalue, span: mir::Span) -> Option<mir::Rvalue> {
    match rvalue {
        mir::Rvalue::BinaryOp(bin_op, lhs, rhs) => {
            let lhs_kind = operand_constant_kind(lhs)?;
            let rhs_kind = operand_constant_kind(rhs)?;
            let result = eval_binary_op(bin_op.clone(), lhs_kind, rhs_kind)?;
            Some(mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                span,
                user_ty: None,
                literal: result,
            })))
        }
        mir::Rvalue::UnaryOp(un_op, operand) => {
            let value = operand_constant_kind(operand)?;
            let result = eval_unary_op(un_op.clone(), value)?;
            Some(mir::Rvalue::Use(mir::Operand::Constant(mir::Constant {
                span,
                user_ty: None,
                literal: result,
            })))
        }
        _ => None,
    }
}

fn operand_constant_kind(operand: &mir::Operand) -> Option<&mir::ConstantKind> {
    match operand {
        mir::Operand::Constant(constant) => Some(&constant.literal),
        _ => None,
    }
}

fn eval_binary_op(
    op: mir::BinOp,
    lhs: &mir::ConstantKind,
    rhs: &mir::ConstantKind,
) -> Option<mir::ConstantKind> {
    match (lhs, rhs) {
        (mir::ConstantKind::Int(a), mir::ConstantKind::Int(b)) => {
            eval_binary_i64(op, *a, *b)
        }
        (mir::ConstantKind::UInt(a), mir::ConstantKind::UInt(b)) => {
            eval_binary_u64(op, *a, *b)
        }
        (mir::ConstantKind::Bool(a), mir::ConstantKind::Bool(b)) => {
            eval_binary_bool(op, *a, *b)
        }
        (mir::ConstantKind::Float(a), mir::ConstantKind::Float(b)) => {
            eval_binary_f64(op, *a, *b)
        }
        _ => None,
    }
}

fn eval_binary_i64(op: mir::BinOp, lhs: i64, rhs: i64) -> Option<mir::ConstantKind> {
    use mir::BinOp;
    let result = match op {
        BinOp::Add => mir::ConstantKind::Int(lhs.wrapping_add(rhs)),
        BinOp::Sub => mir::ConstantKind::Int(lhs.wrapping_sub(rhs)),
        BinOp::Mul => mir::ConstantKind::Int(lhs.wrapping_mul(rhs)),
        BinOp::Div => {
            if rhs == 0 {
                return None;
            }
            mir::ConstantKind::Int(lhs / rhs)
        }
        BinOp::Rem => {
            if rhs == 0 {
                return None;
            }
            mir::ConstantKind::Int(lhs % rhs)
        }
        BinOp::BitAnd => mir::ConstantKind::Int(lhs & rhs),
        BinOp::BitOr => mir::ConstantKind::Int(lhs | rhs),
        BinOp::BitXor => mir::ConstantKind::Int(lhs ^ rhs),
        BinOp::Shl => {
            let shift = u32::try_from(rhs).ok()?;
            mir::ConstantKind::Int(lhs.wrapping_shl(shift))
        }
        BinOp::Shr => {
            let shift = u32::try_from(rhs).ok()?;
            mir::ConstantKind::Int(lhs.wrapping_shr(shift))
        }
        BinOp::Eq => mir::ConstantKind::Bool(lhs == rhs),
        BinOp::Ne => mir::ConstantKind::Bool(lhs != rhs),
        BinOp::Lt => mir::ConstantKind::Bool(lhs < rhs),
        BinOp::Le => mir::ConstantKind::Bool(lhs <= rhs),
        BinOp::Gt => mir::ConstantKind::Bool(lhs > rhs),
        BinOp::Ge => mir::ConstantKind::Bool(lhs >= rhs),
        _ => return None,
    };
    Some(result)
}

fn eval_binary_u64(op: mir::BinOp, lhs: u64, rhs: u64) -> Option<mir::ConstantKind> {
    use mir::BinOp;
    let result = match op {
        BinOp::Add => mir::ConstantKind::UInt(lhs.wrapping_add(rhs)),
        BinOp::Sub => mir::ConstantKind::UInt(lhs.wrapping_sub(rhs)),
        BinOp::Mul => mir::ConstantKind::UInt(lhs.wrapping_mul(rhs)),
        BinOp::Div => {
            if rhs == 0 {
                return None;
            }
            mir::ConstantKind::UInt(lhs / rhs)
        }
        BinOp::Rem => {
            if rhs == 0 {
                return None;
            }
            mir::ConstantKind::UInt(lhs % rhs)
        }
        BinOp::BitAnd => mir::ConstantKind::UInt(lhs & rhs),
        BinOp::BitOr => mir::ConstantKind::UInt(lhs | rhs),
        BinOp::BitXor => mir::ConstantKind::UInt(lhs ^ rhs),
        BinOp::Shl => {
            let shift = u32::try_from(rhs).ok()?;
            mir::ConstantKind::UInt(lhs.wrapping_shl(shift))
        }
        BinOp::Shr => {
            let shift = u32::try_from(rhs).ok()?;
            mir::ConstantKind::UInt(lhs.wrapping_shr(shift))
        }
        BinOp::Eq => mir::ConstantKind::Bool(lhs == rhs),
        BinOp::Ne => mir::ConstantKind::Bool(lhs != rhs),
        BinOp::Lt => mir::ConstantKind::Bool(lhs < rhs),
        BinOp::Le => mir::ConstantKind::Bool(lhs <= rhs),
        BinOp::Gt => mir::ConstantKind::Bool(lhs > rhs),
        BinOp::Ge => mir::ConstantKind::Bool(lhs >= rhs),
        _ => return None,
    };
    Some(result)
}

fn eval_binary_bool(op: mir::BinOp, lhs: bool, rhs: bool) -> Option<mir::ConstantKind> {
    use mir::BinOp;
    let result = match op {
        BinOp::And => mir::ConstantKind::Bool(lhs && rhs),
        BinOp::Or => mir::ConstantKind::Bool(lhs || rhs),
        BinOp::Eq => mir::ConstantKind::Bool(lhs == rhs),
        BinOp::Ne => mir::ConstantKind::Bool(lhs != rhs),
        _ => return None,
    };
    Some(result)
}

fn eval_binary_f64(op: mir::BinOp, lhs: f64, rhs: f64) -> Option<mir::ConstantKind> {
    use mir::BinOp;
    let result = match op {
        BinOp::Add => mir::ConstantKind::Float(lhs + rhs),
        BinOp::Sub => mir::ConstantKind::Float(lhs - rhs),
        BinOp::Mul => mir::ConstantKind::Float(lhs * rhs),
        BinOp::Div => mir::ConstantKind::Float(lhs / rhs),
        BinOp::Eq => mir::ConstantKind::Bool(lhs == rhs),
        BinOp::Ne => mir::ConstantKind::Bool(lhs != rhs),
        BinOp::Lt => mir::ConstantKind::Bool(lhs < rhs),
        BinOp::Le => mir::ConstantKind::Bool(lhs <= rhs),
        BinOp::Gt => mir::ConstantKind::Bool(lhs > rhs),
        BinOp::Ge => mir::ConstantKind::Bool(lhs >= rhs),
        _ => return None,
    };
    Some(result)
}

fn eval_unary_op(op: mir::UnOp, value: &mir::ConstantKind) -> Option<mir::ConstantKind> {
    match value {
        mir::ConstantKind::Int(v) => match op {
            mir::UnOp::Not => Some(mir::ConstantKind::Int(!v)),
            mir::UnOp::Neg => Some(mir::ConstantKind::Int(v.wrapping_neg())),
        },
        mir::ConstantKind::UInt(v) => match op {
            mir::UnOp::Not => Some(mir::ConstantKind::UInt(!v)),
            mir::UnOp::Neg => None,
        },
        mir::ConstantKind::Bool(v) => match op {
            mir::UnOp::Not => Some(mir::ConstantKind::Bool(!v)),
            mir::UnOp::Neg => None,
        },
        mir::ConstantKind::Float(v) => match op {
            mir::UnOp::Not => None,
            mir::UnOp::Neg => Some(mir::ConstantKind::Float(-v)),
        },
        _ => None,
    }
}
