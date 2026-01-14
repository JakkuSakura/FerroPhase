use std::collections::HashMap;

use fp_core::error::Error;
use fp_core::mir;
use fp_core::query::{QueryDocument, QueryKind, SqlDialect};

use crate::error::optimization_error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirPassName {
    ConstFold,
    ConstPropagate,
    CopyPropagate,
    DeadStore,
    SimplifyBranches,
    RemoveStorage,
    RemoveNop,
}

impl MirPassName {
    pub fn as_str(&self) -> &'static str {
        match self {
            MirPassName::ConstFold => "const_fold",
            MirPassName::ConstPropagate => "const_propagate",
            MirPassName::CopyPropagate => "copy_propagate",
            MirPassName::DeadStore => "dead_store",
            MirPassName::SimplifyBranches => "simplify_branches",
            MirPassName::RemoveStorage => "remove_storage",
            MirPassName::RemoveNop => "remove_nop",
        }
    }

    pub fn from_ident(ident: &str) -> Option<Self> {
        match ident.trim().to_ascii_lowercase().as_str() {
            "const_fold" | "const-fold" => Some(MirPassName::ConstFold),
            "const_propagate" | "const-propagate" => Some(MirPassName::ConstPropagate),
            "copy_propagate" | "copy-propagate" => Some(MirPassName::CopyPropagate),
            "dead_store" | "dead-store" => Some(MirPassName::DeadStore),
            "simplify_branches" | "simplify-branches" => Some(MirPassName::SimplifyBranches),
            "remove_storage" | "remove-storage" => Some(MirPassName::RemoveStorage),
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
            1 => Some("SELECT const_fold, const_propagate FROM mir"),
            _ => Some(
                "SELECT const_fold, const_propagate, copy_propagate, simplify_branches, dead_store, remove_storage, remove_nop FROM mir",
            ),
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
    fn run(
        &self,
        program: &mut mir::Program,
        engine: &mut MirQueryEngine,
    ) -> Result<usize, Error>;
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
        optimizer.register(Box::new(ConstPropagatePass));
        optimizer.register(Box::new(CopyPropagatePass));
        optimizer.register(Box::new(SimplifyBranchesPass));
        optimizer.register(Box::new(DeadStorePass));
        optimizer.register(Box::new(RemoveStoragePass));
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
            let mut engine = MirQueryEngine::new();
            let changes = pass.run(program, &mut engine)?;
            report.total_changes += changes;
            report.per_pass.push((*pass_name, changes));
        }

        Ok(report)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ValueState {
    Unknown,
    Const(mir::ConstantKind),
    Alias(mir::LocalId),
}

#[derive(Debug, Clone)]
pub(crate) struct BodyPropagation {
    in_states: Vec<Vec<ValueState>>,
    #[allow(dead_code)]
    out_states: Vec<Vec<ValueState>>,
}

#[derive(Debug, Clone)]
pub(crate) struct BodyLiveness {
    #[allow(dead_code)]
    in_states: Vec<Vec<bool>>,
    out_states: Vec<Vec<bool>>,
}

#[derive(Debug, Default)]
pub struct MirQueryEngine {
    propagation: HashMap<mir::BodyId, BodyPropagation>,
    liveness: HashMap<mir::BodyId, BodyLiveness>,
}

impl MirQueryEngine {
    pub fn new() -> Self {
        Self::default()
    }

    pub(crate) fn propagation(
        &mut self,
        program: &mir::Program,
        body_id: mir::BodyId,
    ) -> Result<&BodyPropagation, Error> {
        if !self.propagation.contains_key(&body_id) {
            let body = program.bodies.get(&body_id).ok_or_else(|| {
                optimization_error(format!("missing MIR body for {body_id:?}"))
            })?;
            let analysis = compute_propagation(body);
            self.propagation.insert(body_id, analysis);
        }
        Ok(self
            .propagation
            .get(&body_id)
            .expect("propagation cached"))
    }

    pub(crate) fn liveness(
        &mut self,
        program: &mir::Program,
        body_id: mir::BodyId,
    ) -> Result<&BodyLiveness, Error> {
        if !self.liveness.contains_key(&body_id) {
            let body = program.bodies.get(&body_id).ok_or_else(|| {
                optimization_error(format!("missing MIR body for {body_id:?}"))
            })?;
            let analysis = compute_liveness(body);
            self.liveness.insert(body_id, analysis);
        }
        Ok(self.liveness.get(&body_id).expect("liveness cached"))
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

fn compute_propagation(body: &mir::Body) -> BodyPropagation {
    let block_count = body.basic_blocks.len();
    let locals = body.locals.len();
    let mut in_states = vec![vec![ValueState::Unknown; locals]; block_count];
    let mut out_states = vec![vec![ValueState::Unknown; locals]; block_count];

    let preds = compute_predecessors(body);
    let mut changed = true;

    while changed {
        changed = false;
        for (block_idx, block) in body.basic_blocks.iter().enumerate() {
            let in_state = if preds[block_idx].is_empty() {
                vec![ValueState::Unknown; locals]
            } else {
                let mut state = out_states[preds[block_idx][0] as usize].clone();
                for pred in preds[block_idx].iter().skip(1) {
                    meet_state(&mut state, &out_states[*pred as usize]);
                }
                state
            };

            let mut state = in_state.clone();
            for stmt in &block.statements {
                transfer_statement(&mut state, stmt);
            }
            if let Some(term) = &block.terminator {
                transfer_terminator(&mut state, term);
            }

            if in_state != in_states[block_idx] {
                in_states[block_idx] = in_state;
                changed = true;
            }
            if state != out_states[block_idx] {
                out_states[block_idx] = state;
                changed = true;
            }
        }
    }

    BodyPropagation {
        in_states,
        out_states,
    }
}

fn compute_liveness(body: &mir::Body) -> BodyLiveness {
    let block_count = body.basic_blocks.len();
    let locals = body.locals.len();
    let mut in_states = vec![vec![false; locals]; block_count];
    let mut out_states = vec![vec![false; locals]; block_count];

    let mut changed = true;

    let mut block_uses = vec![vec![false; locals]; block_count];
    let mut block_defs = vec![vec![false; locals]; block_count];

    for (block_idx, block) in body.basic_blocks.iter().enumerate() {
        let mut defs = vec![false; locals];
        let mut uses = vec![false; locals];

        for stmt in &block.statements {
            collect_statement_uses(stmt, &mut uses, &defs);
            collect_statement_defs(stmt, &mut defs);
        }
        if let Some(term) = &block.terminator {
            let mut term_uses = vec![false; locals];
            let empty_defs = vec![false; locals];
            collect_terminator_uses(term, &mut term_uses, &empty_defs);
            for (idx, used) in term_uses.iter().enumerate() {
                if *used {
                    uses[idx] = true;
                }
            }
        }

        block_uses[block_idx] = uses;
        block_defs[block_idx] = defs;
    }

    while changed {
        changed = false;

        for block_idx in (0..block_count).rev() {
            let mut out_state = vec![false; locals];
            for succ in successors_of(body, block_idx) {
                let succ_idx = succ as usize;
                let succ_in = &in_states[succ_idx];
                for (idx, live) in succ_in.iter().enumerate() {
                    if *live {
                        out_state[idx] = true;
                    }
                }
            }

            let mut in_state = block_uses[block_idx].clone();
            for (idx, live) in out_state.iter().enumerate() {
                if *live && !block_defs[block_idx][idx] {
                    in_state[idx] = true;
                }
            }

            if out_state != out_states[block_idx] {
                out_states[block_idx] = out_state;
                changed = true;
            }
            if in_state != in_states[block_idx] {
                in_states[block_idx] = in_state;
                changed = true;
            }
        }
    }

    BodyLiveness {
        in_states,
        out_states,
    }
}

fn compute_predecessors(body: &mir::Body) -> Vec<Vec<mir::BasicBlockId>> {
    let mut preds = vec![Vec::new(); body.basic_blocks.len()];
    for (idx, block) in body.basic_blocks.iter().enumerate() {
        let Some(term) = &block.terminator else {
            continue;
        };
        for succ in successors_of(body, idx) {
            preds[succ as usize].push(idx as mir::BasicBlockId);
        }
        match &term.kind {
            mir::TerminatorKind::Call { cleanup, .. }
            | mir::TerminatorKind::Drop { unwind: cleanup, .. }
            | mir::TerminatorKind::DropAndReplace { unwind: cleanup, .. }
            | mir::TerminatorKind::Assert { cleanup, .. }
            | mir::TerminatorKind::Yield { drop: cleanup, .. }
            | mir::TerminatorKind::InlineAsm { cleanup, .. }
            | mir::TerminatorKind::FalseUnwind { unwind: cleanup, .. } => {
                if let Some(cleanup) = cleanup {
                    preds[*cleanup as usize].push(idx as mir::BasicBlockId);
                }
            }
            _ => {}
        }
    }
    preds
}

fn successors_of(body: &mir::Body, block_idx: usize) -> Vec<mir::BasicBlockId> {
    let mut successors = Vec::new();
    let Some(term) = &body.basic_blocks[block_idx].terminator else {
        return successors;
    };
    match &term.kind {
        mir::TerminatorKind::Goto { target } => successors.push(*target),
        mir::TerminatorKind::SwitchInt { targets, .. } => {
            successors.extend(targets.targets.iter().copied());
            successors.push(targets.otherwise);
        }
        mir::TerminatorKind::Drop { target, .. } => successors.push(*target),
        mir::TerminatorKind::DropAndReplace { target, .. } => successors.push(*target),
        mir::TerminatorKind::Call { destination, .. } => {
            if let Some((_, target)) = destination {
                successors.push(*target);
            }
        }
        mir::TerminatorKind::Assert { target, .. } => successors.push(*target),
        mir::TerminatorKind::Yield { resume, .. } => successors.push(*resume),
        mir::TerminatorKind::FalseEdge { real_target, .. } => successors.push(*real_target),
        mir::TerminatorKind::FalseUnwind { real_target, .. } => successors.push(*real_target),
        mir::TerminatorKind::InlineAsm { destination, .. } => {
            if let Some(target) = destination {
                successors.push(*target);
            }
        }
        _ => {}
    }
    successors
}

fn meet_state(into: &mut Vec<ValueState>, other: &[ValueState]) {
    for (idx, state) in into.iter_mut().enumerate() {
        if *state != other[idx] {
            *state = ValueState::Unknown;
        }
    }
}

fn transfer_statement(state: &mut [ValueState], stmt: &mir::Statement) {
    match &stmt.kind {
        mir::StatementKind::Assign(place, rvalue) => {
            invalidate_place(state, place);
            if place.projection.is_empty() {
                if let Some(value) = value_from_rvalue(rvalue) {
                    state[place.local as usize] = value;
                }
            }
        }
        mir::StatementKind::StorageDead(local) => {
            state[*local as usize] = ValueState::Unknown;
        }
        mir::StatementKind::SetDiscriminant { place, .. }
        | mir::StatementKind::Retag(_, place)
        | mir::StatementKind::AscribeUserType(place, _, _) => {
            invalidate_place(state, place);
        }
        _ => {}
    }
}

fn transfer_terminator(state: &mut [ValueState], term: &mir::Terminator) {
    match &term.kind {
        mir::TerminatorKind::Call { destination, .. } => {
            if let Some((place, _)) = destination {
                invalidate_place(state, place);
            }
        }
        mir::TerminatorKind::DropAndReplace { place, .. } => {
            invalidate_place(state, place);
        }
        _ => {}
    }
}

fn invalidate_place(state: &mut [ValueState], place: &mir::Place) {
    state[place.local as usize] = ValueState::Unknown;
}

fn value_from_rvalue(rvalue: &mir::Rvalue) -> Option<ValueState> {
    match rvalue {
        mir::Rvalue::Use(operand) => match operand {
            mir::Operand::Constant(constant) => Some(ValueState::Const(constant.literal.clone())),
            mir::Operand::Copy(place) if place.projection.is_empty() => {
                Some(ValueState::Alias(place.local))
            }
            _ => None,
        },
        _ => None,
    }
}

fn collect_statement_uses(stmt: &mir::Statement, uses: &mut [bool], defs: &[bool]) {
    match &stmt.kind {
        mir::StatementKind::Assign(place, rvalue) => {
            if !place.projection.is_empty() {
                uses[place.local as usize] = true;
            }
            collect_rvalue_uses(rvalue, uses, defs);
        }
        mir::StatementKind::IntrinsicCall { args, .. } => {
            for arg in args {
                collect_operand_uses(arg, uses, defs);
            }
        }
        mir::StatementKind::SetDiscriminant { place, .. }
        | mir::StatementKind::Retag(_, place)
        | mir::StatementKind::AscribeUserType(place, _, _) => {
            uses[place.local as usize] = true;
        }
        _ => {}
    }
}

fn collect_statement_defs(stmt: &mir::Statement, defs: &mut [bool]) {
    if let mir::StatementKind::Assign(place, _) = &stmt.kind {
        if place.projection.is_empty() {
            defs[place.local as usize] = true;
        }
    }
}

fn collect_rvalue_uses(rvalue: &mir::Rvalue, uses: &mut [bool], defs: &[bool]) {
    match rvalue {
        mir::Rvalue::Use(op) => collect_operand_uses(op, uses, defs),
        mir::Rvalue::BinaryOp(_, lhs, rhs) | mir::Rvalue::CheckedBinaryOp(_, lhs, rhs) => {
            collect_operand_uses(lhs, uses, defs);
            collect_operand_uses(rhs, uses, defs);
        }
        mir::Rvalue::UnaryOp(_, operand)
        | mir::Rvalue::Repeat(operand, _)
        | mir::Rvalue::Cast(_, operand, _)
        | mir::Rvalue::ContainerLen { container: operand, .. }
        | mir::Rvalue::ContainerGet { container: operand, .. }
        | mir::Rvalue::ShallowInitBox(operand, _) => {
            collect_operand_uses(operand, uses, defs);
        }
        mir::Rvalue::Ref(_, _, place)
        | mir::Rvalue::AddressOf(_, place)
        | mir::Rvalue::Len(place)
        | mir::Rvalue::Discriminant(place) => {
            uses[place.local as usize] = true;
        }
        mir::Rvalue::Aggregate(_, operands) => {
            for operand in operands {
                collect_operand_uses(operand, uses, defs);
            }
        }
        mir::Rvalue::ContainerLiteral { elements, .. } => {
            for operand in elements {
                collect_operand_uses(operand, uses, defs);
            }
        }
        mir::Rvalue::ContainerMapLiteral { entries, .. } => {
            for (key, value) in entries {
                collect_operand_uses(key, uses, defs);
                collect_operand_uses(value, uses, defs);
            }
        }
        mir::Rvalue::IntrinsicCall { args, .. } => {
            for arg in args {
                collect_operand_uses(arg, uses, defs);
            }
        }
        _ => {}
    }
}

fn collect_operand_uses(operand: &mir::Operand, uses: &mut [bool], defs: &[bool]) {
    match operand {
        mir::Operand::Copy(place) | mir::Operand::Move(place) => {
            if !defs[place.local as usize] {
                uses[place.local as usize] = true;
            }
        }
        _ => {}
    }
}

fn collect_terminator_uses(term: &mir::Terminator, uses: &mut [bool], defs: &[bool]) {
    match &term.kind {
        mir::TerminatorKind::SwitchInt { discr, .. } => {
            collect_operand_uses(discr, uses, defs);
        }
        mir::TerminatorKind::Call { func, args, .. } => {
            collect_operand_uses(func, uses, defs);
            for arg in args {
                collect_operand_uses(arg, uses, defs);
            }
        }
        mir::TerminatorKind::Drop { place, .. }
        | mir::TerminatorKind::DropAndReplace { place, .. } => {
            uses[place.local as usize] = true;
        }
        mir::TerminatorKind::Assert { cond, .. } => {
            collect_operand_uses(cond, uses, defs);
        }
        mir::TerminatorKind::Yield { value, .. } => {
            collect_operand_uses(value, uses, defs);
        }
        _ => {}
    }
}

struct ConstFoldPass;

impl MirPass for ConstFoldPass {
    fn name(&self) -> MirPassName {
        MirPassName::ConstFold
    }

    fn run(
        &self,
        program: &mut mir::Program,
        _engine: &mut MirQueryEngine,
    ) -> Result<usize, Error> {
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

struct ConstPropagatePass;

impl MirPass for ConstPropagatePass {
    fn name(&self) -> MirPassName {
        MirPassName::ConstPropagate
    }

    fn run(
        &self,
        program: &mut mir::Program,
        engine: &mut MirQueryEngine,
    ) -> Result<usize, Error> {
        let mut changes = 0;

        let body_ids: Vec<_> = program.bodies.keys().copied().collect();
        for body_id in body_ids {
            let analysis = engine.propagation(program, body_id)?;
            let body = program
                .bodies
                .get_mut(&body_id)
                .ok_or_else(|| optimization_error("missing MIR body for propagation"))?;
            for (block_idx, block) in body.basic_blocks.iter_mut().enumerate() {
                let mut state = analysis.in_states[block_idx].clone();
                for stmt in &mut block.statements {
                    changes += rewrite_statement_operands(
                        stmt,
                        &state,
                        ReplacementPolicy::ConstOnly,
                    );
                    transfer_statement(&mut state, stmt);
                }
                if let Some(term) = &mut block.terminator {
                    changes += rewrite_terminator_operands(
                        term,
                        &state,
                        ReplacementPolicy::ConstOnly,
                    );
                    transfer_terminator(&mut state, term);
                }
            }
        }

        Ok(changes)
    }
}

struct CopyPropagatePass;

impl MirPass for CopyPropagatePass {
    fn name(&self) -> MirPassName {
        MirPassName::CopyPropagate
    }

    fn run(
        &self,
        program: &mut mir::Program,
        engine: &mut MirQueryEngine,
    ) -> Result<usize, Error> {
        let mut changes = 0;

        let body_ids: Vec<_> = program.bodies.keys().copied().collect();
        for body_id in body_ids {
            let analysis = engine.propagation(program, body_id)?;
            let body = program
                .bodies
                .get_mut(&body_id)
                .ok_or_else(|| optimization_error("missing MIR body for propagation"))?;
            for (block_idx, block) in body.basic_blocks.iter_mut().enumerate() {
                let mut state = analysis.in_states[block_idx].clone();
                for stmt in &mut block.statements {
                    changes += rewrite_statement_operands(
                        stmt,
                        &state,
                        ReplacementPolicy::AliasCopy,
                    );
                    transfer_statement(&mut state, stmt);
                }
                if let Some(term) = &mut block.terminator {
                    changes += rewrite_terminator_operands(
                        term,
                        &state,
                        ReplacementPolicy::AliasCopy,
                    );
                    transfer_terminator(&mut state, term);
                }
            }
        }

        Ok(changes)
    }
}

struct SimplifyBranchesPass;

impl MirPass for SimplifyBranchesPass {
    fn name(&self) -> MirPassName {
        MirPassName::SimplifyBranches
    }

    fn run(
        &self,
        program: &mut mir::Program,
        engine: &mut MirQueryEngine,
    ) -> Result<usize, Error> {
        let mut changes = 0;

        let body_ids: Vec<_> = program.bodies.keys().copied().collect();
        for body_id in body_ids {
            let analysis = engine.propagation(program, body_id)?;
            let body = program
                .bodies
                .get_mut(&body_id)
                .ok_or_else(|| optimization_error("missing MIR body for propagation"))?;
            for (block_idx, block) in body.basic_blocks.iter_mut().enumerate() {
                let state = &analysis.in_states[block_idx];
                let Some(term) = &mut block.terminator else {
                    continue;
                };
                let replacement = match &term.kind {
                    mir::TerminatorKind::SwitchInt { discr, targets, .. } => {
                        let constant = operand_to_constant(discr, state);
                        constant_to_u128(constant.as_ref()).map(|value| {
                            let mut selected = targets.otherwise;
                            for (idx, candidate) in targets.values.iter().enumerate() {
                                if *candidate == value {
                                    selected = targets.targets[idx];
                                    break;
                                }
                            }
                            mir::TerminatorKind::Goto { target: selected }
                        })
                    }
                    mir::TerminatorKind::Assert {
                        cond,
                        expected,
                        target,
                        ..
                    } => {
                        let constant = operand_to_constant(cond, state);
                        match constant {
                            Some(mir::ConstantKind::Bool(value)) if value == *expected => {
                                Some(mir::TerminatorKind::Goto { target: *target })
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                };

                if let Some(new_kind) = replacement {
                    term.kind = new_kind;
                    changes += 1;
                }
            }
        }

        Ok(changes)
    }
}

struct DeadStorePass;

impl MirPass for DeadStorePass {
    fn name(&self) -> MirPassName {
        MirPassName::DeadStore
    }

    fn run(
        &self,
        program: &mut mir::Program,
        engine: &mut MirQueryEngine,
    ) -> Result<usize, Error> {
        let mut changes = 0;

        let body_ids: Vec<_> = program.bodies.keys().copied().collect();
        for body_id in body_ids {
            let analysis = engine.liveness(program, body_id)?;
            let body = program
                .bodies
                .get_mut(&body_id)
                .ok_or_else(|| optimization_error("missing MIR body for liveness"))?;
            let arg_count = body.arg_count;
            let local_count = body.locals.len();
            let local_kinds = (0..local_count)
                .map(|idx| {
                    let local = idx as mir::LocalId;
                    if local == 0 {
                        mir::LocalKind::ReturnPointer
                    } else if (local as usize) <= arg_count {
                        mir::LocalKind::Arg
                    } else {
                        mir::LocalKind::Temp
                    }
                })
                .collect::<Vec<_>>();
            for (block_idx, block) in body.basic_blocks.iter_mut().enumerate() {
                let mut live = analysis.out_states[block_idx].clone();
                for stmt in block.statements.iter_mut().rev() {
                    let mut uses = vec![false; body.locals.len()];
                    let mut defs = vec![false; body.locals.len()];
                    collect_statement_uses(stmt, &mut uses, &defs);
                    collect_statement_defs(stmt, &mut defs);

                    let removable = if let mir::StatementKind::Assign(place, rvalue) = &stmt.kind {
                        place.projection.is_empty()
                            && !live[place.local as usize]
                            && matches!(local_kinds[place.local as usize], mir::LocalKind::Temp)
                            && is_pure_rvalue(rvalue)
                    } else {
                        false
                    };

                    if removable {
                        stmt.kind = mir::StatementKind::Nop;
                        changes += 1;
                        continue;
                    }

                    for (idx, def) in defs.iter().enumerate() {
                        if *def {
                            live[idx] = false;
                        }
                    }
                    for (idx, used) in uses.iter().enumerate() {
                        if *used {
                            live[idx] = true;
                        }
                    }
                }
            }
        }

        Ok(changes)
    }
}

struct RemoveStoragePass;

impl MirPass for RemoveStoragePass {
    fn name(&self) -> MirPassName {
        MirPassName::RemoveStorage
    }

    fn run(
        &self,
        program: &mut mir::Program,
        _engine: &mut MirQueryEngine,
    ) -> Result<usize, Error> {
        let mut changes = 0;

        for body in program.bodies.values_mut() {
            for block in &mut body.basic_blocks {
                let before = block.statements.len();
                block.statements.retain(|stmt| {
                    !matches!(
                        stmt.kind,
                        mir::StatementKind::StorageLive(_) | mir::StatementKind::StorageDead(_)
                    )
                });
                changes += before - block.statements.len();
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

    fn run(
        &self,
        program: &mut mir::Program,
        _engine: &mut MirQueryEngine,
    ) -> Result<usize, Error> {
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

#[derive(Debug, Clone, Copy)]
enum ReplacementPolicy {
    ConstOnly,
    AliasCopy,
}

fn rewrite_statement_operands(
    stmt: &mut mir::Statement,
    state: &[ValueState],
    policy: ReplacementPolicy,
) -> usize {
    let span = stmt.source_info;
    match &mut stmt.kind {
        mir::StatementKind::Assign(_, rvalue) => rewrite_rvalue_operands(rvalue, state, policy, span),
        mir::StatementKind::IntrinsicCall { args, .. } => {
            rewrite_operand_list(args, state, policy, span)
        }
        _ => 0,
    }
}

fn rewrite_terminator_operands(
    term: &mut mir::Terminator,
    state: &[ValueState],
    policy: ReplacementPolicy,
) -> usize {
    let span = term.source_info;
    match &mut term.kind {
        mir::TerminatorKind::SwitchInt { discr, .. } => {
            rewrite_operand(discr, state, policy, span).unwrap_or(0)
        }
        mir::TerminatorKind::Call { func, args, .. } => {
            let mut changes = rewrite_operand(func, state, policy, span).unwrap_or(0);
            changes += rewrite_operand_list(args, state, policy, span);
            changes
        }
        mir::TerminatorKind::Assert { cond, .. } => {
            rewrite_operand(cond, state, policy, span).unwrap_or(0)
        }
        mir::TerminatorKind::Yield { value, .. } => {
            rewrite_operand(value, state, policy, span).unwrap_or(0)
        }
        _ => 0,
    }
}

fn rewrite_rvalue_operands(
    rvalue: &mut mir::Rvalue,
    state: &[ValueState],
    policy: ReplacementPolicy,
    span: mir::Span,
) -> usize {
    match rvalue {
        mir::Rvalue::Use(operand) => rewrite_operand(operand, state, policy, span).unwrap_or(0),
        mir::Rvalue::BinaryOp(_, lhs, rhs)
        | mir::Rvalue::CheckedBinaryOp(_, lhs, rhs) => {
            rewrite_operand(lhs, state, policy, span).unwrap_or(0)
                + rewrite_operand(rhs, state, policy, span).unwrap_or(0)
        }
        mir::Rvalue::UnaryOp(_, operand)
        | mir::Rvalue::Repeat(operand, _)
        | mir::Rvalue::Cast(_, operand, _)
        | mir::Rvalue::ContainerLen { container: operand, .. }
        | mir::Rvalue::ContainerGet { container: operand, .. }
        | mir::Rvalue::ShallowInitBox(operand, _) => {
            rewrite_operand(operand, state, policy, span).unwrap_or(0)
        }
        mir::Rvalue::Aggregate(_, operands) => rewrite_operand_list(operands, state, policy, span),
        mir::Rvalue::ContainerLiteral { elements, .. } => {
            rewrite_operand_list(elements, state, policy, span)
        }
        mir::Rvalue::ContainerMapLiteral { entries, .. } => {
            let mut changes = 0;
            for (key, value) in entries {
                changes += rewrite_operand(key, state, policy, span).unwrap_or(0);
                changes += rewrite_operand(value, state, policy, span).unwrap_or(0);
            }
            changes
        }
        mir::Rvalue::IntrinsicCall { args, .. } => {
            rewrite_operand_list(args, state, policy, span)
        }
        _ => 0,
    }
}

fn rewrite_operand_list(
    operands: &mut [mir::Operand],
    state: &[ValueState],
    policy: ReplacementPolicy,
    span: mir::Span,
) -> usize {
    let mut changes = 0;
    for operand in operands {
        changes += rewrite_operand(operand, state, policy, span).unwrap_or(0);
    }
    changes
}

fn rewrite_operand(
    operand: &mut mir::Operand,
    state: &[ValueState],
    policy: ReplacementPolicy,
    span: mir::Span,
) -> Option<usize> {
    let replacement = match operand {
        mir::Operand::Copy(place) => rewrite_place_operand(place, state, policy, span, true),
        mir::Operand::Move(place) => rewrite_place_operand(place, state, policy, span, false),
        _ => None,
    }?;

    *operand = replacement;
    Some(1)
}

fn rewrite_place_operand(
    place: &mir::Place,
    state: &[ValueState],
    policy: ReplacementPolicy,
    span: mir::Span,
    is_copy: bool,
) -> Option<mir::Operand> {
    if !place.projection.is_empty() {
        return None;
    }
    let state = state.get(place.local as usize)?;
    match (policy, state) {
        (ReplacementPolicy::ConstOnly, ValueState::Const(constant)) => {
            Some(mir::Operand::Constant(mir::Constant {
                span,
                user_ty: None,
                literal: constant.clone(),
            }))
        }
        (ReplacementPolicy::AliasCopy, ValueState::Alias(alias))
            if is_copy && *alias != place.local =>
        {
            Some(mir::Operand::Copy(mir::Place::from_local(*alias)))
        }
        _ => None,
    }
}

fn operand_to_constant(
    operand: &mir::Operand,
    state: &[ValueState],
) -> Option<mir::ConstantKind> {
    match operand {
        mir::Operand::Constant(constant) => Some(constant.literal.clone()),
        mir::Operand::Copy(place) | mir::Operand::Move(place) if place.projection.is_empty() => {
            match state.get(place.local as usize)? {
                ValueState::Const(value) => Some(value.clone()),
                _ => None,
            }
        }
        _ => None,
    }
}

fn constant_to_u128(value: Option<&mir::ConstantKind>) -> Option<u128> {
    match value? {
        mir::ConstantKind::Bool(b) => Some(if *b { 1 } else { 0 }),
        mir::ConstantKind::UInt(v) => Some(*v as u128),
        mir::ConstantKind::Int(v) => {
            if *v < 0 {
                None
            } else {
                Some(*v as u128)
            }
        }
        _ => None,
    }
}

fn is_pure_rvalue(rvalue: &mir::Rvalue) -> bool {
    match rvalue {
        mir::Rvalue::Use(_)
        | mir::Rvalue::BinaryOp(_, _, _)
        | mir::Rvalue::CheckedBinaryOp(_, _, _)
        | mir::Rvalue::UnaryOp(_, _)
        | mir::Rvalue::Repeat(_, _)
        | mir::Rvalue::Ref(_, _, _)
        | mir::Rvalue::AddressOf(_, _)
        | mir::Rvalue::Len(_)
        | mir::Rvalue::Cast(_, _, _)
        | mir::Rvalue::Discriminant(_)
        | mir::Rvalue::Aggregate(_, _)
        | mir::Rvalue::ContainerLiteral { .. }
        | mir::Rvalue::ContainerMapLiteral { .. }
        | mir::Rvalue::ContainerLen { .. }
        | mir::Rvalue::ContainerGet { .. }
        | mir::Rvalue::ShallowInitBox(_, _) => true,
        _ => false,
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
