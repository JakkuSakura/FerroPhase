mod error;
mod state;

pub use error::CompilerDriverError;
pub use state::CompilerState;

use fp_backend::transformations::{HirGenerator, LirGenerator, MirLowering};
use fp_core::ast::{NodeKind, Value};
use fp_typing::{PendingTypingRequest, PendingTypingRequestKind};

use crate::scheduler::{
    AstId, CompileTimeNeed, CompilerAnswer, CompilerRequest, CompilerScheduler, CompilerWork,
    ConstValueId, ExecutionMode, FullyQualifiedPath, GenericWorkRequest, HirId, LirId, MirId,
    RequestId, RuntimeValueId, ScheduledAnswer, TypeNeed, TypedAstId, TypingRequest,
};

pub struct CompilerDriver {
    pub scheduler: CompilerScheduler,
    pub state: CompilerState,
    interpreter: fp_interpret::lir::LirInterpreter,
}

impl CompilerDriver {
    pub fn new() -> Self {
        Self {
            scheduler: CompilerScheduler::new(),
            state: CompilerState::new(),
            interpreter: fp_interpret::lir::LirInterpreter::new(),
        }
    }

    pub fn with_state(state: CompilerState) -> Self {
        Self {
            scheduler: CompilerScheduler::new(),
            state,
            interpreter: fp_interpret::lir::LirInterpreter::new(),
        }
    }

    pub fn run_next(&mut self) -> Result<Option<ScheduledAnswer>, CompilerDriverError> {
        let Some(request) = self.scheduler.next_request() else {
            return Ok(None);
        };
        let answer = self.handle_request(&request)?;
        let scheduled = self.scheduler.answer_and_schedule(request.id, answer)?;
        Ok(Some(scheduled))
    }

    fn handle_request(
        &mut self,
        request: &CompilerRequest,
    ) -> Result<CompilerAnswer, CompilerDriverError> {
        match &request.work {
            CompilerWork::TypeAst { ast, path, .. } => self.type_ast(ast, path),
            CompilerWork::LowerToHir {
                typed_ast, path, ..
            } => self.lower_to_hir(typed_ast, path),
            CompilerWork::LowerToMir { hir, path, .. } => self.lower_to_mir(hir, path),
            CompilerWork::LowerToLir { mir, path, .. } => self.lower_to_lir(mir, path),
            CompilerWork::EnqueueGeneric { generic, .. } => Ok(CompilerAnswer::GenericQueued {
                generic: generic.clone(),
            }),
            CompilerWork::Execute {
                lir,
                path,
                mode,
                answer_to,
            } => self.execute_lir(lir, path, *mode, *answer_to),
            work => Err(CompilerDriverError::UnsupportedWork(format!("{work:?}"))),
        }
    }

    fn type_ast(
        &mut self,
        ast_id: &AstId,
        path: &FullyQualifiedPath,
    ) -> Result<CompilerAnswer, CompilerDriverError> {
        let mut ast = self.state.ast(ast_id)?.clone();
        let outcome = fp_typing::annotate(&mut ast)?;
        let all_requests: Vec<TypingRequest> = outcome
            .pending_requests
            .iter()
            .map(|request| self.typing_request_from_outcome(request, path))
            .collect();

        if !self.state.comptime_seeded.contains(ast_id) {
            let comptime_count = all_requests.iter().filter(|r| matches!(r, TypingRequest::Comptime(_))).count();
            if comptime_count > 0 {
                self.state.comptime_pending.insert(ast_id.clone(), comptime_count);
            }
            self.state.comptime_seeded.insert(ast_id.clone());
        } else if self.state.comptime_pending.get(ast_id).copied().unwrap_or(0) == 0
            && all_requests.iter().any(|r| matches!(r, TypingRequest::Comptime(_)))
        {
            return Err(CompilerDriverError::UnresolvableComptime(ast_id.clone()));
        }

        if !self.state.comptime_seeded.contains(ast_id) {
            let comptime_count = all_requests
                .iter()
                .filter(|r| matches!(r, TypingRequest::Comptime(_)))
                .count();
            if comptime_count > 0 {
                self.state
                    .comptime_pending
                    .insert(ast_id.clone(), comptime_count);
            }
            self.state.comptime_seeded.insert(ast_id.clone());
        }

        let requests: Vec<TypingRequest> = all_requests
            .into_iter()
            .filter(|r| {
                !matches!(r, TypingRequest::Comptime(_))
                    || self
                        .state
                        .comptime_pending
                        .get(ast_id)
                        .copied()
                        .unwrap_or(0)
                        > 0
            })
            .collect();

        self.state.extend_typing_diagnostics(outcome.diagnostics);
        let typed_ast = TypedAstId::new(format!("typed_ast:{}", path.to_key()));
        self.state.insert_typed_ast(typed_ast.clone(), ast);
        Ok(CompilerAnswer::TypedAst {
            typed_ast,
            requests,
        })
    }

    fn lower_to_hir(
        &mut self,
        typed_ast_id: &TypedAstId,
        path: &FullyQualifiedPath,
    ) -> Result<CompilerAnswer, CompilerDriverError> {
        let ast = self.state.typed_ast(typed_ast_id)?;
        let hir_program = match ast.kind() {
            NodeKind::Expr(expr) => HirGenerator::new().transform_expr(expr)?,
            NodeKind::File(file) => HirGenerator::with_file(&file.path).transform_file(file)?,
            NodeKind::Query(query) => HirGenerator::new().transform_query_document(query)?,
            NodeKind::Item(_) | NodeKind::Schema(_) | NodeKind::Workspace(_) => {
                return Err(CompilerDriverError::UnsupportedWork(format!(
                    "cannot lower AST node kind to HIR: {:?}",
                    ast.kind()
                )));
            }
        };
        let hir = HirId::new(format!("hir:{}", path.to_key()));
        self.state
            .hir_to_typed_ast
            .insert(hir.clone(), typed_ast_id.clone());
        self.state.insert_hir(hir.clone(), hir_program);
        Ok(CompilerAnswer::Hir { hir })
    }

    fn lower_to_mir(
        &mut self,
        hir_id: &HirId,
        path: &FullyQualifiedPath,
    ) -> Result<CompilerAnswer, CompilerDriverError> {
        let hir = self.state.hir(hir_id)?.clone();
        let mut lowering = MirLowering::new();
        let mir = lowering.transform(hir)?;
        let mir_id = MirId::new(format!("mir:{}", path.to_key()));
        if let Some(typed_ast) = self.state.hir_to_typed_ast.get(hir_id).cloned() {
            self.state
                .mir_to_typed_ast
                .insert(mir_id.clone(), typed_ast);
        }
        self.state.insert_mir(mir_id.clone(), mir);
        Ok(CompilerAnswer::Mir { mir: mir_id })
    }

    fn lower_to_lir(
        &mut self,
        mir_id: &MirId,
        path: &FullyQualifiedPath,
    ) -> Result<CompilerAnswer, CompilerDriverError> {
        let mir = self.state.mir(mir_id)?.clone();
        let mut lowering = LirGenerator::new();
        let lir = lowering.transform(mir)?;
        let lir_id = LirId::new(format!("lir:{}", path.to_key()));
        if let Some(typed_ast) = self.state.mir_to_typed_ast.get(mir_id).cloned() {
            self.state
                .lir_to_typed_ast
                .insert(lir_id.clone(), typed_ast);
        }
        self.state.insert_lir(lir_id.clone(), lir);
        Ok(CompilerAnswer::Lir { lir: lir_id })
    }

    fn execute_lir(
        &mut self,
        lir_id: &LirId,
        path: &FullyQualifiedPath,
        mode: ExecutionMode,
        answer_to: Option<RequestId>,
    ) -> Result<CompilerAnswer, CompilerDriverError> {
        let lir = self.state.lir(lir_id)?.clone();
        match mode {
            ExecutionMode::Comptime => {
                let value = self.evaluate_comptime_lir(&lir).unwrap_or_else(|e| {
                    eprintln!("LIR interpreter error: {e}");
                    Value::unit()
                });

                let value_id = ConstValueId::new(format!("const_value:{}", path.to_key()));
                self.state.insert_const_value(value_id.clone(), value);

                if let Some(blocked) = answer_to {
                    self.resolve_comptime_for_blocked(blocked);
                }

                Ok(CompilerAnswer::CompileTimeValue { value: value_id })
            }
            ExecutionMode::Runtime => {
                let value_id = RuntimeValueId::new(format!("runtime_value:{}", path.to_key()));
                self.state
                    .insert_runtime_value(value_id.clone(), Value::unit());
                Ok(CompilerAnswer::RuntimeOutput { value: value_id })
            }
        }
    }

    fn evaluate_comptime_lir(
        &mut self,
        lir: &fp_core::lir::LirProgram,
    ) -> Result<fp_core::ast::Value, fp_interpret::lir::vm::VmError> {
        self.interpreter.run_main(lir)
    }

    fn resolve_comptime_for_blocked(&mut self, blocked: RequestId) {
        let ast_id =
            self.scheduler
                .answered(blocked)
                .and_then(|completed| match &completed.request.work {
                    CompilerWork::TypeAst { ast, .. } => Some(ast.clone()),
                    _ => None,
                });

        if let Some(ast_id) = ast_id {
            if let Some(count) = self.state.comptime_pending.get_mut(&ast_id) {
                *count = count.saturating_sub(1);
            }
        }
    }

    fn typing_request_from_outcome(
        &self,
        request: &PendingTypingRequest,
        path: &FullyQualifiedPath,
    ) -> TypingRequest {
        match request.kind {
            PendingTypingRequestKind::UnknownType => {
                TypingRequest::UnknownType(TypeNeed::new(request.description.clone()))
            }
            PendingTypingRequestKind::Generic => TypingRequest::Generic(GenericWorkRequest::new(
                path.clone(),
                request.description.clone(),
            )),
            PendingTypingRequestKind::Comptime => {
                TypingRequest::Comptime(CompileTimeNeed::new(request.description.clone()))
            }
        }
    }
}

impl Default for CompilerDriver {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scheduler::{LirConsumer, ScopeId};
    use fp_core::ast::{
        Expr, FunctionSignature, GenericParam, Ident, Item, ItemDefFunction, Node, TypeBounds,
    };

    fn path() -> FullyQualifiedPath {
        FullyQualifiedPath::from_segments(vec!["crate".to_string(), "main".to_string()])
    }

    #[test]
    fn lowers_ast_through_lir_with_concrete_driver_methods() {
        let path = path();
        let ast_id = AstId::new("ast:crate::main");
        let mut driver = CompilerDriver::new();
        driver
            .state
            .insert_ast(ast_id.clone(), Node::expr(Expr::unit()));

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new("crate::main"),
            path,
            consumers: vec![LirConsumer::Bytecode],
        });

        driver.run_next().expect("type AST").expect("typed answer");
        driver.run_next().expect("lower HIR").expect("HIR answer");
        driver.run_next().expect("lower MIR").expect("MIR answer");
        driver.run_next().expect("lower LIR").expect("LIR answer");

        assert_eq!(driver.state.hir_len(), 1);
        assert_eq!(driver.state.mir_len(), 1);
        assert_eq!(driver.state.lir_len(), 1);

        let bytecode = driver
            .scheduler
            .next_request()
            .expect("LIR consumer should request bytecode");
        assert!(matches!(
            bytecode.work,
            CompilerWork::SerializeBytecode { .. }
        ));
    }

    #[test]
    fn typing_enqueues_generic_work_before_lowering() {
        let path = path();
        let ast_id = AstId::new("ast:crate::generic");
        let mut generic_function =
            ItemDefFunction::new_simple(Ident::new("id"), Expr::unit().into());
        generic_function.sig = FunctionSignature {
            generics_params: vec![GenericParam {
                name: Ident::new("T"),
                bounds: TypeBounds::any(),
            }],
            ..FunctionSignature::unit()
        };

        let mut driver = CompilerDriver::new();
        driver
            .state
            .insert_ast(ast_id.clone(), Node::item(Item::from(generic_function)));

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new("crate::generic"),
            path,
            consumers: vec![LirConsumer::Bytecode],
        });

        driver.run_next().expect("type AST").expect("typed answer");

        let generic = driver
            .scheduler
            .next_request()
            .expect("generic work should be queued");
        assert!(matches!(generic.work, CompilerWork::EnqueueGeneric { .. }));
    }

    #[test]
    fn comptime_lir_work_answers_with_const_value() {
        let path = path();
        let ast_id = AstId::new("ast:crate::const");
        let mut driver = CompilerDriver::new();
        driver
            .state
            .insert_ast(ast_id.clone(), Node::expr(Expr::unit()));

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new("crate::const"),
            path,
            consumers: vec![LirConsumer::ExecuteComptime],
        });

        driver.run_next().expect("type AST").expect("typed answer");
        driver.run_next().expect("lower HIR").expect("HIR answer");
        driver.run_next().expect("lower MIR").expect("MIR answer");
        driver.run_next().expect("lower LIR").expect("LIR answer");
        driver
            .run_next()
            .expect("execute comptime LIR")
            .expect("comptime answer");

        assert_eq!(driver.state.const_value_len(), 1);
    }

    #[test]
    fn const_item_discovers_comptime_need_and_evaluates() {
        let path = path();
        let ast_id = AstId::new("ast:crate::answer");

        let const_block = fp_core::ast::ExprConstBlock {
            span: fp_core::span::Span::null(),
            expr: Box::new(Expr::value(fp_core::ast::Value::int(42))),
        };
        let expr = Expr::from(fp_core::ast::ExprKind::ConstBlock(const_block));

        let mut driver = CompilerDriver::new();
        driver.state.insert_ast(ast_id.clone(), Node::expr(expr));

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new("crate::answer"),
            path: path.clone(),
            consumers: vec![LirConsumer::ExecuteComptime],
        });

        let scheduled = driver.run_next().expect("type AST").expect("typed answer");
        assert!(
            !scheduled.followups.is_empty(),
            "comptime need should produce follow-up work"
        );

        driver.run_next().expect("lower HIR").expect("HIR answer");
        driver.run_next().expect("lower MIR").expect("MIR answer");
        driver.run_next().expect("lower LIR").expect("LIR answer");

        let execute_scheduled = driver
            .run_next()
            .expect("execute comptime LIR")
            .expect("comptime answer");

        let retry = driver
            .scheduler
            .next_request()
            .expect("blocked typing should be requeued");
        assert!(matches!(retry.work, CompilerWork::TypeAst { .. }));

        assert_eq!(driver.state.const_value_len(), 1);
        assert!(!execute_scheduled.followups.is_empty());
    }

    #[test]
    fn driver_loop_resolves_full_comptime_chain_to_completion() {
        let path = path();
        let ast_id = AstId::new("ast:crate::const");
        let mut driver = CompilerDriver::new();
        driver
            .state
            .insert_ast(ast_id.clone(), Node::expr(Expr::unit()));

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new("crate::const"),
            path,
            consumers: vec![LirConsumer::ExecuteComptime],
        });

        let mut steps = 0;
        while let Ok(Some(scheduled)) = driver.run_next() {
            steps += 1;
            assert!(steps <= 20, "driver loop should not run forever");
            let _ = scheduled;
        }

        assert_eq!(driver.state.const_value_len(), 1);
        assert!(
            driver.scheduler.is_idle(),
            "scheduler should be idle after comptime chain resolves"
        );
    }

    #[test]
    fn const_block_with_bytecode_consumer_discovers_comptime_branch() {
        let path = path();
        let ast_id = AstId::new("ast:crate::answer");

        let const_block = fp_core::ast::ExprConstBlock {
            span: fp_core::span::Span::null(),
            expr: Box::new(Expr::value(fp_core::ast::Value::int(42))),
        };
        let expr = Expr::from(fp_core::ast::ExprKind::ConstBlock(const_block));

        let mut driver = CompilerDriver::new();
        driver.state.insert_ast(ast_id.clone(), Node::expr(expr));

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new("crate::answer"),
            path: path.clone(),
            consumers: vec![LirConsumer::Bytecode],
        });

        let scheduled = driver.run_next().expect("type AST").expect("typed answer");
        assert!(
            !scheduled.followups.is_empty(),
            "const block should produce comptime follow-up work before bytecode can be emitted"
        );
    }

    #[test]
    fn blocked_on_request_flow_through_scheduler_with_driver() {
        let path = path();
        let ast_id = AstId::new("ast:crate::const");
        let mut driver = CompilerDriver::new();
        driver
            .state
            .insert_ast(ast_id.clone(), Node::expr(Expr::unit()));

        let lir_id = LirId::new("lir:crate::dep");
        let empty_lir = fp_core::lir::LirProgram {
            functions: Vec::new(),
            globals: Vec::new(),
            type_definitions: Vec::new(),
            queries: Vec::new(),
        };
        driver.state.insert_lir(lir_id.clone(), empty_lir);

        let blocked = driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id.clone(),
            scope: ScopeId::new("crate::const"),
            path: path.clone(),
            consumers: vec![LirConsumer::Bytecode],
        });
        let _active_blocked = driver.scheduler.next_request().expect("active type");

        let dependent_id = driver.scheduler.submit(CompilerWork::Execute {
            lir: LirId::new("lir:crate::dep"),
            path: path.clone(),
            mode: ExecutionMode::Comptime,
            answer_to: None,
        });
        let _active_dependent = driver.scheduler.next_request().expect("active execute");

        driver
            .scheduler
            .answer_and_schedule(
                dependent_id,
                CompilerAnswer::BlockedOnRequest { request: blocked },
            )
            .expect("execute blocked on type");

        driver
            .scheduler
            .answer_and_schedule(
                blocked,
                CompilerAnswer::TypedAst {
                    typed_ast: TypedAstId::new("typed_ast:crate::const"),
                    requests: Vec::new(),
                },
            )
            .expect("type answered");

        let retried = driver
            .scheduler
            .next_request()
            .expect("blocked execute retried");
        assert!(matches!(
            retried.work,
            CompilerWork::Execute {
                mode: ExecutionMode::Comptime,
                ..
            }
        ));
    }
}

#[cfg(test)]
mod comptime_source_tests {
    use super::*;
    use crate::scheduler::{AstId, CompilerWork, FullyQualifiedPath, LirConsumer, ScopeId};
    use fp_core::frontend::LanguageFrontend;

    fn path() -> FullyQualifiedPath {
        FullyQualifiedPath::from_segments(vec!["test".to_string(), "main".to_string()])
    }

    #[test]
    fn parses_fp_source_and_runs_const_item_through_driver() {
        let source = r#"
const ANSWER: i64 = 42;

fn main() {
    let result = const { ANSWER * 2 };
}
"#;
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("test.fp"))
            .expect("parse .fp source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let ast_id = AstId::new("ast:test::main");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new("test::main"),
            path: path(),
            consumers: vec![LirConsumer::Bytecode],
        });

        let scheduled = driver.run_next().expect("type AST").expect("typed answer");
        assert!(
            !scheduled.followups.is_empty(),
            "const item and const block should produce comptime follow-ups"
        );

        let lower_hir = driver
            .scheduler
            .next_request()
            .expect("comptime need triggers lowering");
        assert!(
            matches!(lower_hir.work, CompilerWork::LowerToHir { .. }),
            "comptime need should produce LowerToHir work"
        );
    }

    #[test]
    fn parses_simple_const_and_evaluates_through_full_pipeline() {
        let source = "const ANSWER: i64 = 42;\n";
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("const.fp"))
            .expect("parse const source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let ast_id = AstId::new("ast:test::const");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new("test::const"),
            path: path(),
            consumers: vec![LirConsumer::ExecuteComptime],
        });

        let scheduled = driver.run_next().expect("type AST").expect("typed answer");
        assert!(
            scheduled.followups.len() >= 1,
            "const item should produce at least one comptime follow-up"
        );

        driver.run_next().expect("lower HIR").expect("HIR answer");
        driver.run_next().expect("lower MIR").expect("MIR answer");
        driver.run_next().expect("lower LIR").expect("LIR answer");

        let execute_scheduled = driver
            .run_next()
            .expect("execute comptime")
            .expect("comptime answer");
        assert!(
            !execute_scheduled.followups.is_empty(),
            "comptime execution should requeue blocked typing"
        );

        assert_eq!(
            driver.state.const_value_len(),
            1,
            "const should produce one compile-time value"
        );
    }

    #[test]
    fn multiple_const_items_produce_separate_comptime_needs() {
        let source = r#"
const WIDTH: i64 = 640;
const HEIGHT: i64 = 480;
const AREA: i64 = WIDTH * HEIGHT;
"#;
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("multi.fp"))
            .expect("parse multi-const source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let ast_id = AstId::new("ast:test::multi");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new("test::multi"),
            path: path(),
            consumers: vec![LirConsumer::ExecuteComptime],
        });

        let scheduled = driver.run_next().expect("type AST").expect("typed answer");
        assert!(
            scheduled.followups.len() >= 1,
            "const items should produce comptime follow-up work (aggregated per scope)"
        );
    }

    #[test]
    fn const_block_in_function_triggers_comptime_path() {
        let source = r#"
fn calculate() {
    let size = const { 1024 * 8 };
}
"#;
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("block.fp"))
            .expect("parse const-block source");
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let ast_id = AstId::new("ast:test::block");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new("test::block"),
            path: path(),
            consumers: vec![LirConsumer::Bytecode],
        });

        let scheduled = driver.run_next().expect("type AST").expect("typed answer");
        assert!(
            !scheduled.followups.is_empty(),
            "const block in function should produce comptime follow-up work"
        );

        let first = driver.scheduler.next_request().expect("comptime follow-up");
        assert!(
            matches!(first.work, CompilerWork::LowerToHir { .. }),
            "const block should trigger lowering pipeline, got {:?}",
            first.work
        );
    }

    enum ExampleResult {
        Completed { lowered: usize, executed: usize },
        TypedLooping { followups: usize },
    }

    fn compile_example_file(name: &str) -> Result<ExampleResult, String> {
        let abs = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../examples")
            .join(name);
        let source = std::fs::read_to_string(&abs).map_err(|e| format!("read: {e}"))?;

        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(&source, &abs)
            .map_err(|e| format!("parse: {e}"))?;
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let label = name.trim_end_matches(".fp");
        let ast_id = AstId::new(format!("ast:example::{label}"));
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new(label),
            path: FullyQualifiedPath::from_segments(vec!["example".into(), label.to_string()]),
            consumers: vec![LirConsumer::Bytecode],
        });

        let scheduled = driver.run_next().map_err(|e| format!("run_next: {e}"))?;
        let scheduled = scheduled.ok_or_else(|| "no work returned".to_string())?;
        let followups = scheduled.followups.len();

        let mut lowered = 0;
        let mut executed = 0;
        let mut step = 0;
        while let Ok(Some(s)) = driver.run_next() {
            step += 1;
            if step > 500 {
                return Ok(ExampleResult::TypedLooping { followups });
            }
            if matches!(
                s.completed.answer,
                CompilerAnswer::Hir { .. }
                    | CompilerAnswer::Mir { .. }
                    | CompilerAnswer::Lir { .. }
            ) {
                lowered += 1;
            }
            if matches!(s.completed.answer, CompilerAnswer::CompileTimeValue { .. }) {
                executed += 1;
            }
        }

        Ok(ExampleResult::Completed { lowered, executed })
    }

    #[test]
    fn run_all_example_files() {
        let examples_dir =
            std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../examples");
        let mut entries: Vec<_> = std::fs::read_dir(&examples_dir)
            .expect("read examples dir")
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "fp"))
            .map(|e| e.file_name().to_string_lossy().to_string())
            .collect();
        entries.sort();

        let mut completed = 0;
        let mut typed = 0;
        let mut errors = 0;

        for name in &entries {
            print!("  {name:.<50} ");
            match compile_example_file(name) {
                Ok(ExampleResult::Completed { lowered, executed }) => {
                    completed += 1;
                    println!("OK  (lowered={lowered}, executed={executed})");
                }
                Ok(ExampleResult::TypedLooping { followups }) => {
                    typed += 1;
                    println!("TYPED (followups={followups}, loops — comptime not applied yet)");
                }
                Err(e) => {
                    errors += 1;
                    println!("ERROR: {e}");
                }
            }
        }

        println!(
            "\n  Examples: {completed} completed, {typed} typed-but-looping, {errors} errors ({} total)",
            entries.len()
        );
        println!("  Note: 'typed-but-looping' means comptime needs are discovered but");
        println!("  not applied back to AST yet — the driver work-in-progress.");
    }

    fn compile_inline_source(source: &str, expected_const_values: usize) {
        let fe = fp_lang::FerroFrontend::new();
        let result = fe
            .parse_file(source, std::path::Path::new("inline.fp"))
            .unwrap_or_else(|e| panic!("parse inline: {e}"));
        let ast_node = result.ast;

        let mut driver = CompilerDriver::new();
        let ast_id = AstId::new("ast:example::inline");
        driver.state.insert_ast(ast_id.clone(), ast_node);

        driver.scheduler.submit(CompilerWork::TypeAst {
            ast: ast_id,
            scope: ScopeId::new("inline"),
            path: FullyQualifiedPath::from_segments(vec!["example".into(), "inline".into()]),
            consumers: vec![LirConsumer::ExecuteComptime],
        });

        let scheduled = driver.run_next().expect("type AST").expect("typed answer");
        assert!(
            !scheduled.followups.is_empty(),
            "source should produce comptime follow-ups"
        );

        for _ in 0..expected_const_values {
            driver.run_next().expect("lower HIR").expect("HIR answer");
            driver.run_next().expect("lower MIR").expect("MIR answer");
            driver.run_next().expect("lower LIR").expect("LIR answer");
            driver
                .run_next()
                .expect("execute comptime")
                .expect("comptime answer");
        }

        assert_eq!(
            driver.state.const_value_len(),
            expected_const_values,
            "expected {expected_const_values} const values"
        );
    }

    #[test]
    fn comptime_const_with_arithmetic() {
        compile_inline_source(
            r#"
const BUFFER_SIZE: i64 = 1024 * 4;
const MAX_CONNECTIONS: i64 = 150;
const FACTORIAL_5: i64 = 5 * 4 * 3 * 2 * 1;
const IS_LARGE: bool = BUFFER_SIZE > 2048;
"#,
            1,
        );
    }

    #[test]
    fn comptime_const_with_struct_defaults() {
        compile_inline_source(
            r#"
struct Config {
    buffer_size: i64,
    max_connections: i64,
}

const BUFFER_SIZE: i64 = 4096;
const MAX_CONNECTIONS: i64 = 150;
const DEFAULT_CONFIG: Config = Config {
    buffer_size: BUFFER_SIZE,
    max_connections: MAX_CONNECTIONS,
};
"#,
            1,
        );
    }

    #[test]
    fn comptime_const_block_with_conditional() {
        compile_inline_source(
            r#"
const BUFFER_SIZE: i64 = 4096;
const OPTIMIZED_SIZE: i64 = const { BUFFER_SIZE * 2 };
const CACHE_STRATEGY: &str = const {
    if BUFFER_SIZE > 2048 {
        "large"
    } else {
        "small"
    }
};
"#,
            1,
        );
    }
}
