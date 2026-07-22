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
    RuntimeValueId, ScheduledAnswer, TypeNeed, TypedAstId, TypingRequest,
};

pub struct CompilerDriver {
    pub scheduler: CompilerScheduler,
    pub state: CompilerState,
}

impl CompilerDriver {
    pub fn new() -> Self {
        Self {
            scheduler: CompilerScheduler::new(),
            state: CompilerState::new(),
        }
    }

    pub fn with_state(state: CompilerState) -> Self {
        Self {
            scheduler: CompilerScheduler::new(),
            state,
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
                lir, path, mode, ..
            } => self.execute_lir(lir, path, *mode),
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
        let requests = outcome
            .pending_requests
            .iter()
            .map(|request| self.typing_request_from_outcome(request, path))
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
        self.state.insert_lir(lir_id.clone(), lir);
        Ok(CompilerAnswer::Lir { lir: lir_id })
    }

    fn execute_lir(
        &mut self,
        lir_id: &LirId,
        path: &FullyQualifiedPath,
        mode: ExecutionMode,
    ) -> Result<CompilerAnswer, CompilerDriverError> {
        let _ = self.state.lir(lir_id)?;
        match mode {
            ExecutionMode::Comptime => {
                let value_id = ConstValueId::new(format!("const_value:{}", path.to_key()));
                self.state
                    .insert_const_value(value_id.clone(), Value::unit());
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
}
