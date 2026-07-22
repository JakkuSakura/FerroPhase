use std::collections::BTreeMap;

use super::error::SchedulerError;
use super::identity::RequestId;
use super::request::{CompilerRequest, CompletedRequest, ScheduledAnswer};
use super::work::{
    CompilerAnswer, CompilerWork, ExecutionMode, LirConsumer, OutputDestination, OutputObjectId,
    TypingRequest,
};

#[derive(Debug, Default)]
pub struct CompilerScheduler {
    next_id: u64,
    stack: Vec<CompilerRequest>,
    active: BTreeMap<RequestId, CompilerRequest>,
    answered: BTreeMap<RequestId, CompletedRequest>,
    blocked: BTreeMap<RequestId, Vec<CompilerRequest>>,
}

impl CompilerScheduler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn submit(&mut self, work: CompilerWork) -> RequestId {
        let id = self.allocate_id();
        self.stack.push(CompilerRequest { id, work });
        id
    }

    pub fn next_request(&mut self) -> Option<CompilerRequest> {
        let request = self.stack.pop()?;
        self.active.insert(request.id, request.clone());
        Some(request)
    }

    pub fn answer(
        &mut self,
        id: RequestId,
        answer: CompilerAnswer,
    ) -> Result<&CompletedRequest, SchedulerError> {
        let completed = self.complete(id, answer)?;
        self.answered.insert(id, completed);
        Ok(self
            .answered
            .get(&id)
            .expect("completed request was inserted before lookup"))
    }

    pub fn answer_and_schedule(
        &mut self,
        id: RequestId,
        answer: CompilerAnswer,
    ) -> Result<ScheduledAnswer, SchedulerError> {
        let completed = self.complete(id, answer)?;
        let followup_work = self.followup_work(&completed);
        self.record_blocked_request(&completed);
        self.answered.insert(id, completed.clone());

        let mut followups = self.submit_followups(followup_work);
        followups.extend(self.retry_requests_blocked_on(id));

        Ok(ScheduledAnswer {
            completed,
            followups,
        })
    }

    pub fn pending_len(&self) -> usize {
        self.stack.len()
    }

    pub fn active_len(&self) -> usize {
        self.active.len()
    }

    pub fn answered_len(&self) -> usize {
        self.answered.len()
    }

    pub fn is_idle(&self) -> bool {
        self.stack.is_empty() && self.active.is_empty()
    }

    pub fn answered(&self, id: RequestId) -> Option<&CompletedRequest> {
        self.answered.get(&id)
    }

    fn allocate_id(&mut self) -> RequestId {
        let id = RequestId::new(self.next_id);
        self.next_id += 1;
        id
    }

    fn complete(
        &mut self,
        id: RequestId,
        answer: CompilerAnswer,
    ) -> Result<CompletedRequest, SchedulerError> {
        if self.answered.contains_key(&id) {
            return Err(SchedulerError::RequestAlreadyAnswered(id));
        }

        let request = self
            .active
            .remove(&id)
            .ok_or(SchedulerError::RequestNotActive(id))?;
        Ok(CompletedRequest { request, answer })
    }

    fn submit_followups(&mut self, followup_work: Vec<CompilerWork>) -> Vec<RequestId> {
        let mut followups = Vec::with_capacity(followup_work.len());
        for work in followup_work.into_iter().rev() {
            followups.push(self.submit(work));
        }
        followups.reverse();
        followups
    }

    fn record_blocked_request(&mut self, completed: &CompletedRequest) {
        let CompilerAnswer::BlockedOnRequest { request } = completed.answer else {
            return;
        };
        self.blocked
            .entry(request)
            .or_default()
            .push(completed.request.clone());
    }

    fn retry_requests_blocked_on(&mut self, answered: RequestId) -> Vec<RequestId> {
        let Some(blocked_requests) = self.blocked.remove(&answered) else {
            return Vec::new();
        };

        let followup_work = blocked_requests
            .into_iter()
            .map(|request| request.work)
            .collect();
        self.submit_followups(followup_work)
    }

    fn followup_work(&self, completed: &CompletedRequest) -> Vec<CompilerWork> {
        match (&completed.request.work, &completed.answer) {
            (CompilerWork::ParseSource { .. }, CompilerAnswer::RawAst { raw_ast }) => {
                vec![CompilerWork::NormalizeAst {
                    raw_ast: raw_ast.clone(),
                }]
            }
            (
                CompilerWork::NormalizeAst { .. },
                CompilerAnswer::Ast {
                    ast,
                    scope,
                    path,
                    consumers,
                },
            ) => vec![CompilerWork::TypeAst {
                ast: ast.clone(),
                scope: scope.clone(),
                path: path.clone(),
                consumers: consumers.clone(),
            }],
            (
                CompilerWork::TypeAst {
                    scope,
                    path,
                    consumers,
                    ..
                },
                CompilerAnswer::TypedAst {
                    typed_ast,
                    requests,
                },
            ) => {
                if requests.is_empty() {
                    return vec![CompilerWork::LowerToHir {
                        typed_ast: typed_ast.clone(),
                        scope: scope.clone(),
                        path: path.clone(),
                        consumers: consumers.clone(),
                    }];
                }

                requests
                    .iter()
                    .map(|request| {
                        self.typing_request_work(
                            completed.request.id,
                            typed_ast,
                            scope,
                            path,
                            consumers,
                            request,
                        )
                    })
                    .collect()
            }
            (
                CompilerWork::LowerToHir {
                    path, consumers, ..
                },
                CompilerAnswer::Hir { hir },
            ) => vec![CompilerWork::LowerToMir {
                hir: hir.clone(),
                path: path.clone(),
                consumers: consumers.clone(),
            }],
            (
                CompilerWork::LowerToMir {
                    path, consumers, ..
                },
                CompilerAnswer::Mir { mir },
            ) => {
                vec![CompilerWork::LowerToLir {
                    mir: mir.clone(),
                    path: path.clone(),
                    consumers: consumers.clone(),
                }]
            }
            (
                CompilerWork::LowerToLir {
                    path, consumers, ..
                },
                CompilerAnswer::Lir { lir },
            ) => consumers
                .iter()
                .map(|consumer| self.lir_consumer_work(path, lir, *consumer))
                .collect(),
            (CompilerWork::SerializeBytecode { .. }, CompilerAnswer::Bytecode { bytecode }) => {
                vec![CompilerWork::SaveOutput {
                    output: OutputObjectId::Bytecode(bytecode.clone()),
                    destination: OutputDestination::new("bytecode"),
                }]
            }
            (
                CompilerWork::EmitNative { path, jit, .. },
                CompilerAnswer::NativeArtifact { native_object },
            ) => {
                let mut work = vec![CompilerWork::SaveOutput {
                    output: OutputObjectId::Native(native_object.clone()),
                    destination: OutputDestination::new("native"),
                }];
                if *jit {
                    work.push(CompilerWork::JitNative {
                        path: path.clone(),
                        native_object: native_object.clone(),
                    });
                }
                work
            }
            (
                CompilerWork::Execute {
                    answer_to: Some(blocked),
                    ..
                },
                CompilerAnswer::CompileTimeValue { .. },
            ) => self.requeue_blocked_typing_work(*blocked),
            _ => Vec::new(),
        }
    }

    fn typing_request_work(
        &self,
        blocked: RequestId,
        typed_ast: &super::identity::TypedAstId,
        scope: &super::identity::ScopeId,
        path: &super::identity::FullyQualifiedPath,
        consumers: &[LirConsumer],
        request: &TypingRequest,
    ) -> CompilerWork {
        match request {
            TypingRequest::UnknownType(_) | TypingRequest::Comptime(_) => {
                CompilerWork::LowerToHir {
                    typed_ast: typed_ast.clone(),
                    scope: scope.clone(),
                    path: path.clone(),
                    consumers: vec![LirConsumer::AnswerComptime(blocked)],
                }
            }
            TypingRequest::Generic(generic) => CompilerWork::EnqueueGeneric {
                typed_ast: typed_ast.clone(),
                path: path.clone(),
                generic: generic.clone(),
                consumers: consumers.to_vec(),
            },
        }
    }

    fn lir_consumer_work(
        &self,
        path: &super::identity::FullyQualifiedPath,
        lir: &super::identity::LirId,
        consumer: LirConsumer,
    ) -> CompilerWork {
        match consumer {
            LirConsumer::ExecuteComptime => CompilerWork::Execute {
                lir: lir.clone(),
                path: path.clone(),
                mode: ExecutionMode::Comptime,
                answer_to: None,
            },
            LirConsumer::AnswerComptime(blocked) => CompilerWork::Execute {
                lir: lir.clone(),
                path: path.clone(),
                mode: ExecutionMode::Comptime,
                answer_to: Some(blocked),
            },
            LirConsumer::ExecuteRuntime => CompilerWork::Execute {
                lir: lir.clone(),
                path: path.clone(),
                mode: ExecutionMode::Runtime,
                answer_to: None,
            },
            LirConsumer::Bytecode => CompilerWork::SerializeBytecode {
                lir: lir.clone(),
                path: path.clone(),
            },
            LirConsumer::Native => CompilerWork::EmitNative {
                lir: lir.clone(),
                path: path.clone(),
                jit: false,
            },
            LirConsumer::NativeJit => CompilerWork::EmitNative {
                lir: lir.clone(),
                path: path.clone(),
                jit: true,
            },
        }
    }

    fn requeue_blocked_typing_work(&self, blocked: RequestId) -> Vec<CompilerWork> {
        let Some(completed) = self.answered.get(&blocked) else {
            return Vec::new();
        };

        match &completed.request.work {
            CompilerWork::TypeAst {
                ast,
                scope,
                path,
                consumers,
            } => vec![CompilerWork::TypeAst {
                ast: ast.clone(),
                scope: scope.clone(),
                path: path.clone(),
                consumers: consumers.clone(),
            }],
            _ => Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scheduler::{
        AstId, ExecutionMode, FullyQualifiedPath, HirId, LirConsumer, LirId, MirId, RawAstId,
        SchedulerError, ScopeId, SourceId, TypeNeed, TypedAstId, TypingRequest,
    };

    fn path(segments: &[&str]) -> FullyQualifiedPath {
        FullyQualifiedPath::from_segments(segments.iter().map(|seg| (*seg).to_string()).collect())
    }

    #[test]
    fn independent_submit_uses_lifo_stack_order() {
        let mut scheduler = CompilerScheduler::new();
        let first = scheduler.submit(CompilerWork::TypeAst {
            ast: AstId::new("ast:crate::main"),
            scope: ScopeId::new("crate::main"),
            path: path(&["crate", "main"]),
            consumers: Vec::new(),
        });
        let second = scheduler.submit(CompilerWork::LowerToHir {
            typed_ast: TypedAstId::new("typed_ast:crate::main"),
            scope: ScopeId::new("crate::main"),
            path: path(&["crate", "main"]),
            consumers: Vec::new(),
        });

        assert_eq!(first.as_u64(), 0);
        assert_eq!(second.as_u64(), 1);
        assert_eq!(scheduler.pending_len(), 2);

        let next = scheduler.next_request().expect("last-submitted request");
        assert_eq!(next.id, second);
        assert!(matches!(next.work, CompilerWork::LowerToHir { .. }));
        assert_eq!(scheduler.pending_len(), 1);
        assert_eq!(scheduler.active_len(), 1);

        let next = scheduler.next_request().expect("first-submitted request");
        assert_eq!(next.id, first);
        assert!(matches!(next.work, CompilerWork::TypeAst { .. }));
    }

    #[test]
    fn answers_only_active_requests() {
        let mut scheduler = CompilerScheduler::new();
        let request = scheduler.submit(CompilerWork::LowerToLir {
            mir: MirId::new("mir:crate::main"),
            path: path(&["crate", "main"]),
            consumers: Vec::new(),
        });

        let not_active = scheduler
            .answer(
                request,
                CompilerAnswer::Lir {
                    lir: LirId::new("lir:crate::main"),
                },
            )
            .expect_err("pending request is not active");
        assert_eq!(not_active, SchedulerError::RequestNotActive(request));

        let active = scheduler.next_request().expect("active request");
        let completed = scheduler
            .answer(
                active.id,
                CompilerAnswer::Lir {
                    lir: LirId::new("lir:crate::main"),
                },
            )
            .expect("answered request");
        assert_eq!(completed.request.id, request);
        assert_eq!(scheduler.active_len(), 0);
        assert_eq!(scheduler.answered_len(), 1);
        assert!(scheduler.is_idle());
    }

    #[test]
    fn rejects_duplicate_answers() {
        let mut scheduler = CompilerScheduler::new();
        let request = scheduler.submit(CompilerWork::Execute {
            lir: LirId::new("lir:crate::build#{const true}"),
            path: path(&["crate", "build#{const true}"]),
            mode: ExecutionMode::Comptime,
            answer_to: None,
        });
        scheduler.next_request().expect("active request");
        scheduler
            .answer(
                request,
                CompilerAnswer::CompileTimeValue {
                    value: crate::scheduler::ConstValueId::new("value:crate::build#{const true}"),
                },
            )
            .expect("first answer");

        let duplicate = scheduler
            .answer(
                request,
                CompilerAnswer::CompileTimeValue {
                    value: crate::scheduler::ConstValueId::new("value:crate::build#{const true}"),
                },
            )
            .expect_err("duplicate answer");
        assert_eq!(duplicate, SchedulerError::RequestAlreadyAnswered(request));
    }

    #[test]
    fn wires_parse_normalize_and_lowering_steps() {
        let mut scheduler = CompilerScheduler::new();
        let source = scheduler.submit(CompilerWork::ParseSource {
            source: SourceId::new("src/main.fp"),
        });

        let request = scheduler.next_request().expect("parse request");
        assert_eq!(request.id, source);
        let scheduled = scheduler
            .answer_and_schedule(
                request.id,
                CompilerAnswer::RawAst {
                    raw_ast: RawAstId::new("raw_ast:src/main.fp"),
                },
            )
            .expect("raw AST schedules normalization");
        assert_eq!(scheduled.followups.len(), 1);

        let normalize = scheduler.next_request().expect("normalize request");
        assert!(matches!(normalize.work, CompilerWork::NormalizeAst { .. }));
        let scheduled = scheduler
            .answer_and_schedule(
                normalize.id,
                CompilerAnswer::Ast {
                    ast: AstId::new("ast:src/main.fp"),
                    scope: ScopeId::new("crate::main"),
                    path: path(&["crate", "main"]),
                    consumers: vec![
                        LirConsumer::ExecuteRuntime,
                        LirConsumer::Bytecode,
                        LirConsumer::Native,
                    ],
                },
            )
            .expect("AST schedules typing");
        assert_eq!(scheduled.followups.len(), 1);

        let type_ast = scheduler.next_request().expect("type request");
        assert!(matches!(type_ast.work, CompilerWork::TypeAst { .. }));
        scheduler
            .answer_and_schedule(
                type_ast.id,
                CompilerAnswer::TypedAst {
                    typed_ast: TypedAstId::new("typed_ast:crate::main"),
                    requests: Vec::new(),
                },
            )
            .expect("typed AST schedules HIR");

        let hir = scheduler.next_request().expect("HIR request");
        assert!(matches!(hir.work, CompilerWork::LowerToHir { .. }));
        scheduler
            .answer_and_schedule(
                hir.id,
                CompilerAnswer::Hir {
                    hir: HirId::new("hir:crate::main"),
                },
            )
            .expect("HIR schedules MIR");

        let mir = scheduler.next_request().expect("MIR request");
        assert!(matches!(mir.work, CompilerWork::LowerToMir { .. }));
        scheduler
            .answer_and_schedule(
                mir.id,
                CompilerAnswer::Mir {
                    mir: MirId::new("mir:crate::main"),
                },
            )
            .expect("MIR schedules LIR");

        let lir = scheduler.next_request().expect("LIR request");
        assert!(matches!(lir.work, CompilerWork::LowerToLir { .. }));
        let scheduled = scheduler
            .answer_and_schedule(
                lir.id,
                CompilerAnswer::Lir {
                    lir: LirId::new("lir:crate::main"),
                },
            )
            .expect("LIR schedules requested consumers");
        assert_eq!(scheduled.followups.len(), 3);

        let execute = scheduler.next_request().expect("execute request");
        assert!(matches!(
            execute.work,
            CompilerWork::Execute {
                mode: ExecutionMode::Runtime,
                ..
            }
        ));

        let bytecode = scheduler.next_request().expect("bytecode request");
        assert!(matches!(
            bytecode.work,
            CompilerWork::SerializeBytecode { .. }
        ));

        let native = scheduler.next_request().expect("native request");
        assert!(matches!(native.work, CompilerWork::EmitNative { .. }));
    }

    #[test]
    fn wires_lir_to_comptime_execution() {
        let mut scheduler = CompilerScheduler::new();
        let request = scheduler.submit(CompilerWork::LowerToLir {
            mir: MirId::new("mir:crate::build"),
            path: path(&["crate", "build"]),
            consumers: vec![LirConsumer::ExecuteComptime],
        });
        scheduler.next_request().expect("active request");

        let scheduled = scheduler
            .answer_and_schedule(
                request,
                CompilerAnswer::Lir {
                    lir: LirId::new("lir:crate::build"),
                },
            )
            .expect("LIR schedules comptime execution");
        assert_eq!(scheduled.followups.len(), 1);

        let execute = scheduler.next_request().expect("execute request");
        assert!(matches!(
            execute.work,
            CompilerWork::Execute {
                mode: ExecutionMode::Comptime,
                ..
            }
        ));
    }

    #[test]
    fn unknown_type_requests_run_lowering_then_retype_after_comptime_answer() {
        let mut scheduler = CompilerScheduler::new();
        let request = scheduler.submit(CompilerWork::TypeAst {
            ast: AstId::new("ast:crate::main"),
            scope: ScopeId::new("crate::main"),
            path: path(&["crate", "main"]),
            consumers: vec![LirConsumer::Bytecode],
        });
        scheduler.next_request().expect("active type request");

        let scheduled = scheduler
            .answer_and_schedule(
                request,
                CompilerAnswer::TypedAst {
                    typed_ast: TypedAstId::new("typed_ast:crate::main"),
                    requests: vec![TypingRequest::UnknownType(TypeNeed::new("unknown type T"))],
                },
            )
            .expect("typed AST schedules comptime-backed type need");
        assert_eq!(scheduled.followups.len(), 1);

        let hir = scheduler
            .next_request()
            .expect("type need starts HIR lowering");
        assert!(matches!(
            hir.work,
            CompilerWork::LowerToHir {
                consumers,
                ..
            } if consumers == vec![LirConsumer::AnswerComptime(request)]
        ));
        scheduler
            .answer_and_schedule(
                hir.id,
                CompilerAnswer::Hir {
                    hir: HirId::new("hir:crate::main"),
                },
            )
            .expect("HIR schedules MIR");

        let mir = scheduler.next_request().expect("MIR request");
        scheduler
            .answer_and_schedule(
                mir.id,
                CompilerAnswer::Mir {
                    mir: MirId::new("mir:crate::main"),
                },
            )
            .expect("MIR schedules LIR");

        let lir = scheduler.next_request().expect("LIR request");
        scheduler
            .answer_and_schedule(
                lir.id,
                CompilerAnswer::Lir {
                    lir: LirId::new("lir:crate::main"),
                },
            )
            .expect("LIR schedules interpreter answer");

        let execute = scheduler.next_request().expect("execute request");
        assert!(matches!(
            execute.work,
            CompilerWork::Execute {
                mode: ExecutionMode::Comptime,
                answer_to: Some(blocked),
                ..
            } if blocked == request
        ));

        let scheduled = scheduler
            .answer_and_schedule(
                execute.id,
                CompilerAnswer::CompileTimeValue {
                    value: crate::scheduler::ConstValueId::new("const_value:crate::main"),
                },
            )
            .expect("comptime answer requeues blocked typing");
        assert_eq!(scheduled.followups.len(), 1);

        let retry = scheduler.next_request().expect("typing retry");
        assert!(matches!(retry.work, CompilerWork::TypeAst { .. }));
    }

    #[test]
    fn execute_can_answer_blocked_on_request() {
        let mut scheduler = CompilerScheduler::new();
        let blocked = scheduler.submit(CompilerWork::TypeAst {
            ast: AstId::new("ast:crate::main"),
            scope: ScopeId::new("crate::main"),
            path: path(&["crate", "main"]),
            consumers: vec![LirConsumer::Bytecode],
        });
        let _active_blocked = scheduler.next_request().expect("active type request");

        let dependent_id = scheduler.submit(CompilerWork::Execute {
            lir: LirId::new("lir:crate::dep"),
            path: path(&["crate", "dep"]),
            mode: ExecutionMode::Comptime,
            answer_to: None,
        });
        let _active_dependent = scheduler.next_request().expect("active execute request");

        let scheduled = scheduler
            .answer_and_schedule(
                dependent_id,
                CompilerAnswer::BlockedOnRequest { request: blocked },
            )
            .expect("execute returns BlockedOnRequest");
        assert!(scheduled.followups.is_empty());
        assert_eq!(scheduler.blocked.get(&blocked).map(|v| v.len()), Some(1));
    }

    #[test]
    fn dependency_answer_retries_blocked_execute_request() {
        let mut scheduler = CompilerScheduler::new();

        let blocked = scheduler.submit(CompilerWork::TypeAst {
            ast: AstId::new("ast:crate::main"),
            scope: ScopeId::new("crate::main"),
            path: path(&["crate", "main"]),
            consumers: vec![LirConsumer::Bytecode],
        });
        let _active_blocked = scheduler.next_request().expect("active type request");

        let dependent_id = scheduler.submit(CompilerWork::Execute {
            lir: LirId::new("lir:crate::dep"),
            path: path(&["crate", "dep"]),
            mode: ExecutionMode::Comptime,
            answer_to: None,
        });
        let _active_dependent = scheduler.next_request().expect("active execute request");

        scheduler
            .answer_and_schedule(
                dependent_id,
                CompilerAnswer::BlockedOnRequest { request: blocked },
            )
            .expect("execute blocked on type request");

        let scheduled = scheduler
            .answer_and_schedule(
                blocked,
                CompilerAnswer::TypedAst {
                    typed_ast: TypedAstId::new("typed_ast:crate::main"),
                    requests: Vec::new(),
                },
            )
            .expect("type request answered");

        assert_eq!(scheduled.followups.len(), 2);
        let retried = scheduler.next_request().expect("retried execute");
        assert!(matches!(
            retried.work,
            CompilerWork::Execute {
                mode: ExecutionMode::Comptime,
                answer_to: None,
                ..
            }
        ));
    }
}
