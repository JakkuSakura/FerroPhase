use std::collections::BTreeMap;

use super::error::SchedulerError;
use super::identity::RequestId;
use super::request::{CompilerRequest, CompletedRequest, ScheduledAnswer};
use super::work::{CompilerAnswer, CompilerWork};

#[derive(Debug, Default)]
pub struct CompilerScheduler {
    next_id: u64,
    stack: Vec<CompilerRequest>,
    active: BTreeMap<RequestId, CompilerRequest>,
    answered: BTreeMap<RequestId, CompletedRequest>,
    blocked: BTreeMap<RequestId, Vec<CompilerRequest>>,
    current_processing: Option<RequestId>,
    dependencies: BTreeMap<RequestId, Vec<RequestId>>,
}

impl CompilerScheduler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn begin_processing(&mut self, id: RequestId) {
        self.current_processing = Some(id);
    }

    pub fn end_processing(&mut self) {
        self.current_processing = None;
    }

    pub fn submit(&mut self, work: CompilerWork) -> RequestId {
        let id = self.allocate_id();
        if let Some(current) = self.current_processing {
            self.dependencies
                .entry(current)
                .or_default()
                .push(id);
        }
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
        let pending_deps = self.pending_dependencies(id);

        if pending_deps.is_empty() {
            self.answered.insert(id, completed.clone());
            let mut followups = self.retry_requests_blocked_on(id);
            let additional = self.followup_from_answer(&completed.answer, &completed.request);
            followups.extend(self.submit_followups(additional));
            Ok(ScheduledAnswer {
                completed,
                followups,
            })
        } else {
            self.answered.insert(id, completed.clone());
            for dep_id in &pending_deps {
                self.blocked
                    .entry(*dep_id)
                    .or_default()
                    .push(completed.request.clone());
            }
            let followups = self.retry_requests_blocked_on(id);
            Ok(ScheduledAnswer {
                completed,
                followups,
            })
        }
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

    pub fn blocked_len(&self) -> usize {
        self.blocked.len()
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

    fn pending_dependencies(&self, id: RequestId) -> Vec<RequestId> {
        self.dependencies
            .get(&id)
            .map(|deps| {
                deps.iter()
                    .filter(|dep_id| !self.answered.contains_key(dep_id))
                    .copied()
                    .collect()
            })
            .unwrap_or_default()
    }

    fn submit_followups(&mut self, followup_work: Vec<CompilerWork>) -> Vec<RequestId> {
        let mut followups = Vec::with_capacity(followup_work.len());
        for work in followup_work.into_iter().rev() {
            followups.push(self.submit(work));
        }
        followups.reverse();
        followups
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

    fn followup_from_answer(&self, answer: &CompilerAnswer, request: &CompilerRequest) -> Vec<CompilerWork> {
        match answer {
            CompilerAnswer::CompileUnitCompileNative => match &request.work {
                CompilerWork::CompileUnitCompileNative { ast, path } => {
                    vec![CompilerWork::CompileUnitCompileBytecode {
                        ast: ast.clone(),
                        path: path.clone(),
                    }]
                }
                _ => vec![],
            },
            _ => vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scheduler::{
        AstId, ConstValueId, FullyQualifiedPath, SchedulerError,
    };

    fn path(segments: &[&str]) -> FullyQualifiedPath {
        FullyQualifiedPath::from_segments(segments.iter().map(|seg| (*seg).to_string()).collect())
    }

    #[test]
    fn independent_submit_uses_lifo_stack_order() {
        let mut scheduler = CompilerScheduler::new();
        let first = scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: AstId::new("ast:crate::main"),
            path: path(&["crate", "main"]),
        });
        let second = scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: AstId::new("ast:crate::dep"),
            path: path(&["crate", "dep"]),
        });

        assert_eq!(first.as_u64(), 0);
        assert_eq!(second.as_u64(), 1);
        assert_eq!(scheduler.pending_len(), 2);

        let next = scheduler.next_request().expect("last-submitted request");
        assert_eq!(next.id, second);
        assert!(matches!(next.work, CompilerWork::CompileUnitCompileNative { .. }));
        assert_eq!(scheduler.pending_len(), 1);
        assert_eq!(scheduler.active_len(), 1);

        let next = scheduler.next_request().expect("first-submitted request");
        assert_eq!(next.id, first);
        assert!(matches!(next.work, CompilerWork::CompileUnitCompileNative { .. }));
    }

    #[test]
    fn answers_only_active_requests() {
        let mut scheduler = CompilerScheduler::new();
        let request = scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: AstId::new("ast:crate::main"),
            path: path(&["crate", "main"]),
        });

        let not_active = scheduler
            .answer(request, CompilerAnswer::CompileUnitCompileNative)
            .expect_err("pending request is not active");
        assert_eq!(not_active, SchedulerError::RequestNotActive(request));

        let active = scheduler.next_request().expect("active request");
        let completed = scheduler
            .answer(active.id, CompilerAnswer::CompileUnitCompileNative)
            .expect("answered request");
        assert_eq!(completed.request.id, request);
        assert_eq!(scheduler.active_len(), 0);
        assert_eq!(scheduler.answered_len(), 1);
        assert!(scheduler.is_idle());
    }

    #[test]
    fn rejects_duplicate_answers() {
        let mut scheduler = CompilerScheduler::new();
        let request = scheduler.submit(CompilerWork::CompileUnitAnswerComptime {
            ast: AstId::new("ast:crate::build"),
            path: path(&["crate", "build"]),
        });
        scheduler.next_request().expect("active request");
        scheduler
            .answer(
                request,
                CompilerAnswer::CompileUnitAnswerComptime {
                    value: crate::scheduler::ConstValueId::new("value:crate::build"),
                },
            )
            .expect("first answer");

        let duplicate = scheduler
            .answer(
                request,
                CompilerAnswer::CompileUnitAnswerComptime {
                    value: crate::scheduler::ConstValueId::new("value:crate::build"),
                },
            )
            .expect_err("duplicate answer");
        assert_eq!(duplicate, SchedulerError::RequestAlreadyAnswered(request));
    }

    #[test]
    fn implicit_dependency_auto_blocks_and_retries() {
        let mut scheduler = CompilerScheduler::new();

        let native_id = scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: AstId::new("ast:crate::main"),
            path: path(&["crate", "main"]),
        });
        let _active = scheduler.next_request().expect("active compile native");

        // Simulate the handler: set current_processing, submit a dependent work item
        scheduler.begin_processing(native_id);
        let comptime_id = scheduler.submit(CompilerWork::CompileUnitAnswerComptime {
            ast: AstId::new("ast:crate::main"),
            path: path(&["crate", "main"]),
        });
        scheduler.end_processing();

        // Answer the native unit — it should auto-block because CompileUnitAnswerComptime is pending
        let scheduled = scheduler
            .answer_and_schedule(native_id, CompilerAnswer::CompileUnitCompileNative)
            .expect("auto-blocked on comptime dependency");
        assert!(scheduled.followups.is_empty(), "should have no followups when auto-blocked");

        // Verify the blocked relationship
        assert_eq!(
            scheduler.blocked.get(&comptime_id).map(|v| v.len()),
            Some(1),
            "compile native should be blocked on comptime"
        );

        // Now complete the comptime work
        let _active = scheduler.next_request().expect("comptime work");
        let scheduled = scheduler
            .answer_and_schedule(
                comptime_id,
                CompilerAnswer::CompileUnitAnswerComptime {
                    value: ConstValueId::new("value:crate::main"),
                },
            )
            .expect("comptime answered");

        // Should retry the blocked CompileUnitCompileNative
        assert_eq!(
            scheduled.followups.len(),
            1,
            "completing dependency should retry blocked work"
        );
        let retried = scheduler.next_request().expect("retried compile native");
        assert!(matches!(
            retried.work,
            CompilerWork::CompileUnitCompileNative { .. }
        ));
    }

    #[test]
    fn load_package_dependency_auto_blocks_and_retries() {
        let mut scheduler = CompilerScheduler::new();

        let native_id = scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: AstId::new("ast:crate::main"),
            path: path(&["crate", "main"]),
        });
        let _active = scheduler.next_request().expect("active compile native");

        // Simulate the handler: the typer requested a not-yet-loaded package
        // and yielded — submit LoadPackage as a scheduler-dependency work item
        // exactly like the comptime case above.
        scheduler.begin_processing(native_id);
        let load_id = scheduler.submit(CompilerWork::LoadPackage {
            name: "std".to_string(),
        });
        scheduler.end_processing();

        // Answer the native unit — it should auto-block because LoadPackage is pending
        let scheduled = scheduler
            .answer_and_schedule(native_id, CompilerAnswer::CompileUnitCompileNative)
            .expect("auto-blocked on package dependency");
        assert!(scheduled.followups.is_empty(), "should have no followups when auto-blocked");

        assert_eq!(
            scheduler.blocked.get(&load_id).map(|v| v.len()),
            Some(1),
            "compile native should be blocked on package load"
        );

        // Now complete the package load
        let _active = scheduler.next_request().expect("load package work");
        let scheduled = scheduler
            .answer_and_schedule(
                load_id,
                CompilerAnswer::PackageLoaded {
                    name: "std".to_string(),
                },
            )
            .expect("package answered");

        // Should retry the blocked CompileUnitCompileNative
        assert_eq!(
            scheduled.followups.len(),
            1,
            "completing package load should retry blocked work"
        );
        let retried = scheduler.next_request().expect("retried compile native");
        assert!(matches!(
            retried.work,
            CompilerWork::CompileUnitCompileNative { .. }
        ));
    }

    #[test]
    fn no_auto_block_when_no_pending_dependencies() {
        let mut scheduler = CompilerScheduler::new();

        let native_id = scheduler.submit(CompilerWork::CompileUnitCompileNative {
            ast: AstId::new("ast:crate::main"),
            path: path(&["crate", "main"]),
        });
        let _active = scheduler.next_request().expect("active compile");

        // No dependencies submitted — should complete normally with bytecode followup
        let scheduled = scheduler
            .answer_and_schedule(native_id, CompilerAnswer::CompileUnitCompileNative)
            .expect("direct compile");
        assert_eq!(scheduled.followups.len(), 1, "bytecode followup");

        assert!(scheduler.blocked.is_empty(), "nothing blocked");
        // Drain the bytecode followup
        assert!(!scheduler.is_idle(), "bytecode work pending");
        scheduler.next_request();
        scheduler.answer(scheduled.followups[0], CompilerAnswer::CompileUnitCompileBytecode).ok();
        assert!(scheduler.is_idle());
    }
}
