use serde::{Deserialize, Serialize};

use super::identity::RequestId;
use super::work::{CompilerAnswer, CompilerWork};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CompilerRequest {
    pub id: RequestId,
    pub work: CompilerWork,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompletedRequest {
    pub request: CompilerRequest,
    pub answer: CompilerAnswer,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScheduledAnswer {
    pub completed: CompletedRequest,
    pub followups: Vec<RequestId>,
}
