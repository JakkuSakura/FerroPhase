use thiserror::Error;

use super::identity::RequestId;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum SchedulerError {
    #[error("request {0} is not active")]
    RequestNotActive(RequestId),
    #[error("request {0} has already been answered")]
    RequestAlreadyAnswered(RequestId),
}
