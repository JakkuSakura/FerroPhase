mod pipeline;
mod registry;

pub(crate) use pipeline::maybe_transpile_container;
pub(crate) use registry::{ContainerInputKind, ContainerRegistry};

