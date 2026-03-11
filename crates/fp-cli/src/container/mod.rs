use fp_core::container::{ContainerFile, ContainerReader};

use crate::error::{CliError, Result};

pub(crate) struct ContainerRegistry {
    object_reader: fp_native::container::ObjectContainerReader,
}

impl ContainerRegistry {
    pub(crate) fn new() -> Self {
        Self {
            object_reader: fp_native::container::ObjectContainerReader::new(),
        }
    }

    pub(crate) fn read_native_object(&self, bytes: &[u8]) -> Result<ContainerFile> {
        if !self.object_reader.can_read(bytes) {
            return Err(CliError::InvalidInput(
                "input is not a recognized object container".to_string(),
            ));
        }
        self.object_reader
            .read(bytes)
            .map_err(|err| CliError::Compilation(format!("Failed to parse object container: {err}")))
    }
}

