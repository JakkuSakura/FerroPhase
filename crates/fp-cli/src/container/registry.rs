use fp_core::container::{
    ContainerArchitecture, ContainerEndianness, ContainerFile, ContainerFormat, ContainerKind,
    ContainerReader, ContainerSection, ContainerSectionKind,
};

use crate::error::{CliError, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ContainerInputKind {
    NativeObject,
    NativeArchive,
    JvmBytecode,
    Cil,
    GoAsm,
    Urcl,
}

pub(crate) struct ReadContainer {
    pub(crate) kind: ContainerInputKind,
    pub(crate) payload: Vec<u8>,
}

pub(crate) struct ContainerRegistry {
    object_reader: fp_native::container::ObjectContainerReader,
}

impl ContainerRegistry {
    pub(crate) fn new() -> Self {
        Self {
            object_reader: fp_native::container::ObjectContainerReader::new(),
        }
    }

    pub(crate) fn detect_input_kind(
        &self,
        input: &std::path::Path,
        source_language: Option<&str>,
    ) -> Option<ContainerInputKind> {
        let extension = input
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| ext.to_ascii_lowercase());

        if let Some(lang) = source_language.map(|lang| lang.trim().to_ascii_lowercase()) {
            match lang.as_str() {
                "object" | "native-object" | "obj" | "native-obj" | "o" => {
                    return Some(ContainerInputKind::NativeObject);
                }
                "archive" | "ar" | "native-archive" | "a" | "lib" => {
                    return Some(ContainerInputKind::NativeArchive);
                }
                "jvm" | "jvm-bytecode" | "bytecode-jvm" | "class" | "jar" => {
                    return Some(ContainerInputKind::JvmBytecode);
                }
                "cil" | "msil" | "dotnet-cil" => {
                    return Some(ContainerInputKind::Cil);
                }
                "goasm" | "go-asm" => {
                    return Some(ContainerInputKind::GoAsm);
                }
                "urcl" => {
                    return Some(ContainerInputKind::Urcl);
                }
                _ => {}
            }
        }

        match extension.as_deref() {
            Some("o" | "obj") => Some(ContainerInputKind::NativeObject),
            Some("a" | "lib") => Some(ContainerInputKind::NativeArchive),
            Some("class" | "jar") => Some(ContainerInputKind::JvmBytecode),
            Some("il" | "dll" | "exe") => Some(ContainerInputKind::Cil),
            Some("goasm") => Some(ContainerInputKind::GoAsm),
            Some("urcl") => Some(ContainerInputKind::Urcl),
            _ => {
                // Magic sniff: allow container inputs without a canonical extension.
                // This is intentionally shallow (header-based), matching `inspect` behavior.
                let prefix = {
                    use std::io::Read;

                    let mut file = std::fs::File::open(input).ok()?;
                    let mut buf = vec![0u8; 4096];
                    let n = file.read(&mut buf).ok()?;
                    buf.truncate(n);
                    buf
                };

                if self.object_reader.can_read(&prefix) {
                    return Some(ContainerInputKind::NativeObject);
                }
                if fp_native::archive::can_read_archive(&prefix) {
                    return Some(ContainerInputKind::NativeArchive);
                }
                if prefix.starts_with(b"PK\x03\x04") || prefix.starts_with(b"\xCA\xFE\xBA\xBE") {
                    return Some(ContainerInputKind::JvmBytecode);
                }
                if prefix.starts_with(b"MZ") {
                    // This could also be a native PE, but we default to the .NET ecosystem
                    // container unless explicitly overridden via `--source-language`.
                    return Some(ContainerInputKind::Cil);
                }
                None
            }
        }
    }

    pub(crate) fn read_container(
        &self,
        kind: ContainerInputKind,
        payload: Vec<u8>,
    ) -> Result<ReadContainer> {
        let _container = match kind {
            ContainerInputKind::NativeObject => {
                if !self.object_reader.can_read(&payload) {
                    return Err(CliError::InvalidInput(
                        "input is not a recognized object container".to_string(),
                    ));
                }
                self.object_reader.read(&payload).map_err(|err| {
                    CliError::Compilation(format!("Failed to parse object container: {err}"))
                })?
            }
            ContainerInputKind::NativeArchive => {
                if !fp_native::archive::can_read_archive(&payload) {
                    return Err(CliError::InvalidInput(
                        "input is not a recognized native archive container".to_string(),
                    ));
                }

                let mut file = ContainerFile::new(
                    ContainerKind::Archive,
                    ContainerFormat::Other("archive(ar)".to_string()),
                    ContainerArchitecture::Other("native".to_string()),
                    ContainerEndianness::Little,
                );
                file.sections.push(ContainerSection {
                    name: ".container".to_string(),
                    kind: ContainerSectionKind::Other,
                    align: 1,
                    data: payload.clone(),
                });
                file
            }
            ContainerInputKind::JvmBytecode => {
                // Keep this container representation lossless by storing raw bytes.
                let format = if payload.starts_with(b"PK\x03\x04") {
                    ContainerFormat::Jar
                } else {
                    ContainerFormat::Class
                };
                let mut file = ContainerFile::new(
                    ContainerKind::Other,
                    format,
                    ContainerArchitecture::Other("jvm".to_string()),
                    ContainerEndianness::Little,
                );
                file.sections.push(ContainerSection {
                    name: ".container".to_string(),
                    kind: ContainerSectionKind::Other,
                    align: 1,
                    data: payload.clone(),
                });
                file
            }
            ContainerInputKind::Cil => {
                // `.il` is textual; `.dll/.exe` is PE. We keep both lossless.
                let is_pe = payload.starts_with(b"MZ");
                let format = if is_pe {
                    ContainerFormat::Pe
                } else {
                    ContainerFormat::Cil
                };
                let mut file = ContainerFile::new(
                    ContainerKind::Other,
                    format,
                    ContainerArchitecture::Other("cil".to_string()),
                    ContainerEndianness::Little,
                );
                file.sections.push(ContainerSection {
                    name: ".container".to_string(),
                    kind: ContainerSectionKind::Other,
                    align: 1,
                    data: payload.clone(),
                });
                file
            }
            ContainerInputKind::GoAsm => {
                let mut file = ContainerFile::new(
                    ContainerKind::Other,
                    ContainerFormat::Other("goasm".to_string()),
                    ContainerArchitecture::Other("goasm".to_string()),
                    ContainerEndianness::Little,
                );
                file.sections.push(ContainerSection {
                    name: ".container".to_string(),
                    kind: ContainerSectionKind::Other,
                    align: 1,
                    data: payload.clone(),
                });
                file
            }
            ContainerInputKind::Urcl => {
                let mut file = ContainerFile::new(
                    ContainerKind::Other,
                    ContainerFormat::Other("urcl".to_string()),
                    ContainerArchitecture::Other("urcl".to_string()),
                    ContainerEndianness::Little,
                );
                file.sections.push(ContainerSection {
                    name: ".container".to_string(),
                    kind: ContainerSectionKind::Other,
                    align: 1,
                    data: payload.clone(),
                });
                file
            }
        };

        Ok(ReadContainer { kind, payload })
    }
}
