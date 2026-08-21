use fp_core::asmir::AsmObjectFormat;
use fp_core::container::{
    ContainerArchitecture, ContainerEndianness, ContainerFile, ContainerKind, ContainerSection,
    ContainerSectionKind,
};

use crate::error::{CliError, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ContainerInputKind {
    NativeArchive,
    JvmBytecode,
    Cil,
    GoAsm,
    Urcl,
}

/// What `classify_input` decided a path is, computed exactly once per input
/// file. Replaces three independent detectors (`detect_input_kind`,
/// `detect_native_object_source`, `detect_native_asm_source`) that used to
/// be called at different points in the compile hot path — sometimes more
/// than once each — re-deriving the same answer (and, for the byte-sniffed
/// case, re-reading the file) every time.
///
/// Native objects and native asm text are *not* represented here even
/// though they're foreign artifacts too — both are real, registered
/// languages (`languages::NATIVE_OBJECT`/`NATIVE_ASM`, see
/// `fp_native::NativeObjectPackageProvider`), so they already flow through
/// `InputClass::Source`'s ordinary language-registry path with no
/// container-specific branch anywhere in `compile.rs`/`pipeline.rs`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InputClass {
    Container(ContainerInputKind),
    /// Not a recognized bespoke-pipeline container — the source-language
    /// registry (`languages::detect_source_language`/`provider_for_language`)
    /// owns this input instead (this also covers native objects/asm text).
    Source,
}

pub(crate) struct ReadContainer {
    pub(crate) kind: ContainerInputKind,
    pub(crate) payload: Vec<u8>,
}

pub(crate) struct ContainerRegistry;

impl ContainerRegistry {
    pub(crate) fn new() -> Self {
        Self
    }

    /// Classifies `input` exactly once: an explicit `--source-language`
    /// override wins outright (container keywords), else
    /// the extension decides, else (only for inputs with neither) a shallow
    /// magic-byte sniff of the first 4KiB — matching `inspect`'s behavior —
    /// decides among the container kinds. Never sniffs for asm: unlike a
    /// container format, plain assembly text has no header to recognize
    /// without an extension or an explicit override.
    pub(crate) fn classify_input(
        &self,
        input: &std::path::Path,
        source_language: Option<&str>,
    ) -> InputClass {
        if let Some(lang) = source_language.map(|lang| lang.trim().to_ascii_lowercase()) {
            match lang.as_str() {
                "archive" | "ar" | "native-archive" | "a" | "lib" => {
                    return InputClass::Container(ContainerInputKind::NativeArchive);
                }
                "jvm" | "jvm-bytecode" | "bytecode-jvm" | "class" | "jar" => {
                    return InputClass::Container(ContainerInputKind::JvmBytecode);
                }
                "cil" | "msil" | "dotnet-cil" => {
                    return InputClass::Container(ContainerInputKind::Cil);
                }
                "goasm" | "go-asm" => {
                    return InputClass::Container(ContainerInputKind::GoAsm);
                }
                "urcl" => {
                    return InputClass::Container(ContainerInputKind::Urcl);
                }
                // Native objects and asm text (including all their dialect
                // aliases, e.g. "x86_64-asm") are real registered languages
                // now (`languages::NATIVE_OBJECT`/`NATIVE_ASM`) — the
                // override string is handed straight to the language
                // registry as-is, not reclassified here.
                _ => return InputClass::Source,
            }
        }

        let extension = input
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| ext.to_ascii_lowercase());

        match extension.as_deref() {
            Some("a" | "lib") => InputClass::Container(ContainerInputKind::NativeArchive),
            Some("class" | "jar") => InputClass::Container(ContainerInputKind::JvmBytecode),
            Some("il" | "dll" | "exe") => InputClass::Container(ContainerInputKind::Cil),
            Some("goasm") => InputClass::Container(ContainerInputKind::GoAsm),
            Some("urcl") => InputClass::Container(ContainerInputKind::Urcl),
            _ => self
                .sniff_container_kind(input)
                .map(InputClass::Container)
                .unwrap_or(InputClass::Source),
        }
    }

    /// Magic-sniff fallback for container inputs with no canonical
    /// extension (e.g. `/tmp/ls`) — intentionally shallow (header-based),
    /// matching `inspect`'s own behavior. Reads at most 4KiB, once.
    fn sniff_container_kind(&self, input: &std::path::Path) -> Option<ContainerInputKind> {
        let prefix = {
            use std::io::Read;

            let mut file = std::fs::File::open(input).ok()?;
            let mut buf = vec![0u8; 4096];
            let n = file.read(&mut buf).ok()?;
            buf.truncate(n);
            buf
        };

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

    pub(crate) fn read_container(
        &self,
        kind: ContainerInputKind,
        payload: Vec<u8>,
    ) -> Result<ReadContainer> {
        let _container = match kind {
            ContainerInputKind::NativeArchive => {
                if !fp_native::archive::can_read_archive(&payload) {
                    return Err(CliError::InvalidInput(
                        "input is not a recognized native archive container".to_string(),
                    ));
                }

                let mut file = ContainerFile::new(
                    ContainerKind::Archive,
                    AsmObjectFormat::Custom("archive(ar)".to_string()),
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
                    AsmObjectFormat::Custom("jar".to_string())
                } else {
                    AsmObjectFormat::Custom("class".to_string())
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
                    AsmObjectFormat::Pe
                } else {
                    AsmObjectFormat::Custom("cil".to_string())
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
                    AsmObjectFormat::Custom("goasm".to_string()),
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
                    AsmObjectFormat::Custom("urcl".to_string()),
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
