use fp_core::asmir::AsmObjectFormat;
use fp_core::container::{
    ContainerArchitecture, ContainerEndianness, ContainerFile, ContainerKind, ContainerSection,
    ContainerSectionKind,
};

use crate::error::{CliError, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ContainerInputKind {
    NativeArchive,
}

/// What `classify_input` decided a path is, computed exactly once per input
/// file. Replaces three independent detectors (`detect_input_kind`,
/// `detect_native_object_source`, `detect_native_asm_source`) that used to
/// be called at different points in the compile hot path — sometimes more
/// than once each — re-deriving the same answer (and, for the byte-sniffed
/// case, re-reading the file) every time.
///
/// Native objects/asm text, goasm, URCL, JVM bytecode, and CIL/.NET are
/// *not* represented here even though they're foreign artifacts too —
/// all are real, registered languages (`languages::NATIVE_OBJECT`/
/// `NATIVE_ASM`/`GOASM`/`URCL`/`JVM_BYTECODE`/`CIL`), so they already flow
/// through `InputClass::Source`'s ordinary language-registry path with no
/// container-specific branch anywhere in `compile.rs`/`pipeline.rs`.
/// `NativeArchive` is the one kind still on the bespoke pipeline (no
/// `PackageProvider` migration for it yet).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InputClass {
    Container(ContainerInputKind),
    /// Not a recognized bespoke-pipeline container — the source-language
    /// registry (`languages::detect_source_language`/`provider_for_language`)
    /// owns this input instead.
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
    /// decides among the container kinds.
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
                // Native objects/asm text, goasm, URCL, JVM bytecode, and
                // CIL/.NET (including all their dialect aliases, e.g.
                // "x86_64-asm") are real registered languages now
                // (`languages::NATIVE_OBJECT`/`NATIVE_ASM`/`GOASM`/`URCL`/
                // `JVM_BYTECODE`/`CIL`) — the override string is handed
                // straight to the language registry as-is, not
                // reclassified here.
                _ => return InputClass::Source,
            }
        }

        let extension = input
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| ext.to_ascii_lowercase());

        match extension.as_deref() {
            Some("a" | "lib") => InputClass::Container(ContainerInputKind::NativeArchive),
            _ => self
                .sniff_container_kind(input)
                .map(InputClass::Container)
                .unwrap_or(InputClass::Source),
        }
    }

    /// Magic-sniff fallback for container inputs with no canonical
    /// extension (e.g. `/tmp/libfoo`) — intentionally shallow
    /// (header-based), matching `inspect`'s own behavior. Reads at most
    /// 4KiB, once.
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
        };

        Ok(ReadContainer { kind, payload })
    }
}
