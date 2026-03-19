use crate::asmir::AsmObjectFormat;
use crate::error::{Error, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContainerKind {
    Object,
    Executable,
    Archive,
    Other,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContainerArchitecture {
    X86_64,
    Aarch64,
    Other(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContainerEndianness {
    Little,
    Big,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContainerSectionKind {
    Text,
    ReadOnlyData,
    Data,
    Bss,
    Debug,
    Other,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContainerSection {
    pub name: String,
    pub kind: ContainerSectionKind,
    pub align: u64,
    pub data: Vec<u8>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContainerSymbolKind {
    Text,
    Data,
    Section,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContainerSymbolScope {
    Local,
    Global,
    Weak,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContainerSymbol {
    pub name: String,
    pub kind: ContainerSymbolKind,
    pub scope: ContainerSymbolScope,

    pub section: Option<usize>,
    pub value: u64,
    pub size: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContainerRelocationTarget {
    Symbol(String),
    Section(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContainerRelocationKind {
    Absolute,
    Relative,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContainerRelocationEncoding {
    Generic,
    X86Branch,
    Aarch64Call,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ContainerRelocationSpec {
    pub kind: ContainerRelocationKind,
    pub encoding: ContainerRelocationEncoding,
    pub size: u8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContainerRelocation {
    pub section: usize,
    pub offset: u64,
    pub target: ContainerRelocationTarget,
    pub addend: i64,
    pub spec: ContainerRelocationSpec,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContainerFile {
    pub kind: ContainerKind,
    pub format: AsmObjectFormat,
    pub architecture: ContainerArchitecture,
    pub endianness: ContainerEndianness,

    pub sections: Vec<ContainerSection>,
    pub symbols: Vec<ContainerSymbol>,
    pub relocations: Vec<ContainerRelocation>,
}

impl ContainerFile {
    pub fn new(
        kind: ContainerKind,
        format: AsmObjectFormat,
        architecture: ContainerArchitecture,
        endianness: ContainerEndianness,
    ) -> Self {
        Self {
            kind,
            format,
            architecture,
            endianness,
            sections: Vec::new(),
            symbols: Vec::new(),
            relocations: Vec::new(),
        }
    }

    pub fn add_section(&mut self, section: ContainerSection) -> usize {
        let id = self.sections.len();
        self.sections.push(section);
        id
    }

    pub fn add_symbol(&mut self, symbol: ContainerSymbol) {
        self.symbols.push(symbol);
    }

    pub fn add_relocation(&mut self, relocation: ContainerRelocation) {
        self.relocations.push(relocation);
    }
}

pub trait ContainerReader {
    fn can_read(&self, bytes: &[u8]) -> bool;
    fn read(&self, bytes: &[u8]) -> Result<ContainerFile>;
}

pub trait ContainerWriter {
    fn can_write(&self, format: &AsmObjectFormat) -> bool;
    fn write(&self, container: &ContainerFile) -> Result<Vec<u8>>;
}

pub fn unsupported(message: impl std::fmt::Display) -> Error {
    Error::from(format!("container: {message}"))
}
