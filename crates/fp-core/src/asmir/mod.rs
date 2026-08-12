pub mod pretty;
pub mod sysop;

use crate::container::ContainerFile;
use crate::lir::{
    CallingConvention, DebugInfo, Linkage, LirDataLayout, Name, StackSlot, Ty, Visibility,
};
use std::collections::BTreeMap;

pub use sysop::{AsmSysOp, PosixDirentStyle, PosixFlagStyle};

pub type AsmBlockId = u32;
pub type AsmInstrId = u32;
pub type AsmVirtualRegId = u32;
pub type AsmType = Ty;

#[derive(Debug, Clone, PartialEq)]
pub struct AsmProgram {
    pub target: AsmTarget,
    pub data_layout: LirDataLayout,
    /// If this program was lifted from an existing container / assembly stream,
    /// this records the original target so emitters can decide whether it is
    /// safe to reuse preserved machine encodings.
    pub lifted_from: Option<AsmTarget>,
    pub container: Option<ContainerFile>,
    pub sections: Vec<AsmSection>,
    pub globals: Vec<AsmGlobal>,
    pub functions: Vec<AsmFunction>,
    pub type_definitions: Vec<AsmTypeDefinition>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmTarget {
    pub architecture: AsmArchitecture,
    pub object_format: AsmObjectFormat,
    pub endianness: AsmEndianness,
    pub pointer_width: u16,
    pub default_calling_convention: Option<CallingConvention>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmArchitecture {
    X86_64,
    Aarch64,
    Arm,
    RiscV64,
    Bpf,
    Wasm32,
    Generic,
    Custom(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AsmObjectFormat {
    Elf,
    MachO,
    Coff,
    Pe,
    Wasm,
    Raw,
    Custom(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmEndianness {
    Little,
    Big,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmSection {
    pub name: String,
    pub kind: AsmSectionKind,
    pub flags: Vec<AsmSectionFlag>,
    pub alignment: Option<u64>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmSectionKind {
    Text,
    Data,
    ReadOnlyData,
    Bss,
    Tls,
    Metadata,
    Custom(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmSectionFlag {
    Allocate,
    Write,
    Execute,
    Merge,
    Strings,
    Tls,
    Custom(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmGlobal {
    pub name: Name,
    pub ty: AsmType,
    pub initializer: Option<AsmConstant>,
    pub relocations: Vec<AsmGlobalRelocation>,
    pub section: Option<String>,
    pub linkage: Linkage,
    pub visibility: Visibility,
    pub alignment: Option<u32>,
    pub is_constant: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AsmRelocationKind {
    Abs64,
    PcRel32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsmGlobalRelocation {
    pub offset: u64,
    pub kind: AsmRelocationKind,
    pub symbol: Name,
    pub addend: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmFunction {
    pub name: Name,
    pub signature: AsmFunctionSignature,
    pub basic_blocks: Vec<AsmBlock>,
    pub locals: Vec<AsmLocal>,
    pub stack_slots: Vec<AsmStackSlot>,
    pub frame: Option<AsmStackFrame>,
    pub linkage: Linkage,
    pub visibility: Visibility,
    pub calling_convention: Option<CallingConvention>,
    pub section: Option<String>,
    pub is_declaration: bool,
    /// Canonical type/bank/width for every virtual register this function
    /// defines. `AsmRegister::Virtual` carries only an id; this table is the
    /// sole source of truth for what that id means.
    pub virtual_registers: BTreeMap<AsmVirtualRegId, AsmVirtualRegister>,
    next_virtual_reg: AsmVirtualRegId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmVirtualRegister {
    pub ty: AsmType,
    pub bank: AsmRegisterBank,
    pub bits: u16,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmFunctionSignature {
    pub params: Vec<AsmType>,
    pub return_type: AsmType,
    pub is_variadic: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmStackFrame {
    pub stack_size: u32,
    pub stack_alignment: u32,
    pub callee_saved: Vec<AsmRegister>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmBlock {
    pub id: AsmBlockId,
    pub label: Option<Name>,
    pub instructions: Vec<AsmInstruction>,
    pub terminator: AsmTerminator,
    /// When lifting from an existing object, this may contain the original
    /// machine encoding for the terminator.
    pub terminator_encoding: Option<Vec<u8>>,
    pub predecessors: Vec<AsmBlockId>,
    pub successors: Vec<AsmBlockId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmInstruction {
    pub id: AsmInstrId,
    pub opcode: AsmOpcode,
    pub operands: Vec<AsmOperand>,
    pub implicit_uses: Vec<AsmRegister>,
    pub implicit_defs: Vec<AsmRegister>,
    pub encoding: Option<Vec<u8>>,
    pub debug_info: Option<DebugInfo>,
    pub annotations: Vec<AsmAnnotation>,
}

impl AsmInstruction {
    pub fn new(id: AsmInstrId, opcode: AsmOpcode, operands: Vec<AsmOperand>) -> Self {
        Self {
            id,
            opcode,
            operands,
            implicit_uses: Vec::new(),
            implicit_defs: Vec::new(),
            encoding: None,
            debug_info: None,
            annotations: Vec::new(),
        }
    }

    /// The register written by this instruction, if any. An instruction
    /// defines at most one value; that value is always a `Write` (or
    /// `ReadWrite`, for tied target operands) register operand.
    pub fn result_register(&self) -> Option<&AsmRegister> {
        self.operands.iter().find_map(|operand| match operand {
            AsmOperand::Register { reg, access } if *access != OperandAccess::Read => Some(reg),
            _ => None,
        })
    }

    /// For `Call`-opcode instructions: the call target and its argument
    /// operands, in that order. Operand layout is `[dest?] [Attr...] target
    /// arg...` — dest (if present) is always the sole `Write` register, and
    /// every `Attr` is metadata (calling convention, tail-call), so the
    /// first non-Attr, non-dest operand is unambiguously the target.
    /// Returns `None` for non-`Call` opcodes.
    pub fn call_target_and_args(&self) -> Option<(&AsmOperand, &[AsmOperand])> {
        if !matches!(self.opcode, AsmOpcode::Generic(AsmGenericOpcode::Call)) {
            return None;
        }
        let target_idx = self.operands.iter().position(|op| {
            !matches!(op, AsmOperand::Attr(_))
                && !matches!(
                    op,
                    AsmOperand::Register {
                        access: OperandAccess::Write,
                        ..
                    }
                )
        })?;
        Some((&self.operands[target_idx], &self.operands[target_idx + 1..]))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmOperand {
    Register {
        reg: AsmRegister,
        access: OperandAccess,
    },
    Immediate(i128),
    Memory(AsmMemoryOperand),
    Label(Name),
    Symbol(Name),
    Block(AsmBlockId),
    Relocation(AsmRelocationRef),
    Predicate {
        reg: AsmRegister,
        inverted: bool,
    },
    /// Reference to a function-local stack local, by id (see `AsmFunction::locals`).
    Local(u32),
    /// Reference to a function-local stack slot, by id (see `AsmFunction::stack_slots`).
    StackSlot(u32),
    /// A full-fidelity constant value (int/float/bool/string/bytes/array/
    /// struct/global-ref/function-ref/null/undef). Kept as one payload
    /// rather than decomposed, so no information is lost converting from
    /// the LIR-level constant and passes like string interning can pattern
    /// match on it directly.
    Constant(AsmConstant),
    /// A condition code, e.g. the third operand of a decomposed comparison
    /// (`lhs`, `rhs`, `Condition(cc)`).
    Condition(AsmConditionCode),
    SysOp(Box<AsmSysOp>),
    /// Non-def/use instruction metadata (alignment, volatility, calling
    /// convention, tail-call, landing-pad clause tags, inline-asm/intrinsic
    /// syntax text, ...). Kept as operands rather than instruction fields
    /// so `operands` remains the single source of truth for everything the
    /// instruction carries.
    Attr(AsmAttr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmAttr {
    Alignment(u32),
    Volatile,
    Inbounds,
    TailCall,
    SideEffects,
    AlignStack,
    Cleanup,
    CallingConv(CallingConvention),
    SyscallConvention(AsmSyscallConvention),
    Intrinsic(AsmIntrinsicKind),
    /// `IntrinsicCall`'s format string.
    Format(String),
    /// `InlineAsm`'s assembly text.
    AsmText(String),
    /// `InlineAsm`'s constraint string.
    Constraints(String),
    SymbolAddressKind(AsmSymbolAddressKind),
    /// Tags one value operand as a landing-pad `catch` clause.
    LandingPadCatch,
    /// Tags the following `n` value operands as one landing-pad `filter` clause.
    LandingPadFilter(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandAccess {
    Read,
    Write,
    ReadWrite,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmRegister {
    Physical(AsmPhysicalRegister),
    Virtual(AsmVirtualRegId),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmPhysicalRegister {
    pub name: String,
    pub bank: AsmRegisterBank,
    pub size_bits: u16,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmRegisterBank {
    General,
    Float,
    Vector,
    Predicate,
    /// Condition-code / flags register produced by compare-like opcodes and
    /// consumed by conditional branches/selects.
    Flags,
    Special,
    Custom(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmMemoryOperand {
    pub base: Option<AsmRegister>,
    pub index: Option<AsmRegister>,
    pub scale: u8,
    pub displacement: i64,
    pub segment: Option<AsmRegister>,
    pub size_bytes: Option<u16>,
    pub address_space: Option<u32>,
    pub pre_indexed: bool,
    pub post_indexed: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmRelocationRef {
    pub kind: String,
    pub symbol: Name,
    pub addend: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmAnnotation {
    pub key: String,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AsmOpcode {
    Generic(AsmGenericOpcode),
    Custom(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AsmGenericOpcode {
    Nop,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Not,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Ult,
    Ule,
    Ugt,
    Uge,
    Load,
    Store,
    Alloca,
    GetElementPtr,
    Bitcast,
    PtrToInt,
    IntToPtr,
    Trunc,
    ZExt,
    SExt,
    FPExt,
    FPTrunc,
    FPToUI,
    FPToSI,
    UIToFP,
    SIToFP,
    ExtractValue,
    InsertValue,
    Call,
    IntrinsicCall,
    SextOrTrunc,
    Phi,
    Select,
    InlineAsm,
    LandingPad,
    Unreachable,
    Freeze,
    Syscall,
    SysOp,
    Splat,
    BuildVector,
    ExtractLane,
    InsertLane,
    ZipLow,
    SymbolAddress,
}

impl AsmOpcode {
    pub fn mnemonic(&self) -> &str {
        match self {
            AsmOpcode::Generic(opcode) => opcode.mnemonic(),
            AsmOpcode::Custom(opcode) => opcode.as_str(),
        }
    }
}

impl AsmGenericOpcode {
    pub fn mnemonic(&self) -> &str {
        match self {
            AsmGenericOpcode::Nop => "nop",
            AsmGenericOpcode::Add => "add",
            AsmGenericOpcode::Sub => "sub",
            AsmGenericOpcode::Mul => "mul",
            AsmGenericOpcode::Div => "div",
            AsmGenericOpcode::Rem => "rem",
            AsmGenericOpcode::And => "and",
            AsmGenericOpcode::Or => "or",
            AsmGenericOpcode::Xor => "xor",
            AsmGenericOpcode::Shl => "shl",
            AsmGenericOpcode::Shr => "shr",
            AsmGenericOpcode::Not => "not",
            AsmGenericOpcode::Eq => "eq",
            AsmGenericOpcode::Ne => "ne",
            AsmGenericOpcode::Lt => "lt",
            AsmGenericOpcode::Le => "le",
            AsmGenericOpcode::Gt => "gt",
            AsmGenericOpcode::Ge => "ge",
            AsmGenericOpcode::Ult => "ult",
            AsmGenericOpcode::Ule => "ule",
            AsmGenericOpcode::Ugt => "ugt",
            AsmGenericOpcode::Uge => "uge",
            AsmGenericOpcode::Load => "load",
            AsmGenericOpcode::Store => "store",
            AsmGenericOpcode::Alloca => "alloca",
            AsmGenericOpcode::GetElementPtr => "gep",
            AsmGenericOpcode::Bitcast => "bitcast",
            AsmGenericOpcode::PtrToInt => "ptrtoint",
            AsmGenericOpcode::IntToPtr => "inttoptr",
            AsmGenericOpcode::Trunc => "trunc",
            AsmGenericOpcode::ZExt => "zext",
            AsmGenericOpcode::SExt => "sext",
            AsmGenericOpcode::FPExt => "fpext",
            AsmGenericOpcode::FPTrunc => "fptrunc",
            AsmGenericOpcode::FPToUI => "fptoui",
            AsmGenericOpcode::FPToSI => "fptosi",
            AsmGenericOpcode::UIToFP => "uitofp",
            AsmGenericOpcode::SIToFP => "sitofp",
            AsmGenericOpcode::ExtractValue => "extractvalue",
            AsmGenericOpcode::InsertValue => "insertvalue",
            AsmGenericOpcode::Call => "call",
            AsmGenericOpcode::IntrinsicCall => "intrinsic.call",
            AsmGenericOpcode::SextOrTrunc => "sextortrunc",
            AsmGenericOpcode::Phi => "phi",
            AsmGenericOpcode::Select => "select",
            AsmGenericOpcode::InlineAsm => "inlineasm",
            AsmGenericOpcode::LandingPad => "landingpad",
            AsmGenericOpcode::Unreachable => "unreachable",
            AsmGenericOpcode::Freeze => "freeze",
            AsmGenericOpcode::Syscall => "syscall",
            AsmGenericOpcode::SysOp => "sysop",
            AsmGenericOpcode::Splat => "splat",
            AsmGenericOpcode::BuildVector => "build_vector",
            AsmGenericOpcode::ExtractLane => "extract_lane",
            AsmGenericOpcode::InsertLane => "insert_lane",
            AsmGenericOpcode::ZipLow => "zip_low",
            AsmGenericOpcode::SymbolAddress => "symbol_address",
        }
    }

    /// Whether this opcode always defines a result when selected. `Call`,
    /// `IntrinsicCall`, `Syscall`, and `InlineAsm` are excluded because
    /// voidness is data-driven (a call may or may not have a destination),
    /// not implied by the opcode alone.
    pub fn always_defines_result(&self) -> bool {
        !matches!(
            self,
            AsmGenericOpcode::Nop
                | AsmGenericOpcode::Store
                | AsmGenericOpcode::Unreachable
                | AsmGenericOpcode::Call
                | AsmGenericOpcode::IntrinsicCall
                | AsmGenericOpcode::Syscall
                | AsmGenericOpcode::InlineAsm
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmSyscallConvention {
    LinuxX86_64,
    LinuxAarch64,
    DarwinX86_64,
    DarwinAarch64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmSymbolAddressKind {
    Direct,
    Got,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmIntrinsicKind {
    Print,
    Println,
    Format,
    TimeNow,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmTerminator {
    Return(Option<AsmOperand>),
    Br(AsmBlockId),
    CondBr {
        condition: AsmOperand,
        if_true: AsmBlockId,
        if_false: AsmBlockId,
    },
    Switch {
        value: AsmOperand,
        default: AsmBlockId,
        cases: Vec<(u64, AsmBlockId)>,
    },
    IndirectBr {
        address: AsmOperand,
        destinations: Vec<AsmBlockId>,
    },
    Invoke {
        function: AsmOperand,
        args: Vec<AsmOperand>,
        normal_dest: AsmBlockId,
        unwind_dest: AsmBlockId,
        calling_convention: CallingConvention,
    },
    Resume(AsmOperand),
    Unreachable,
    CleanupRet {
        cleanup_pad: AsmOperand,
        unwind_dest: Option<AsmBlockId>,
    },
    CatchRet {
        catch_pad: AsmOperand,
        successor: AsmBlockId,
    },
    CatchSwitch {
        parent_pad: Option<AsmOperand>,
        handlers: Vec<AsmBlockId>,
        unwind_dest: Option<AsmBlockId>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmConditionCode {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Ult,
    Ule,
    Ugt,
    Uge,
    Nz,
}

/// An address computed from operand-like parts, used internally while
/// selection/GEP-folding builds up an effective address before it is
/// finalized into an `AsmOperand::Memory` (or a plain register/local/stack
/// operand, if it never needed a full addressing mode).
#[derive(Debug, Clone, PartialEq)]
pub struct AsmAddressValue {
    pub base: Option<Box<AsmOperand>>,
    pub index: Option<Box<AsmOperand>>,
    pub scale: u8,
    pub displacement: i64,
    pub segment: Option<Box<AsmOperand>>,
    pub size_bytes: Option<u16>,
    pub address_space: Option<u32>,
    pub pre_indexed: bool,
    pub post_indexed: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmConstant {
    Int(i64, AsmType),
    UInt(u64, AsmType),
    Float(f64, AsmType),
    Bool(bool),
    String(String),
    Bytes(Vec<u8>),
    Array(Vec<AsmConstant>, AsmType),
    Struct(Vec<AsmConstant>, AsmType),
    GlobalRef(Name, AsmType, Vec<u64>),
    FunctionRef(Name, AsmType),
    Null(AsmType),
    Undef(AsmType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmTypeDefinition {
    pub name: Name,
    pub ty: AsmType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmLocal {
    pub id: u32,
    pub ty: AsmType,
    pub name: Option<String>,
    pub is_argument: bool,
}

pub type AsmStackSlot = StackSlot;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AsmValidationError {
    pub function: Name,
    pub block: Option<AsmBlockId>,
    pub instruction: Option<AsmInstrId>,
    pub operand_index: Option<usize>,
    pub register: Option<AsmVirtualRegId>,
    pub message: String,
}

impl std::fmt::Display for AsmValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.function, self.message)?;
        if let Some(block) = self.block {
            write!(f, " (block bb{block}")?;
            if let Some(instruction) = self.instruction {
                write!(f, ", instruction #{instruction}")?;
            }
            if let Some(idx) = self.operand_index {
                write!(f, ", operand {idx}")?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl std::error::Error for AsmValidationError {}

impl AsmProgram {
    pub fn new(target: AsmTarget, data_layout: LirDataLayout) -> Self {
        Self {
            target,
            data_layout,
            lifted_from: None,
            container: None,
            sections: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            type_definitions: Vec::new(),
        }
    }
}

impl AsmTarget {
    pub fn data_layout(&self) -> LirDataLayout {
        let pointer_size = u32::from(self.pointer_width / 8);
        LirDataLayout::new(
            u32::from(self.pointer_width),
            pointer_size,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .expect("AsmTarget must define a valid data layout")
    }
}

impl AsmGlobal {
    pub fn clear_initializer(&mut self) {
        self.relocations.clear();
        self.initializer = None;
    }
}

impl AsmFunction {
    /// Constructs an empty function declaration. `virtual_registers` starts
    /// empty and its allocator id space is private to this `AsmFunction` —
    /// callers must use `alloc_virtual_register` rather than assembling
    /// `AsmRegister::Virtual` ids by hand.
    pub fn new(name: Name, signature: AsmFunctionSignature) -> Self {
        Self {
            name,
            signature,
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
            virtual_registers: BTreeMap::new(),
            next_virtual_reg: 0,
        }
    }

    /// Allocates a fresh virtual register id, independent of any
    /// `AsmInstrId` space, and records its canonical type/bank/width.
    pub fn alloc_virtual_register(
        &mut self,
        ty: AsmType,
        bank: AsmRegisterBank,
        bits: u16,
    ) -> AsmVirtualRegId {
        let id = self.next_virtual_reg;
        self.next_virtual_reg = self
            .next_virtual_reg
            .checked_add(1)
            .expect("AsmFunction virtual register id space exhausted");
        self.virtual_registers
            .insert(id, AsmVirtualRegister { ty, bank, bits });
        id
    }

    pub fn virtual_register(&self, id: AsmVirtualRegId) -> Option<&AsmVirtualRegister> {
        self.virtual_registers.get(&id)
    }

    /// Structural well-formedness checks over this function's canonical
    /// AsmIR: every virtual-register use resolves to a declared register,
    /// every write is to a declared register, and every opcode that always
    /// defines a result actually has one. This does not (yet) enforce a
    /// full per-opcode operand arity/kind schema.
    pub fn validate(&self) -> Result<(), Vec<AsmValidationError>> {
        let mut errors = Vec::new();

        for (id, _) in &self.virtual_registers {
            if *id >= self.next_virtual_reg {
                errors.push(AsmValidationError {
                    function: self.name.clone(),
                    block: None,
                    instruction: None,
                    operand_index: None,
                    register: Some(*id),
                    message: format!(
                        "virtual register v{id} is declared past the allocator's next id (v{})",
                        self.next_virtual_reg
                    ),
                });
            }
        }

        for block in &self.basic_blocks {
            for instruction in &block.instructions {
                let mut has_result = false;
                for (idx, operand) in instruction.operands.iter().enumerate() {
                    let (reg, access) = match operand {
                        AsmOperand::Register { reg, access } => (reg, Some(*access)),
                        AsmOperand::Predicate { reg, .. } => (reg, None),
                        _ => continue,
                    };
                    let AsmRegister::Virtual(id) = reg else {
                        continue;
                    };
                    if !self.virtual_registers.contains_key(id) {
                        errors.push(AsmValidationError {
                            function: self.name.clone(),
                            block: Some(block.id),
                            instruction: Some(instruction.id),
                            operand_index: Some(idx),
                            register: Some(*id),
                            message: format!("use of undeclared virtual register v{id}"),
                        });
                    }
                    if matches!(access, Some(OperandAccess::Write) | Some(OperandAccess::ReadWrite)) {
                        has_result = true;
                    }
                }

                if let AsmOpcode::Generic(opcode) = &instruction.opcode {
                    if opcode.always_defines_result() && !has_result {
                        errors.push(AsmValidationError {
                            function: self.name.clone(),
                            block: Some(block.id),
                            instruction: Some(instruction.id),
                            operand_index: None,
                            register: None,
                            message: format!(
                                "opcode {} always defines a result but instruction has no write operand",
                                opcode.mnemonic()
                            ),
                        });
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_function() -> AsmFunction {
        AsmFunction {
            name: Name::new("main"),
            signature: AsmFunctionSignature {
                params: Vec::new(),
                return_type: Ty::I32,
                is_variadic: false,
            },
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: Some(AsmStackFrame {
                stack_size: 0,
                stack_alignment: 16,
                callee_saved: Vec::new(),
            }),
            linkage: Linkage::External,
            visibility: Visibility::Default,
            calling_convention: Some(CallingConvention::X86_64SysV),
            section: Some(".text".to_string()),
            is_declaration: false,
            virtual_registers: BTreeMap::new(),
            next_virtual_reg: 0,
        }
    }

    #[test]
    fn asmir_program_construction_is_stable() {
        let target = AsmTarget {
            architecture: AsmArchitecture::X86_64,
            object_format: AsmObjectFormat::Elf,
            endianness: AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: Some(CallingConvention::X86_64SysV),
        };
        let mut program = AsmProgram::new(target.clone(), target.data_layout());
        program.sections.push(AsmSection {
            name: ".text".to_string(),
            kind: AsmSectionKind::Text,
            flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
            alignment: Some(16),
        });

        let mut function = sample_function();
        let dest = function.alloc_virtual_register(Ty::I32, AsmRegisterBank::General, 32);
        function.basic_blocks.push(AsmBlock {
            id: 0,
            label: Some(Name::new("entry")),
            instructions: vec![AsmInstruction::new(
                0,
                AsmOpcode::Generic(AsmGenericOpcode::Freeze),
                vec![
                    AsmOperand::Register {
                        reg: AsmRegister::Virtual(dest),
                        access: OperandAccess::Write,
                    },
                    AsmOperand::Immediate(0),
                ],
            )],
            terminator: AsmTerminator::Return(Some(AsmOperand::Register {
                reg: AsmRegister::Virtual(dest),
                access: OperandAccess::Read,
            })),
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        });
        program.functions.push(function);

        assert_eq!(program.functions.len(), 1);
        assert_eq!(
            program.functions[0].basic_blocks[0].instructions[0].opcode,
            AsmOpcode::Generic(AsmGenericOpcode::Freeze)
        );
        assert!(program.functions[0].validate().is_ok());
    }

    #[test]
    fn validate_rejects_undeclared_virtual_register() {
        let mut function = sample_function();
        function.basic_blocks.push(AsmBlock {
            id: 0,
            label: None,
            instructions: vec![AsmInstruction::new(
                0,
                AsmOpcode::Generic(AsmGenericOpcode::Freeze),
                vec![
                    AsmOperand::Register {
                        reg: AsmRegister::Virtual(0),
                        access: OperandAccess::Write,
                    },
                    AsmOperand::Immediate(0),
                ],
            )],
            terminator: AsmTerminator::Unreachable,
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        });

        let errors = function.validate().expect_err("undeclared register");
        assert!(errors.iter().any(|e| e.register == Some(0)));
    }

    #[test]
    fn validate_rejects_missing_result_for_result_opcode() {
        let mut function = sample_function();
        function.basic_blocks.push(AsmBlock {
            id: 0,
            label: None,
            instructions: vec![AsmInstruction::new(
                0,
                AsmOpcode::Generic(AsmGenericOpcode::Add),
                vec![AsmOperand::Immediate(1), AsmOperand::Immediate(2)],
            )],
            terminator: AsmTerminator::Unreachable,
            terminator_encoding: None,
            predecessors: Vec::new(),
            successors: Vec::new(),
        });

        let errors = function.validate().expect_err("missing result");
        assert!(errors.iter().any(|e| e.message.contains("always defines a result")));
    }
}
