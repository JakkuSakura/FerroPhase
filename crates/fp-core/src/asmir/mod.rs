pub mod pretty;
pub mod sysop;

use crate::lir::{CallingConvention, DebugInfo, Linkage, Name, StackSlot, Ty, Visibility};
use crate::{container::ContainerFile};
pub use sysop::{AsmSysOp, PosixDirentStyle, PosixFlagStyle};

pub type AsmBlockId = u32;
pub type AsmInstrId = u32;
pub type AsmVirtualRegId = u32;
pub type AsmType = Ty;

#[derive(Debug, Clone, PartialEq)]
pub struct AsmProgram {
    pub target: AsmTarget,
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

#[derive(Debug, Clone, PartialEq)]
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
    pub section: Option<String>,
    pub linkage: Linkage,
    pub visibility: Visibility,
    pub alignment: Option<u32>,
    pub is_constant: bool,
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
    pub predecessors: Vec<AsmBlockId>,
    pub successors: Vec<AsmBlockId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmInstruction {
    pub id: AsmInstrId,
    pub kind: AsmInstructionKind,
    pub type_hint: Option<AsmType>,
    pub opcode: AsmOpcode,
    pub operands: Vec<AsmOperand>,
    pub implicit_uses: Vec<AsmRegister>,
    pub implicit_defs: Vec<AsmRegister>,
    pub encoding: Option<Vec<u8>>,
    pub debug_info: Option<DebugInfo>,
    pub annotations: Vec<AsmAnnotation>,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OperandAccess {
    Read,
    Write,
    ReadWrite,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmRegister {
    Physical(AsmPhysicalRegister),
    Virtual {
        id: AsmVirtualRegId,
        bank: AsmRegisterBank,
        size_bits: u16,
    },
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
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsmSyscallConvention {
    LinuxX86_64,
    LinuxAarch64,
    DarwinX86_64,
    DarwinAarch64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmInstructionKind {
    Add(AsmValue, AsmValue),
    Sub(AsmValue, AsmValue),
    Mul(AsmValue, AsmValue),
    Div(AsmValue, AsmValue),
    Rem(AsmValue, AsmValue),
    And(AsmValue, AsmValue),
    Or(AsmValue, AsmValue),
    Xor(AsmValue, AsmValue),
    Shl(AsmValue, AsmValue),
    Shr(AsmValue, AsmValue),
    Not(AsmValue),
    Eq(AsmValue, AsmValue),
    Ne(AsmValue, AsmValue),
    Lt(AsmValue, AsmValue),
    Le(AsmValue, AsmValue),
    Gt(AsmValue, AsmValue),
    Ge(AsmValue, AsmValue),
    Ult(AsmValue, AsmValue),
    Ule(AsmValue, AsmValue),
    Ugt(AsmValue, AsmValue),
    Uge(AsmValue, AsmValue),
    Load {
        address: AsmValue,
        alignment: Option<u32>,
        volatile: bool,
    },
    Store {
        value: AsmValue,
        address: AsmValue,
        alignment: Option<u32>,
        volatile: bool,
    },
    Alloca {
        size: AsmValue,
        alignment: u32,
    },
    GetElementPtr {
        ptr: AsmValue,
        indices: Vec<AsmValue>,
        inbounds: bool,
    },
    Bitcast(AsmValue, AsmType),
    PtrToInt(AsmValue),
    IntToPtr(AsmValue),
    Trunc(AsmValue, AsmType),
    ZExt(AsmValue, AsmType),
    SExt(AsmValue, AsmType),
    FPExt(AsmValue, AsmType),
    FPTrunc(AsmValue, AsmType),
    FPToUI(AsmValue, AsmType),
    FPToSI(AsmValue, AsmType),
    UIToFP(AsmValue, AsmType),
    SIToFP(AsmValue, AsmType),
    ExtractValue {
        aggregate: AsmValue,
        indices: Vec<u32>,
    },
    InsertValue {
        aggregate: AsmValue,
        element: AsmValue,
        indices: Vec<u32>,
    },
    Call {
        function: AsmValue,
        args: Vec<AsmValue>,
        calling_convention: CallingConvention,
        tail_call: bool,
    },
    IntrinsicCall {
        kind: AsmIntrinsicKind,
        format: String,
        args: Vec<AsmValue>,
    },
    SextOrTrunc(AsmValue, AsmType),
    Phi {
        incoming: Vec<(AsmValue, AsmBlockId)>,
    },
    Select {
        condition: AsmValue,
        if_true: AsmValue,
        if_false: AsmValue,
    },
    InlineAsm {
        asm_string: String,
        constraints: String,
        inputs: Vec<AsmValue>,
        output_type: AsmType,
        side_effects: bool,
        align_stack: bool,
    },
    LandingPad {
        result_type: AsmType,
        personality: Option<AsmValue>,
        cleanup: bool,
        clauses: Vec<AsmLandingPadClause>,
    },
    Unreachable,
    Freeze(AsmValue),
    Syscall {
        convention: AsmSyscallConvention,
        number: AsmValue,
        args: Vec<AsmValue>,
    },
    SysOp(AsmSysOp),
    /// Replicates a scalar value into all lanes of a vector.
    ///
    /// `lane_bits * lanes` must match the destination vector size.
    Splat {
        value: AsmValue,
        lane_bits: u16,
        lanes: u16,
    },
    /// Constructs a vector value from explicit lane values.
    ///
    /// Lane types are derived from the destination instruction `type_hint`.
    BuildVector {
        elements: Vec<AsmValue>,
    },
    /// Extract a lane (0-based) from a vector value.
    ExtractLane {
        vector: AsmValue,
        lane: u16,
    },
    /// Produces a new vector with one lane overwritten.
    InsertLane {
        vector: AsmValue,
        lane: u16,
        value: AsmValue,
    },
    /// Interleaves low lanes from two vectors.
    ///
    /// For example with `lane_bits=16`, the result is the equivalent of:
    /// `zip1 vD.8h, vLhs.8h, vRhs.8h` on AArch64 or `punpcklwd` on x86_64.
    ZipLow {
        lhs: AsmValue,
        rhs: AsmValue,
        lane_bits: u16,
    },
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
    Return(Option<AsmValue>),
    Br(AsmBlockId),
    CondBr {
        condition: AsmValue,
        if_true: AsmBlockId,
        if_false: AsmBlockId,
    },
    Switch {
        value: AsmValue,
        default: AsmBlockId,
        cases: Vec<(u64, AsmBlockId)>,
    },
    IndirectBr {
        address: AsmValue,
        destinations: Vec<AsmBlockId>,
    },
    Invoke {
        function: AsmValue,
        args: Vec<AsmValue>,
        normal_dest: AsmBlockId,
        unwind_dest: AsmBlockId,
        calling_convention: CallingConvention,
    },
    Resume(AsmValue),
    Unreachable,
    CleanupRet {
        cleanup_pad: AsmValue,
        unwind_dest: Option<AsmBlockId>,
    },
    CatchRet {
        catch_pad: AsmValue,
        successor: AsmBlockId,
    },
    CatchSwitch {
        parent_pad: Option<AsmValue>,
        handlers: Vec<AsmBlockId>,
        unwind_dest: Option<AsmBlockId>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmValue {
    Register(u32),
    PhysicalRegister(AsmPhysicalRegister),
    Address(Box<AsmAddressValue>),
    Condition(AsmConditionCode),
    Comparison(Box<AsmComparisonValue>),
    Flags(u32),
    Constant(AsmConstant),
    Global(String, AsmType),
    Function(String),
    Local(u32),
    StackSlot(u32),
    Undef(AsmType),
    Null(AsmType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AsmComparisonValue {
    pub lhs: AsmValue,
    pub rhs: AsmValue,
    pub condition: AsmConditionCode,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct AsmAddressValue {
    pub base: Option<Box<AsmValue>>,
    pub index: Option<Box<AsmValue>>,
    pub scale: u8,
    pub displacement: i64,
    pub segment: Option<Box<AsmValue>>,
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

#[derive(Debug, Clone, PartialEq)]
pub enum AsmLandingPadClause {
    Catch(AsmValue),
    Filter(Vec<AsmValue>),
}

impl AsmProgram {
    pub fn new(target: AsmTarget) -> Self {
        Self {
            target,
            container: None,
            sections: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            type_definitions: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn asmir_program_construction_is_stable() {
        let mut program = AsmProgram::new(AsmTarget {
            architecture: AsmArchitecture::X86_64,
            object_format: AsmObjectFormat::Elf,
            endianness: AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: Some(CallingConvention::X86_64SysV),
        });
        program.sections.push(AsmSection {
            name: ".text".to_string(),
            kind: AsmSectionKind::Text,
            flags: vec![AsmSectionFlag::Allocate, AsmSectionFlag::Execute],
            alignment: Some(16),
        });
        program.functions.push(AsmFunction {
            name: Name::new("main"),
            signature: AsmFunctionSignature {
                params: Vec::new(),
                return_type: Ty::I32,
                is_variadic: false,
            },
            basic_blocks: vec![AsmBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![AsmInstruction {
                    id: 0,
                    kind: AsmInstructionKind::Freeze(AsmValue::Constant(AsmConstant::Int(
                        0,
                        Ty::I32,
                    ))),
                    type_hint: Some(Ty::I32),
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Freeze),
                    operands: vec![
                        AsmOperand::Register {
                            reg: AsmRegister::Physical(AsmPhysicalRegister {
                                name: "rax".to_string(),
                                bank: AsmRegisterBank::General,
                                size_bits: 64,
                            }),
                            access: OperandAccess::Write,
                        },
                        AsmOperand::Immediate(0),
                    ],
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: vec![AsmAnnotation {
                        key: "selected_from".to_string(),
                        value: "lir.return".to_string(),
                    }],
                }],
                terminator: AsmTerminator::Return(Some(AsmValue::Constant(AsmConstant::Int(
                    0,
                    Ty::I32,
                )))),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
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
        });

        assert_eq!(program.functions.len(), 1);
        assert_eq!(
            program.functions[0].basic_blocks[0].instructions[0].opcode,
            AsmOpcode::Generic(AsmGenericOpcode::Freeze)
        );
    }
}
