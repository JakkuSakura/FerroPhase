// use std::collections::HashMap; // Temporarily disabled - unused

pub type Ty = crate::types::Ty;
pub type LirId = u32;
pub type RegisterId = u32;
pub type BasicBlockId = u32;
pub type LabelId = u32;

#[derive(Debug, Clone, PartialEq)]
pub struct LirProgram {
    pub functions: Vec<LirFunction>,
    pub globals: Vec<LirGlobal>,
    pub type_definitions: Vec<LirTypeDefinition>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirFunction {
    pub name: String,
    pub signature: LirFunctionSignature,
    pub basic_blocks: Vec<LirBasicBlock>,
    pub locals: Vec<LirLocal>,
    pub stack_slots: Vec<StackSlot>,
    pub calling_convention: CallingConvention,
    pub linkage: Linkage,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirFunctionSignature {
    pub params: Vec<LirType>,
    pub return_type: LirType,
    pub is_variadic: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirBasicBlock {
    pub id: BasicBlockId,
    pub label: Option<String>,
    pub instructions: Vec<LirInstruction>,
    pub terminator: LirTerminator,
    pub predecessors: Vec<BasicBlockId>,
    pub successors: Vec<BasicBlockId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirInstruction {
    pub id: LirId,
    pub kind: LirInstructionKind,
    pub type_hint: Option<LirType>,
    pub debug_info: Option<DebugInfo>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirInstructionKind {
    // Arithmetic operations
    Add(LirValue, LirValue),
    Sub(LirValue, LirValue),
    Mul(LirValue, LirValue),
    Div(LirValue, LirValue),
    Rem(LirValue, LirValue),

    // Bitwise operations
    And(LirValue, LirValue),
    Or(LirValue, LirValue),
    Xor(LirValue, LirValue),
    Shl(LirValue, LirValue),
    Shr(LirValue, LirValue),
    Not(LirValue),

    // Comparison operations
    Eq(LirValue, LirValue),
    Ne(LirValue, LirValue),
    Lt(LirValue, LirValue),
    Le(LirValue, LirValue),
    Gt(LirValue, LirValue),
    Ge(LirValue, LirValue),

    // Memory operations
    Load {
        address: LirValue,
        alignment: Option<u32>,
        volatile: bool,
    },
    Store {
        value: LirValue,
        address: LirValue,
        alignment: Option<u32>,
        volatile: bool,
    },
    Alloca {
        size: LirValue,
        alignment: u32,
    },

    // Pointer operations
    GetElementPtr {
        ptr: LirValue,
        indices: Vec<LirValue>,
        inbounds: bool,
    },
    PtrToInt(LirValue),
    IntToPtr(LirValue),

    // Type conversion operations
    Trunc(LirValue, LirType),
    ZExt(LirValue, LirType),
    SExt(LirValue, LirType),
    FPTrunc(LirValue, LirType),
    FPExt(LirValue, LirType),
    FPToUI(LirValue, LirType),
    FPToSI(LirValue, LirType),
    UIToFP(LirValue, LirType),
    SIToFP(LirValue, LirType),
    Bitcast(LirValue, LirType),

    // Aggregate operations
    ExtractValue {
        aggregate: LirValue,
        indices: Vec<u32>,
    },
    InsertValue {
        aggregate: LirValue,
        element: LirValue,
        indices: Vec<u32>,
    },

    // Function operations
    Call {
        function: LirValue,
        args: Vec<LirValue>,
        calling_convention: CallingConvention,
        tail_call: bool,
    },

    // Helper to materialize integer-to-integer casts for runtime lowering
    SextOrTrunc(LirValue, LirType),

    // Control flow helpers
    Phi {
        incoming: Vec<(LirValue, BasicBlockId)>,
    },
    Select {
        condition: LirValue,
        if_true: LirValue,
        if_false: LirValue,
    },

    // Inline assembly
    InlineAsm {
        asm_string: String,
        constraints: String,
        inputs: Vec<LirValue>,
        output_type: LirType,
        side_effects: bool,
        align_stack: bool,
    },

    // Landing pad for exception handling
    LandingPad {
        result_type: LirType,
        personality: Option<LirValue>,
        cleanup: bool,
        clauses: Vec<LandingPadClause>,
    },

    // Misc
    Unreachable,
    Freeze(LirValue),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirTerminator {
    Return(Option<LirValue>),
    Br(BasicBlockId),
    CondBr {
        condition: LirValue,
        if_true: BasicBlockId,
        if_false: BasicBlockId,
    },
    Switch {
        value: LirValue,
        default: BasicBlockId,
        cases: Vec<(u64, BasicBlockId)>,
    },
    IndirectBr {
        address: LirValue,
        destinations: Vec<BasicBlockId>,
    },
    Invoke {
        function: LirValue,
        args: Vec<LirValue>,
        normal_dest: BasicBlockId,
        unwind_dest: BasicBlockId,
        calling_convention: CallingConvention,
    },
    Resume(LirValue),
    Unreachable,
    CleanupRet {
        cleanup_pad: LirValue,
        unwind_dest: Option<BasicBlockId>,
    },
    CatchRet {
        catch_pad: LirValue,
        successor: BasicBlockId,
    },
    CatchSwitch {
        parent_pad: Option<LirValue>,
        handlers: Vec<BasicBlockId>,
        unwind_dest: Option<BasicBlockId>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirValue {
    // Registers/SSA values
    Register(RegisterId),

    // Constants
    Constant(LirConstant),

    // Global references
    Global(String, Ty),

    // Function references
    Function(String),

    // Local variable references
    Local(u32),

    // Stack slot references
    StackSlot(u32),

    // Undefined value
    Undef(LirType),

    // Null pointer
    Null(LirType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirConstant {
    Int(i64, LirType),
    UInt(u64, LirType),
    Float(f64, LirType),
    Bool(bool),
    String(String),
    Array(Vec<LirConstant>, LirType),
    Struct(Vec<LirConstant>, LirType),
    Null(LirType),
    Undef(LirType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirType {
    // Integer types
    I1,
    I8,
    I16,
    I32,
    I64,
    I128,

    // Floating-point types
    F32,
    F64,

    // Pointer type
    Ptr(Box<LirType>),

    // Array type
    Array(Box<LirType>, u64),

    // Structure type
    Struct {
        fields: Vec<LirType>,
        packed: bool,
        name: Option<String>,
    },

    // Function type
    Function {
        return_type: Box<LirType>,
        param_types: Vec<LirType>,
        is_variadic: bool,
    },

    // Vector type
    Vector(Box<LirType>, u32),

    // Void type
    Void,

    // Label type (for basic block addresses)
    Label,

    // Token type (for exception handling)
    Token,

    // Metadata type
    Metadata,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirGlobal {
    pub name: String,
    pub ty: LirType,
    pub initializer: Option<LirConstant>,
    pub linkage: Linkage,
    pub visibility: Visibility,
    pub is_constant: bool,
    pub alignment: Option<u32>,
    pub section: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirTypeDefinition {
    pub name: String,
    pub ty: LirType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirLocal {
    pub id: u32,
    pub ty: LirType,
    pub name: Option<String>,
    pub is_argument: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StackSlot {
    pub id: u32,
    pub size: u32,
    pub alignment: u32,
    pub name: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallingConvention {
    C,
    Fast,
    Cold,
    WebKitJS,
    AnyReg,
    PreserveMost,
    PreserveAll,
    Swift,
    CxxFastTLS,
    X86StdCall,
    X86FastCall,
    X86ThisCall,
    X86VectorCall,
    Win64,
    X86_64SysV,
    AAPCS,
    AAPCSVfp,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Linkage {
    External,
    AvailableExternally,
    LinkOnceAny,
    LinkOnceOdr,
    WeakAny,
    WeakOdr,
    Appending,
    Internal,
    Private,
    ExternalWeak,
    Common,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Default,
    Hidden,
    Protected,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LandingPadClause {
    Catch(LirValue),
    Filter(Vec<LirValue>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DebugInfo {
    pub file: String,
    pub line: u32,
    pub column: u32,
    pub scope: Option<String>,
}

// Implementation helpers
impl LirProgram {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            globals: Vec::new(),
            type_definitions: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: LirFunction) {
        self.functions.push(function);
    }

    pub fn add_global(&mut self, global: LirGlobal) {
        self.globals.push(global);
    }
}

impl LirFunction {
    pub fn new(
        name: String,
        signature: LirFunctionSignature,
        calling_convention: CallingConvention,
        linkage: Linkage,
    ) -> Self {
        Self {
            name,
            signature,
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention,
            linkage,
        }
    }

    pub fn add_basic_block(&mut self, block: LirBasicBlock) {
        self.basic_blocks.push(block);
    }

    pub fn get_basic_block(&self, id: BasicBlockId) -> Option<&LirBasicBlock> {
        self.basic_blocks.iter().find(|bb| bb.id == id)
    }

    pub fn get_basic_block_mut(&mut self, id: BasicBlockId) -> Option<&mut LirBasicBlock> {
        self.basic_blocks.iter_mut().find(|bb| bb.id == id)
    }
}

impl LirBasicBlock {
    pub fn new(id: BasicBlockId, label: Option<String>) -> Self {
        Self {
            id,
            label,
            instructions: Vec::new(),
            terminator: LirTerminator::Unreachable,
            predecessors: Vec::new(),
            successors: Vec::new(),
        }
    }

    pub fn add_instruction(&mut self, instruction: LirInstruction) {
        self.instructions.push(instruction);
    }

    pub fn set_terminator(&mut self, terminator: LirTerminator) {
        self.terminator = terminator;
    }
}

impl LirInstruction {
    pub fn new(id: LirId, kind: LirInstructionKind) -> Self {
        Self {
            id,
            kind,
            type_hint: None,
            debug_info: None,
        }
    }

    pub fn with_type(mut self, ty: LirType) -> Self {
        self.type_hint = Some(ty);
        self
    }

    pub fn with_debug_info(mut self, debug_info: DebugInfo) -> Self {
        self.debug_info = Some(debug_info);
        self
    }
}

impl LirType {
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            LirType::I1 | LirType::I8 | LirType::I16 | LirType::I32 | LirType::I64 | LirType::I128
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, LirType::F32 | LirType::F64)
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, LirType::Ptr(_))
    }

    pub fn size_in_bits(&self) -> Option<u32> {
        match self {
            LirType::I1 => Some(1),
            LirType::I8 => Some(8),
            LirType::I16 => Some(16),
            LirType::I32 => Some(32),
            LirType::I64 => Some(64),
            LirType::I128 => Some(128),
            LirType::F32 => Some(32),
            LirType::F64 => Some(64),
            LirType::Ptr(_) => Some(64), // Assume 64-bit pointers
            LirType::Array(element_ty, count) => {
                element_ty.size_in_bits().map(|size| size * (*count as u32))
            }
            _ => None,
        }
    }
}

impl Default for CallingConvention {
    fn default() -> Self {
        CallingConvention::C
    }
}

impl Default for Linkage {
    fn default() -> Self {
        Linkage::External
    }
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Default
    }
}
