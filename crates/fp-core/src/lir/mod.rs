pub mod ident;
pub mod layout;
pub mod path;
pub mod pretty;
pub mod program;
pub mod ty;

use crate::query::{QueryExpr, QueryIrDocument, QueryIrStmt, QueryOrigin};
use crate::span::Span;
pub use ident::Name;
pub use path::LirPath;
pub use program::LirProgram;
pub use ty::Ty;
/// LIR packages share the compiler-wide package identity.
pub type PackageId = crate::package::PackageId;
pub type LirType = Ty;
pub type LirId = u32;
pub type RegisterId = u32;
pub type BasicBlockId = u32;
pub type LabelId = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeSymbol {
    Printf,
    Fprintf,
    Malloc,
    Free,
    Realloc,
    Sin,
    Sinf,
    Cos,
    Cosf,
    Tan,
    Tanf,
    Sqrt,
    Sqrtf,
    Pow,
    Powf,
    Strlen,
    Strcmp,
    Exit,
}

impl RuntimeSymbol {
    pub fn as_str(self) -> &'static str {
        match self {
            RuntimeSymbol::Printf => "printf",
            RuntimeSymbol::Fprintf => "fprintf",
            RuntimeSymbol::Malloc => "malloc",
            RuntimeSymbol::Free => "free",
            RuntimeSymbol::Realloc => "realloc",
            RuntimeSymbol::Sin => "sin",
            RuntimeSymbol::Sinf => "sinf",
            RuntimeSymbol::Cos => "cos",
            RuntimeSymbol::Cosf => "cosf",
            RuntimeSymbol::Tan => "tan",
            RuntimeSymbol::Tanf => "tanf",
            RuntimeSymbol::Sqrt => "sqrt",
            RuntimeSymbol::Sqrtf => "sqrtf",
            RuntimeSymbol::Pow => "pow",
            RuntimeSymbol::Powf => "powf",
            RuntimeSymbol::Strlen => "strlen",
            RuntimeSymbol::Strcmp => "strcmp",
            RuntimeSymbol::Exit => "exit",
        }
    }
}

/// Flat, unidentified LIR content — every function/global/type/query
/// produced for one package, with no module identity of its own. Also
/// `ItemKind::PrecompiledLir`'s payload: an item embedded directly in a
/// package's AST doesn't need its own identity beyond its position in
/// that AST. `LirPackage` (see its own doc comment) is just one of these
/// per package — there's no separate per-module/per-artifact identity
/// layer underneath it.
#[derive(Debug, Clone, PartialEq)]
pub struct LirBlob {
    pub data_layout: LirDataLayout,
    pub functions: Vec<LirFunction>,
    pub globals: Vec<LirGlobal>,
    pub type_definitions: Vec<LirTypeDefinition>,
    pub queries: Vec<LirQuery>,
}

/// `ItemKind::PrecompiledLir` (see `fp_core::ast::item`) needs `LirBlob`
/// to satisfy the same derive bounds every other `ItemKind` payload gets
/// via the `common_enum!` macro (`Hash`, `Serialize`, `Deserialize`) —
/// mirrors `AsmProgram`'s identical treatment for `ItemKind::PrecompiledAsm`
/// (`fp_core::asmir`): trivial/error stand-ins, not real implementations.
/// An already-compiled artifact is never meant to be hashed for
/// deduplication or serialized to disk as AST.
impl std::hash::Hash for LirBlob {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}

impl serde::Serialize for LirBlob {
    fn serialize<S: serde::Serializer>(&self, _serializer: S) -> Result<S::Ok, S::Error> {
        Err(serde::ser::Error::custom(
            "LirBlob (ItemKind::PrecompiledLir) does not support serialization",
        ))
    }
}

impl<'de> serde::Deserialize<'de> for LirBlob {
    fn deserialize<D: serde::Deserializer<'de>>(_deserializer: D) -> Result<Self, D::Error> {
        Err(serde::de::Error::custom(
            "LirBlob (ItemKind::PrecompiledLir) does not support deserialization",
        ))
    }
}

/// One compiled package's LIR content — pairs with `LirBlob` the same way
/// `hir::HirPackage`/`mir::MirPackage` pair with their own layer's `Program`
/// type. A plain `Vec<LirBlob>`, one entry per lowering pass
/// (`CompilerState::insert_lir_blob_for_package` just pushes, never merges
/// or resets) — a package re-lowered after a comptime value resolves
/// (`CompilerDriver::relower_cached_lir_units`) ends up with more than one
/// entry, and a lookup that cares about the latest one (`LirProgram::
/// find_function`/`find_global`/`find_function_by_def_id`) searches from
/// the end. `LirProgram::merged_blob_for_package` flattens every package's
/// own blobs (and every dependency's) into the one combined `LirBlob` a
/// `TargetBackend` actually needs.
#[derive(Debug, Clone, PartialEq)]
pub struct LirPackage {
    pub data_layout: LirDataLayout,
    pub blobs: Vec<LirBlob>,
}

impl LirPackage {
    pub fn new(data_layout: LirDataLayout) -> Self {
        Self {
            data_layout,
            blobs: Vec::new(),
        }
    }
}

/// One independently addressable LIR definition — used only for the one
/// renamed entrypoint function `CompilerState::insert_runtime_program`
/// stores (see its own doc comment), not as `LirPackage`'s storage (that's
/// just a `LirBlob` now).
#[derive(Debug, Clone, PartialEq)]
pub struct LirCodeUnit {
    pub package_id: PackageId,
    pub module_path: crate::ast::path::QualifiedPath,
    pub kind: LirCodeUnitKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirCodeUnitKind {
    Function(LirFunction),
    Global(LirGlobal),
    TypeDefinition(LirTypeDefinition),
    Query(LirQuery),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LirDataLayout {
    pub pointer_size_bits: u32,
    pub pointer_alignment: u32,
    pub integer_alignments: Vec<(u32, u32)>,
}

#[derive(Debug, thiserror::Error, Clone, PartialEq, Eq)]
pub enum LirDataLayoutError {
    #[error("pointer size must be non-zero and byte-addressable, got {0}")]
    InvalidPointerSize(u32),
    #[error("pointer alignment must be non-zero, got {0}")]
    InvalidPointerAlignment(u32),
    #[error("integer alignment for i{width} must be non-zero, got {alignment}")]
    InvalidIntegerAlignment { width: u32, alignment: u32 },
    #[error("duplicate integer alignment for i{0}")]
    DuplicateIntegerAlignment(u32),
    #[error("data layout has no alignment for i{0}")]
    MissingIntegerAlignment(u32),
    #[error("layout size overflow")]
    SizeOverflow,
    #[error("expected a struct type, got {0:?}")]
    ExpectedStruct(LirType),
    #[error("the error type has no data layout")]
    ErrorTypeHasNoLayout,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirQuery {
    pub query_id: LirId,
    pub origin: QueryOrigin,
    pub ir: QueryIrDocument,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirFunction {
    pub def_id: Option<crate::hir::DefId>,
    pub name: Name,
    pub signature: LirFunctionSignature,
    pub basic_blocks: Vec<LirBasicBlock>,
    pub locals: Vec<LirLocal>,
    pub stack_slots: Vec<StackSlot>,
    pub calling_convention: CallingConvention,
    pub linkage: Linkage,
    pub is_declaration: bool,
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
    pub label: Option<Name>,
    pub instructions: Vec<LirInstruction>,
    pub terminator: LirTerminator,
    pub predecessors: Vec<BasicBlockId>,
    pub successors: Vec<BasicBlockId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirInstruction {
    pub id: LirId,
    pub kind: LirInstructionKind,
    /// The SSA value defined by this instruction. Void instructions do not
    /// define a value.
    pub result: Option<LirRegister>,
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
    ExecQuery(LirQuery),

    // Comptime-only operations — only the interpreter handles these.
    // Codegen backends can add a single catch-all arm.
    ComptimeOp(ComptimeOp),

    // Backend runtime intrinsics
    IntrinsicCall {
        kind: LirIntrinsicKind,
        format: String,
        args: Vec<LirValue>,
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
pub enum LirIntrinsicKind {
    Print,
    Println,
    Format,
    TimeNow,
    /// Parses proc-macro source text into a `TokenStream`. Not yet
    /// implemented by any backend — real token-stream support requires an
    /// actual tokenizer, so every backend must fail loudly rather than
    /// silently substitute a placeholder value if this is ever genuinely
    /// invoked (real user programs that don't use proc-macro APIs never
    /// reach this; it only exists so `std::proc_macro`'s own wrapper
    /// functions, compiled unconditionally as part of `std`, have a real
    /// (if unimplemented) MIR/LIR shape instead of silently becoming unit).
    ProcMacroTokenStreamFromStr,
    /// Prints a `TokenStream` back to source text — see
    /// `ProcMacroTokenStreamFromStr`'s doc comment.
    ProcMacroTokenStreamToString,
}

/// Comptime-only operations that build struct metadata, or report a
/// diagnostic from inside a `const { .. }` block. Only the LIR interpreter
/// handles these; codegen backends skip them — by the time a real compiled
/// program's MIR/LIR exists, every `const` binding whose initializer
/// reaches one of these has already been fully evaluated during the
/// comptime probe (see `hir_to_mir/expr.rs`'s `lower_operand`, the only
/// place that ever constructs one), so no compiled binary needs to
/// re-execute it at runtime.
#[derive(Debug, Clone, PartialEq)]
pub enum ComptimeOp {
    TypeValue {
        value: crate::ast::Ty,
    },
    CreateStruct {
        name: LirValue,
    },
    AddField {
        struct_handle: LirValue,
        field_name: LirValue,
        field_type: LirValue,
    },
    CloneStruct {
        value: LirValue,
    },
    IntoType {
        value: LirValue,
    },
    /// `std::intrinsics::primitive_type(name)` — reflects a primitive
    /// type name (or a `&`-prefixed reference to one) as a runtime type
    /// value, the same shape `CreateStruct`'s result already has.
    PrimitiveType {
        name: LirValue,
    },
    /// `compile_warning!(message)` — reports `message` and evaluates to
    /// `()`, without aborting comptime evaluation.
    CompileWarning {
        message: LirValue,
    },
    /// `compile_error!(message)` — aborts comptime evaluation, surfacing
    /// `message` as a real compilation error.
    CompileError {
        message: LirValue,
    },
    /// `unionify(f)` — produces a closure (`Value::UnionifyClosure`)
    /// capturing `f`. Calling that closure with a reflected union type
    /// applies `f` to each member's literal string and rebuilds the union
    /// — an ordinary indirect call, handled in `handle_call`, not here. See
    /// `LangIntrinsic::Unionify`'s doc comment.
    Unionify {
        function: LirValue,
    },
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

/// A typed SSA definition. LLVM instructions that produce a result are Values;
/// this is the operand-level reference to such a definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LirRegister {
    pub id: RegisterId,
    pub ty: LirType,
}

/// A typed operand in LIR. Every operand owns one authoritative type; callers
/// do not infer it from its producer or from an instruction-side hint.
#[derive(Debug, Clone, PartialEq)]
pub struct LirValue {
    pub ty: LirType,
    pub kind: LirValueKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirValueKind {
    Register(RegisterId),
    Constant(LirConstantKind),
    Global(Name),
    Function(LirFunctionRef),
    Local(u32),
    StackSlot(u32),
}

/// The identity of a function value. The value's function-pointer type lives
/// on `LirValue`, exactly as the type of a global or function value does in
/// LLVM IR.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LirFunctionRef {
    Name(Name),
    Package { package_id: PackageId, name: Name },
    Definition(crate::hir::DefId),
}

/// A typed constant value. This is the Rust analogue of LLVM's `Constant`
/// base class: its type is stored once here, not repeated in each payload.
#[derive(Debug, Clone, PartialEq)]
pub struct LirConstant {
    pub ty: LirType,
    pub kind: LirConstantKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirConstantKind {
    Data(LirConstantData),
    Aggregate(LirConstantAggregate),
    GlobalAddress { global: Name },
    FunctionAddress(LirFunctionRef),
    Null,
    Undef,
    Poison,
    Expr(LirConstantExpr),
}

/// Operand-less constant data, corresponding to LLVM's `ConstantData`.
#[derive(Debug, Clone, PartialEq)]
pub enum LirConstantData {
    Integer(LirInteger),
    Float(LirFloat),
    Bytes(Vec<u8>),
}

/// An integer constant payload. Fixed-width language types retain their native
/// representation; arbitrary LLVM widths use the APInt-style word buffer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LirInteger {
    I1(bool),
    I8(u8),
    I16(u16),
    I32(u32),
    I64(u64),
    I128(u128),
    Arbitrary(LirApInt),
}

/// A width-carrying arbitrary integer bit pattern, modeled after LLVM's
/// `APInt`. It is used only where no native-width LIR integer applies.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LirApInt {
    pub bit_width: u32,
    pub words: Box<[u64]>,
}

#[derive(Debug, thiserror::Error, Clone, PartialEq, Eq)]
pub enum LirConstantError {
    #[error("integer payload {integer:?} is incompatible with {ty:?}")]
    IntegerTypeMismatch { ty: LirType, integer: LirInteger },
    #[error("floating payload {float:?} is incompatible with {ty:?}")]
    FloatTypeMismatch { ty: LirType, float: LirFloat },
}

impl LirApInt {
    pub fn from_words(bit_width: u32, words: Vec<u64>) -> Option<Self> {
        let word_count = usize::try_from(bit_width.div_ceil(64)).ok()?;
        if bit_width == 0 || words.len() != word_count {
            return None;
        }
        let used_bits_in_last_word = bit_width % 64;
        if used_bits_in_last_word != 0 {
            let valid_mask = u64::MAX >> (64 - used_bits_in_last_word);
            if words.last().is_some_and(|word| *word & !valid_mask != 0) {
                return None;
            }
        }
        Some(Self {
            bit_width,
            words: words.into_boxed_slice(),
        })
    }
}

impl LirInteger {
    pub fn is_zero(&self) -> bool {
        match self {
            LirInteger::I1(value) => !*value,
            LirInteger::I8(value) => *value == 0,
            LirInteger::I16(value) => *value == 0,
            LirInteger::I32(value) => *value == 0,
            LirInteger::I64(value) => *value == 0,
            LirInteger::I128(value) => *value == 0,
            LirInteger::Arbitrary(value) => value.words.iter().all(|word| *word == 0),
        }
    }

    pub fn matches_type(&self, ty: &LirType) -> bool {
        match (self, ty) {
            (LirInteger::I1(_), LirType::I1)
            | (LirInteger::I8(_), LirType::I8)
            | (LirInteger::I16(_), LirType::I16)
            | (LirInteger::I32(_), LirType::I32)
            | (LirInteger::I64(_), LirType::I64)
            | (LirInteger::I128(_), LirType::I128) => true,
            (LirInteger::Arbitrary(value), LirType::Integer(width)) => value.bit_width == *width,
            _ => false,
        }
    }
}

impl std::fmt::Display for LirInteger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LirInteger::I1(value) => write!(f, "{}", u8::from(*value)),
            LirInteger::I8(value) => write!(f, "{value}"),
            LirInteger::I16(value) => write!(f, "{value}"),
            LirInteger::I32(value) => write!(f, "{value}"),
            LirInteger::I64(value) => write!(f, "{value}"),
            LirInteger::I128(value) => write!(f, "{value}"),
            LirInteger::Arbitrary(value) => {
                write!(f, "0x")?;
                for word in value.words.iter().rev() {
                    write!(f, "{word:016x}")?;
                }
                Ok(())
            }
        }
    }
}

/// The exact IEEE payload of an LIR floating constant. Decimal source values
/// are rounded during lowering; LIR itself never stores an untyped `f64`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LirFloat {
    F32(u32),
    F64(u64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LirConstantAggregate {
    Array(Vec<LirConstant>),
    Struct(Vec<LirConstant>),
    Vector(Vec<LirConstant>),
}

/// A constant expression is an immutable, typed expression whose operands are
/// themselves constants. Ordinary computations must use instructions.
#[derive(Debug, Clone, PartialEq)]
pub enum LirConstantExpr {
    GetElementPtr {
        base: Box<LirConstant>,
        indices: Vec<LirConstant>,
        inbounds: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LirRelocationKind {
    Abs64,
    PcRel32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LirRelocationTarget {
    Global(Name),
    Function(Name),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LirGlobalRelocation {
    pub offset: u64,
    pub kind: LirRelocationKind,
    pub target: LirRelocationTarget,
    pub addend: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirGlobal {
    pub name: Name,
    pub ty: LirType,
    pub initializer: Option<LirConstant>,
    pub relocations: Vec<LirGlobalRelocation>,
    pub linkage: Linkage,
    pub visibility: Visibility,
    pub is_constant: bool,
    pub alignment: Option<u32>,
    pub section: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LirTypeDefinition {
    pub name: Name,
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
    /// Internal calling convention used by the native binary lifters.
    ///
    /// Lifted x86_64 machine code models the architectural register file as a
    /// shared memory blob. Calls between lifted functions pass a single hidden
    /// pointer to that register file (instead of using the platform ABI).
    FpLiftedX86_64RegFile,
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
impl LirBlob {
    pub fn new(data_layout: LirDataLayout) -> Self {
        Self {
            data_layout,
            functions: Vec::new(),
            globals: Vec::new(),
            type_definitions: Vec::new(),
            queries: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: LirFunction) {
        self.functions.push(function);
    }

    pub fn add_global(&mut self, global: LirGlobal) {
        self.globals.push(global);
    }

    /// Return the only embedded query item in this LIR program.
    pub fn single_query(&self) -> std::result::Result<&LirQuery, crate::Error> {
        let mut queries = self.queries.iter();
        let query = queries
            .next()
            .ok_or_else(|| crate::Error::from("LIR program contains no embedded query item"))?;
        if queries.next().is_some() {
            return Err(crate::Error::from(
                "LIR program contains multiple embedded query items",
            ));
        }
        Ok(query)
    }
}

impl LirFunction {
    pub fn new(
        name: Name,
        signature: LirFunctionSignature,
        calling_convention: CallingConvention,
        linkage: Linkage,
    ) -> Self {
        Self {
            def_id: None,
            name,
            signature,
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention,
            linkage,
            is_declaration: false,
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
    pub fn new(id: BasicBlockId, label: Option<Name>) -> Self {
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
            result: None,
            debug_info: None,
        }
    }

    pub fn with_result(mut self, ty: LirType) -> Self {
        self.result = Some(LirRegister { id: self.id, ty });
        self
    }

    pub fn with_debug_info(mut self, debug_info: DebugInfo) -> Self {
        self.debug_info = Some(debug_info);
        self
    }
}

impl LirValue {
    pub fn register(id: RegisterId, ty: LirType) -> Self {
        Self {
            ty,
            kind: LirValueKind::Register(id),
        }
    }

    pub fn constant(constant: LirConstant) -> Self {
        Self {
            ty: constant.ty.clone(),
            kind: LirValueKind::Constant(constant.kind),
        }
    }

    pub fn global(name: Name, ty: LirType) -> Self {
        Self {
            ty,
            kind: LirValueKind::Global(name),
        }
    }

    pub fn function(function: LirFunctionRef, ty: LirType) -> Self {
        Self {
            ty,
            kind: LirValueKind::Function(function),
        }
    }

    pub fn local(id: u32, ty: LirType) -> Self {
        Self {
            ty,
            kind: LirValueKind::Local(id),
        }
    }

    pub fn stack_slot(id: u32, ty: LirType) -> Self {
        Self {
            ty,
            kind: LirValueKind::StackSlot(id),
        }
    }
}

impl LirConstant {
    pub fn integer(ty: LirType, value: LirInteger) -> Result<Self, LirConstantError> {
        if !value.matches_type(&ty) {
            return Err(LirConstantError::IntegerTypeMismatch { ty, integer: value });
        }
        Ok(Self {
            ty,
            kind: LirConstantKind::Data(LirConstantData::Integer(value)),
        })
    }

    pub fn float(ty: LirType, value: LirFloat) -> Result<Self, LirConstantError> {
        let matches_type = matches!(
            (&ty, value),
            (LirType::F32, LirFloat::F32(_)) | (LirType::F64, LirFloat::F64(_))
        );
        if !matches_type {
            return Err(LirConstantError::FloatTypeMismatch { ty, float: value });
        }
        Ok(Self {
            ty,
            kind: LirConstantKind::Data(LirConstantData::Float(value)),
        })
    }

    pub fn bytes(ty: LirType, value: Vec<u8>) -> Self {
        Self {
            ty,
            kind: LirConstantKind::Data(LirConstantData::Bytes(value)),
        }
    }

    pub fn aggregate(ty: LirType, value: LirConstantAggregate) -> Self {
        Self {
            ty,
            kind: LirConstantKind::Aggregate(value),
        }
    }

    pub fn global_address(ty: LirType, global: Name) -> Self {
        Self {
            ty,
            kind: LirConstantKind::GlobalAddress { global },
        }
    }

    pub fn function_address(ty: LirType, function: LirFunctionRef) -> Self {
        Self {
            ty,
            kind: LirConstantKind::FunctionAddress(function),
        }
    }

    pub fn get_element_ptr(
        ty: LirType,
        base: LirConstant,
        indices: Vec<LirConstant>,
        inbounds: bool,
    ) -> Self {
        Self {
            ty,
            kind: LirConstantKind::Expr(LirConstantExpr::GetElementPtr {
                base: Box::new(base),
                indices,
                inbounds,
            }),
        }
    }

    pub fn null(ty: LirType) -> Self {
        Self {
            ty,
            kind: LirConstantKind::Null,
        }
    }

    pub fn undef(ty: LirType) -> Self {
        Self {
            ty,
            kind: LirConstantKind::Undef,
        }
    }

    pub fn poison(ty: LirType) -> Self {
        Self {
            ty,
            kind: LirConstantKind::Poison,
        }
    }
}

impl LirType {
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            LirType::Integer(_)
                | LirType::I1
                | LirType::I8
                | LirType::I16
                | LirType::I32
                | LirType::I64
                | LirType::I128
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
            LirType::Integer(width) => Some(*width),
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
                let count = u32::try_from(*count).ok()?;
                element_ty.size_in_bits()?.checked_mul(count)
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

impl LirDataLayout {
    pub fn x86_64() -> Self {
        Self::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .expect("built-in x86_64 data layout is valid")
    }

    pub fn aarch64() -> Self {
        Self::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .expect("built-in aarch64 data layout is valid")
    }

    pub fn new(
        pointer_size_bits: u32,
        pointer_alignment: u32,
        integer_alignments: Vec<(u32, u32)>,
    ) -> Result<Self, LirDataLayoutError> {
        if pointer_size_bits == 0 || !pointer_size_bits.is_multiple_of(8) {
            return Err(LirDataLayoutError::InvalidPointerSize(pointer_size_bits));
        }
        if pointer_alignment == 0 {
            return Err(LirDataLayoutError::InvalidPointerAlignment(
                pointer_alignment,
            ));
        }
        for (index, (width, alignment)) in integer_alignments.iter().enumerate() {
            if *width == 0 || *alignment == 0 {
                return Err(LirDataLayoutError::InvalidIntegerAlignment {
                    width: *width,
                    alignment: *alignment,
                });
            }
            if integer_alignments[..index]
                .iter()
                .any(|(previous_width, _)| previous_width == width)
            {
                return Err(LirDataLayoutError::DuplicateIntegerAlignment(*width));
            }
        }
        Ok(Self {
            pointer_size_bits,
            pointer_alignment,
            integer_alignments,
        })
    }

    pub fn integer_alignment(&self, width: u32) -> Result<u32, LirDataLayoutError> {
        self.integer_alignments
            .iter()
            .find_map(|(layout_width, alignment)| (*layout_width == width).then_some(*alignment))
            .ok_or(LirDataLayoutError::MissingIntegerAlignment(width))
    }
}

impl LirBlob {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl LirFunction {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl LirBasicBlock {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl LirInstruction {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl LirInstructionKind {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl LirTerminator {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

#[cfg(test)]
mod tests {
    use super::{LirApInt, LirBlob, LirConstant, LirDataLayout, LirInteger, LirType};

    fn data_layout() -> LirDataLayout {
        LirDataLayout::new(
            64,
            8,
            vec![(1, 1), (8, 1), (16, 2), (32, 4), (64, 8), (128, 16)],
        )
        .expect("valid data layout")
    }

    #[test]
    fn integer_constant_requires_matching_native_type() {
        assert!(LirConstant::integer(LirType::I32, LirInteger::I32(42)).is_ok());
        assert!(LirConstant::integer(LirType::I64, LirInteger::I32(42)).is_err());
    }

    #[test]
    fn arbitrary_integer_constant_requires_matching_width() {
        let value = LirApInt::from_words(256, vec![0; 4]).expect("valid APInt words");
        assert!(LirConstant::integer(LirType::Integer(256), LirInteger::Arbitrary(value)).is_ok());

        let value = LirApInt::from_words(256, vec![0; 4]).expect("valid APInt words");
        assert!(LirConstant::integer(LirType::Integer(257), LirInteger::Arbitrary(value)).is_err());
    }

    #[test]
    fn arbitrary_integer_rejects_unused_high_bits() {
        assert!(LirApInt::from_words(65, vec![0, 2]).is_none());
        assert!(LirApInt::from_words(65, vec![0, 1]).is_some());
    }

    #[test]
    fn built_in_64_bit_layouts_define_all_native_integer_alignments() {
        for layout in [LirDataLayout::x86_64(), LirDataLayout::aarch64()] {
            assert_eq!(layout.pointer_size_bits, 64);
            assert_eq!(layout.pointer_alignment, 8);
            assert_eq!(layout.integer_alignment(1), Ok(1));
            assert_eq!(layout.integer_alignment(8), Ok(1));
            assert_eq!(layout.integer_alignment(16), Ok(2));
            assert_eq!(layout.integer_alignment(32), Ok(4));
            assert_eq!(layout.integer_alignment(64), Ok(8));
            assert_eq!(layout.integer_alignment(128), Ok(16));
        }
    }
}

impl LirValue {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl LirConstant {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl LirGlobal {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl LirQuery {
    /// Return the only lowered query statement carried by this LIR query item.
    pub fn single_statement(&self) -> std::result::Result<&QueryIrStmt, crate::Error> {
        self.ir.single_statement()
    }

    /// Return the only lowered SELECT carried by this LIR query item.
    pub fn single_query(&self) -> std::result::Result<&QueryExpr, crate::Error> {
        self.ir.single_query()
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl LirConstant {}

impl LirTypeDefinition {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl LirLocal {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl StackSlot {
    pub fn span(&self) -> Span {
        Span::null()
    }
}

impl DebugInfo {
    pub fn span(&self) -> Span {
        Span::null()
    }
}
