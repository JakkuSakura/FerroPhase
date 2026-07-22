use std::collections::HashMap;
use std::fmt;

use fp_core::lir::RegisterId;

// ---------------------------------------------------------------------------
// Error
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum VmError {
    StackOverflow,
    InvalidAddress(u64),
    UnalignedAccess(u64, u32),
    DivisionByZero,
    UndefinedRegister(RegisterId),
    TypeMismatch { expected: String, found: String },
    Runtime(String),
}

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VmError::StackOverflow => write!(f, "stack overflow"),
            VmError::InvalidAddress(a) => write!(f, "invalid address 0x{a:x}"),
            VmError::UnalignedAccess(a, align) => {
                write!(f, "unaligned access at 0x{a:x} (required {align})")
            }
            VmError::DivisionByZero => write!(f, "division by zero"),
            VmError::UndefinedRegister(id) => write!(f, "undefined register r{id}"),
            VmError::TypeMismatch { expected, found } => {
                write!(f, "type mismatch: expected {expected}, got {found}")
            }
            VmError::Runtime(msg) => write!(f, "runtime error: {msg}"),
        }
    }
}

// ---------------------------------------------------------------------------
// Unified virtual memory
// ---------------------------------------------------------------------------

const PAGE_SIZE: u64 = 4096;
const STACK_SIZE: u64 = 8 * 1024 * 1024; // 8 MiB
const HEAP_DEFAULT: u64 = 64 * 1024 * 1024; // 64 MiB

/// Unified byte-addressable virtual memory.
///
/// Layout:
///   0x0            → program base (code / read-only)
///   0x1_0000       → heap start (grows up)
///   heap_end       → free
///   stack_top - 8M → stack (grows down)
///   stack_top      → initial sp
pub struct VirtMem {
    bytes: Vec<u8>,
    /// Next free byte in the heap region (bump allocator).
    heap_ptr: u64,
    /// Top of the stack region (sp starts here, grows down).
    stack_top: u64,
    /// Lowest used stack address (for bounds checking).
    stack_low: u64,
}

impl VirtMem {
    pub fn new(heap_size: u64) -> Self {
        let total = heap_size + STACK_SIZE + 0x10000;
        let bytes = vec![0u8; total as usize];
        let stack_top = total - 1;
        let stack_low = stack_top - STACK_SIZE;
        Self {
            bytes,
            heap_ptr: 0x10000,
            stack_top,
            stack_low,
        }
    }

    fn bounds(&self, addr: u64, size: u64) -> Result<(), VmError> {
        if addr + size > self.bytes.len() as u64 {
            return Err(VmError::InvalidAddress(addr));
        }
        Ok(())
    }

    // -- allocation --

    /// Bump-allocate `size` bytes with `alignment`. Returns a heap address.
    pub fn heap_alloc(&mut self, size: u64, alignment: u32) -> Result<u64, VmError> {
        let aligned = (self.heap_ptr + alignment as u64 - 1) & !(alignment as u64 - 1);
        let end = aligned + size;
        if end > self.stack_low {
            return Err(VmError::StackOverflow);
        }
        self.heap_ptr = end;
        Ok(aligned)
    }

    /// Allocate on the stack (grows down). Returns the new sp value.
    pub fn stack_alloc(&mut self, sp: u64, size: u64, alignment: u32) -> Result<u64, VmError> {
        let aligned_size = (size + alignment as u64 - 1) & !(alignment as u64 - 1);
        let new_sp = sp.checked_sub(aligned_size).ok_or(VmError::StackOverflow)?;
        if new_sp < self.stack_low {
            return Err(VmError::StackOverflow);
        }
        self.bounds(new_sp, aligned_size)?;
        Ok(new_sp)
    }

    // -- typed load / store --

    pub fn store_u64(&mut self, addr: u64, val: u64) -> Result<(), VmError> {
        self.bounds(addr, 8)?;
        let a = addr as usize;
        self.bytes[a..a + 8].copy_from_slice(&val.to_le_bytes());
        Ok(())
    }

    pub fn load_u64(&self, addr: u64) -> Result<u64, VmError> {
        self.bounds(addr, 8)?;
        let a = addr as usize;
        Ok(u64::from_le_bytes(self.bytes[a..a + 8].try_into().unwrap()))
    }

    pub fn store_u32(&mut self, addr: u64, val: u32) -> Result<(), VmError> {
        self.bounds(addr, 4)?;
        let a = addr as usize;
        self.bytes[a..a + 4].copy_from_slice(&val.to_le_bytes());
        Ok(())
    }

    pub fn load_u32(&self, addr: u64) -> Result<u32, VmError> {
        self.bounds(addr, 4)?;
        let a = addr as usize;
        Ok(u32::from_le_bytes(self.bytes[a..a + 4].try_into().unwrap()))
    }

    pub fn store_u16(&mut self, addr: u64, val: u16) -> Result<(), VmError> {
        self.bounds(addr, 2)?;
        let a = addr as usize;
        self.bytes[a..a + 2].copy_from_slice(&val.to_le_bytes());
        Ok(())
    }

    pub fn load_u16(&self, addr: u64) -> Result<u16, VmError> {
        self.bounds(addr, 2)?;
        let a = addr as usize;
        Ok(u16::from_le_bytes(self.bytes[a..a + 2].try_into().unwrap()))
    }

    pub fn store_u8(&mut self, addr: u64, val: u8) -> Result<(), VmError> {
        self.bounds(addr, 1)?;
        self.bytes[addr as usize] = val;
        Ok(())
    }

    pub fn load_u8(&self, addr: u64) -> Result<u8, VmError> {
        self.bounds(addr, 1)?;
        Ok(self.bytes[addr as usize])
    }

    pub fn store_f32(&mut self, addr: u64, val: f32) -> Result<(), VmError> {
        self.store_u32(addr, val.to_bits())
    }

    pub fn load_f32(&self, addr: u64) -> Result<f32, VmError> {
        Ok(f32::from_bits(self.load_u32(addr)?))
    }

    pub fn store_f64(&mut self, addr: u64, val: f64) -> Result<(), VmError> {
        self.store_u64(addr, val.to_bits())
    }

    pub fn load_f64(&self, addr: u64) -> Result<f64, VmError> {
        Ok(f64::from_bits(self.load_u64(addr)?))
    }

    pub fn store_bytes(&mut self, addr: u64, data: &[u8]) -> Result<(), VmError> {
        self.bounds(addr, data.len() as u64)?;
        let a = addr as usize;
        self.bytes[a..a + data.len()].copy_from_slice(data);
        Ok(())
    }

    pub fn load_bytes(&self, addr: u64, len: usize) -> Result<&[u8], VmError> {
        self.bounds(addr, len as u64)?;
        let a = addr as usize;
        Ok(&self.bytes[a..a + len])
    }

    pub fn initial_sp(&self) -> u64 {
        self.stack_top
    }

    pub fn heap_base(&self) -> u64 {
        0x10000
    }
}

// ---------------------------------------------------------------------------
// Register file
// ---------------------------------------------------------------------------

const REG_COUNT: usize = 256;

/// 64-bit general-purpose register file.
/// r0 is hardwired to zero. r1 is the stack pointer.
#[derive(Clone)]
pub struct RegFile {
    pub gpr: [u64; REG_COUNT],
}

impl RegFile {
    pub fn new(sp: u64) -> Self {
        let mut gpr = [0u64; REG_COUNT];
        gpr[1] = sp;
        Self { gpr }
    }

    pub fn read(&self, reg: RegisterId) -> u64 {
        let idx = reg as usize;
        if idx >= REG_COUNT {
            0
        } else {
            self.gpr[idx]
        }
    }

    pub fn write(&mut self, reg: RegisterId, value: u64) {
        let idx = reg as usize;
        if idx < REG_COUNT {
            self.gpr[idx] = value;
        }
    }

    pub fn sp(&self) -> u64 {
        self.gpr[1]
    }

    pub fn set_sp(&mut self, val: u64) {
        self.gpr[1] = val;
    }
}

// ---------------------------------------------------------------------------
// Stack frame
// ---------------------------------------------------------------------------

pub struct StackFrame {
    pub function_name: String,
    /// sp value to restore on return.
    pub caller_sp: u64,
    /// Saved register snapshot for caller-saved regs.
    pub saved_regs: HashMap<RegisterId, u64>,
    /// Local variable → stack offset mapping.
    pub local_offsets: HashMap<u32, u64>,
}

impl StackFrame {
    pub fn new(function_name: String, caller_sp: u64) -> Self {
        Self {
            function_name,
            caller_sp,
            saved_regs: HashMap::new(),
            local_offsets: HashMap::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Thread state
// ---------------------------------------------------------------------------

pub struct ThreadState {
    pub regs: RegFile,
    pub mem: VirtMem,
    pub call_stack: Vec<StackFrame>,
    /// Managed heap: objects allocated via intrinsics (malloc, constructors).
    /// Handles are indices into this vec.
    pub objects: Vec<Value>,
}

impl ThreadState {
    pub fn new() -> Self {
        let mem = VirtMem::new(HEAP_DEFAULT);
        let sp = mem.initial_sp();
        Self {
            regs: RegFile::new(sp),
            mem,
            call_stack: Vec::new(),
            objects: Vec::new(),
        }
    }

    pub fn push_frame(&mut self, func_name: String) {
        let sp = self.regs.sp();
        self.call_stack.push(StackFrame::new(func_name, sp));
    }

    pub fn pop_frame(&mut self) {
        if let Some(frame) = self.call_stack.pop() {
            self.regs.set_sp(frame.caller_sp);
            for (reg, val) in frame.saved_regs.iter() {
                self.regs.write(*reg, *val);
            }
        }
    }

    pub fn current_frame(&self) -> &StackFrame {
        self.call_stack
            .last()
            .expect("no active frame — missing function prologue")
    }

    pub fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.call_stack
            .last_mut()
            .expect("no active frame — missing function prologue")
    }

    pub fn local_addr(&self, local_idx: u32) -> u64 {
        self.current_frame()
            .local_offsets
            .get(&local_idx)
            .copied()
            .unwrap_or(0)
    }

    pub fn set_local_addr(&mut self, local_idx: u32, addr: u64) {
        self.current_frame_mut()
            .local_offsets
            .insert(local_idx, addr);
    }
}

// ---------------------------------------------------------------------------
// Type helpers
// ---------------------------------------------------------------------------

use fp_core::ast::Value;

pub fn raw_to_value(raw: u64, signed: bool, bits: u32) -> Value {
    match (signed, bits) {
        (true, 1) => Value::bool(raw != 0),
        (true, 8) => Value::int(raw as i8 as i64),
        (false, 8) => Value::uint(raw as u8 as u64),
        (true, 16) => Value::int(raw as i16 as i64),
        (false, 16) => Value::uint(raw as u16 as u64),
        (true, 32) => Value::int(raw as i32 as i64),
        (false, 32) => Value::uint(raw as u32 as u64),
        (true, _) => Value::int(raw as i64),
        (false, _) => Value::uint(raw),
    }
}

pub fn value_to_raw(v: &Value) -> u64 {
    match v {
        Value::Int(i) => i.value as u64,
        Value::UInt(u) => u.value,
        Value::Bool(b) => {
            if b.value {
                1
            } else {
                0
            }
        }
        Value::Decimal(f) => f.value.to_bits(),
        _ => 0,
    }
}

pub fn lir_type_info(ty: &fp_core::lir::LirType) -> (u32, bool) {
    use fp_core::lir::LirType;
    match ty {
        LirType::I1 => (1, false),
        LirType::I8 => (8, true),
        LirType::I16 => (16, true),
        LirType::I32 => (32, true),
        LirType::I64 => (64, true),
        LirType::F32 => (32, false),
        LirType::F64 => (64, false),
        LirType::Ptr(_) => (64, false), // pointer — treated as object handle
        LirType::Void => (0, false),
        _ => (64, false), // struct/array — also handles
    }
}

/// Returns true if this type is stored in the managed object heap (not a scalar).
pub fn is_object_type(ty: &fp_core::lir::LirType) -> bool {
    use fp_core::lir::LirType;
    matches!(
        ty,
        LirType::Ptr(_) | LirType::Struct { .. } | LirType::Array(..) | LirType::Vector(..)
    )
}

pub fn mem_store(
    mem: &mut VirtMem,
    addr: u64,
    raw: u64,
    ty: &fp_core::lir::LirType,
) -> Result<(), VmError> {
    let (bits, _) = lir_type_info(ty);
    match bits {
        1 | 8 => mem.store_u8(addr, raw as u8),
        16 => mem.store_u16(addr, raw as u16),
        32 => mem.store_u32(addr, raw as u32),
        _ => mem.store_u64(addr, raw),
    }
}

pub fn mem_load(mem: &VirtMem, addr: u64, ty: &fp_core::lir::LirType) -> Result<u64, VmError> {
    let (bits, signed) = lir_type_info(ty);
    let raw = match bits {
        1 | 8 => mem.load_u8(addr)? as u64,
        16 => mem.load_u16(addr)? as u64,
        32 => mem.load_u32(addr)? as u64,
        _ => mem.load_u64(addr)?,
    };
    if signed && bits < 64 {
        let shift = 64 - bits;
        Ok(((raw << shift) as i64 >> shift) as u64)
    } else {
        Ok(raw)
    }
}

impl Default for ThreadState {
    fn default() -> Self {
        Self::new()
    }
}
