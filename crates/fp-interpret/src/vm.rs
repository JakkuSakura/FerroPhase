use std::collections::HashMap;
use std::fmt;

use fp_core::lir::RegisterId;

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

const STACK_SIZE: u64 = 8 * 1024 * 1024;
const HEAP_DEFAULT: u64 = 64 * 1024 * 1024;

pub struct VirtMem {
    bytes: Vec<u8>,
    heap_next: u64,
    stack_top: u64,
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
            heap_next: 0x1000,
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

    pub fn stack_alloc(&mut self, sp: u64, size: u64, alignment: u32) -> Result<u64, VmError> {
        let aligned_size = (size + alignment as u64 - 1) & !(alignment as u64 - 1);
        let new_sp = sp.checked_sub(aligned_size).ok_or(VmError::StackOverflow)?;
        if new_sp < self.stack_low {
            return Err(VmError::StackOverflow);
        }
        self.bounds(new_sp, aligned_size)?;
        Ok(new_sp)
    }

    pub fn heap_alloc(&mut self, size: u64, alignment: u32) -> Result<u64, VmError> {
        let alignment = u64::from(alignment.max(1));
        let aligned = self
            .heap_next
            .checked_add(alignment - 1)
            .map(|value| value & !(alignment - 1))
            .ok_or(VmError::InvalidAddress(self.heap_next))?;
        let end = aligned
            .checked_add(size)
            .ok_or(VmError::InvalidAddress(aligned))?;
        self.bounds(aligned, size)?;
        self.heap_next = end;
        Ok(aligned)
    }

    pub fn store_bytes(&mut self, addr: u64, bytes: &[u8]) -> Result<(), VmError> {
        self.bounds(addr, bytes.len() as u64)?;
        let start = addr as usize;
        self.bytes[start..start + bytes.len()].copy_from_slice(bytes);
        Ok(())
    }

    pub fn load_bytes(&self, addr: u64, size: u64) -> Result<Vec<u8>, VmError> {
        self.bounds(addr, size)?;
        let start = addr as usize;
        Ok(self.bytes[start..start + size as usize].to_vec())
    }

    pub fn load_c_string(&self, addr: u64) -> Result<Vec<u8>, VmError> {
        let mut bytes = Vec::new();
        let mut current = addr;
        loop {
            let byte = self.load_u8(current)?;
            if byte == 0 {
                return Ok(bytes);
            }
            bytes.push(byte);
            current = current
                .checked_add(1)
                .ok_or(VmError::InvalidAddress(current))?;
        }
    }

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

    pub fn initial_sp(&self) -> u64 {
        self.stack_top
    }
}

/// Initial capacity only — `RegFile` grows on demand (see `write`), since
/// LIR register ids are numbered globally across an entire compiled
/// package/program (`MirToLirLowerer`'s `next_id()` never resets per
/// function), not locally per function. A fixed-size register file sized
/// for "one function's local register count" silently dropped writes to
/// any register id beyond it once a package grew large enough that a
/// deeply-compiled function's instructions referenced register ids past
/// this bound — e.g. a `const` global's own initializer, evaluated in
/// isolation via `run_function`, referencing register ids in the
/// thousands purely because of its position in the overall compile order.
const REG_COUNT: usize = 1024;

#[derive(Clone)]
pub struct RegFile {
    pub gpr: Vec<u64>,
}

impl RegFile {
    pub fn new(sp: u64) -> Self {
        let mut gpr = vec![0u64; REG_COUNT];
        gpr[1] = sp;
        Self { gpr }
    }

    pub fn write(&mut self, reg: RegisterId, value: u64) {
        let idx = reg as usize;
        if idx >= self.gpr.len() {
            self.gpr.resize(idx + 1, 0);
        }
        self.gpr[idx] = value;
    }

    pub fn sp(&self) -> u64 {
        self.gpr[1]
    }

    pub fn set_sp(&mut self, val: u64) {
        self.gpr[1] = val;
    }
}

pub struct StackFrame {
    pub caller_sp: u64,
    pub saved_regs: HashMap<RegisterId, u64>,
    pub local_offsets: HashMap<u32, u64>,
}

impl StackFrame {
    pub fn new(caller_sp: u64) -> Self {
        Self {
            caller_sp,
            saved_regs: HashMap::new(),
            local_offsets: HashMap::new(),
        }
    }
}

pub struct ThreadState {
    pub regs: RegFile,
    pub mem: VirtMem,
    pub call_stack: Vec<StackFrame>,
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

    pub fn push_frame(&mut self, _func_name: String) {
        let sp = self.regs.sp();
        self.call_stack.push(StackFrame::new(sp));
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
            .expect("no active frame - missing function prologue")
    }

    pub fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.call_stack
            .last_mut()
            .expect("no active frame - missing function prologue")
    }

    pub fn local_addr(&self, local_idx: u32) -> Result<u64, VmError> {
        self.current_frame()
            .local_offsets
            .get(&local_idx)
            .copied()
            .ok_or(VmError::InvalidAddress(local_idx as u64))
    }

    pub fn set_local_addr(&mut self, local_idx: u32, addr: u64) {
        self.current_frame_mut()
            .local_offsets
            .insert(local_idx, addr);
    }
}

use fp_core::ast::Value;

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
        LirType::Ptr(_) => (64, false),
        LirType::Void => (0, false),
        _ => (64, false),
    }
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
