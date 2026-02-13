use crate::emit::{EmitPlan, RelocKind, RelocSection, TargetArch, TargetFormat};
use crate::ffi::DynamicLibrary;
use fp_core::error::{Error, Result};
use fp_core::lir::LirProgram;
use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::Arc;
#[cfg(unix)]
use libc;

#[derive(Debug)]
pub struct JitModule {
    text: JitMemory,
    rodata: JitMemory,
    entry: *const c_void,
    symbols: HashMap<String, *const c_void>,
    _lib: Arc<DynamicLibrary>,
}

impl JitModule {
    pub fn entry_ptr(&self) -> *const c_void {
        self.entry
    }

    pub fn symbol_ptr(&self, name: &str) -> Option<*const c_void> {
        self.symbols.get(name).copied()
    }
}

pub struct JitEngine {
    format: TargetFormat,
    arch: TargetArch,
    lib: Arc<DynamicLibrary>,
}

impl JitEngine {
    pub fn new(target_triple: Option<&str>) -> Result<Self> {
        let (format, arch) = crate::emit::detect_target(target_triple)?;
        Ok(Self {
            format,
            arch,
            lib: Arc::new(DynamicLibrary::open_default()?),
        })
    }

    pub fn compile(&mut self, program: &LirProgram) -> Result<JitModule> {
        let plan = crate::emit::emit_plan(program, self.format, self.arch)?;
        self.compile_plan(&plan)
    }

    pub fn compile_plan(&mut self, plan: &EmitPlan) -> Result<JitModule> {
        let text = JitMemory::new(plan.text.len(), MemoryKind::Text)?;
        let rodata = JitMemory::new(plan.rodata.len(), MemoryKind::Rodata)?;

        unsafe {
            text.write_bytes(&plan.text)?;
            rodata.write_bytes(&plan.rodata)?;
        }

        let text_base = text.ptr as u64;
        let rodata_base = rodata.ptr as u64;

        self.apply_relocations(plan, text_base, rodata_base, &text, &rodata)?;

        text.make_executable()?;
        rodata.make_readonly()?;

        let mut symbols = HashMap::new();
        for (name, offset) in &plan.symbols {
            symbols.insert(name.clone(), (text_base + *offset) as *const c_void);
        }
        for (name, offset) in &plan.rodata_symbols {
            symbols.insert(name.clone(), (rodata_base + *offset) as *const c_void);
        }

        let entry = (text_base + plan.entry_offset) as *const c_void;

        Ok(JitModule {
            text,
            rodata,
            entry,
            symbols,
            _lib: Arc::clone(&self.lib),
        })
    }

    fn apply_relocations(
        &self,
        plan: &EmitPlan,
        text_base: u64,
        rodata_base: u64,
        text: &JitMemory,
        rodata: &JitMemory,
    ) -> Result<()> {
        for reloc in &plan.relocs {
            let (section_base, section_mem) = match reloc.section {
                RelocSection::Text => (text_base, text),
                RelocSection::Rdata => (rodata_base, rodata),
            };
            let location = section_base
                .checked_add(reloc.offset)
                .ok_or_else(|| Error::from("relocation offset overflow"))?;

            let target = self.resolve_symbol_addr(plan, &reloc.symbol, text_base, rodata_base)?;
            let target = target
                .checked_add(reloc.addend as u64)
                .ok_or_else(|| Error::from("relocation addend overflow"))?;

            match reloc.kind {
                RelocKind::Abs64 => unsafe {
                    section_mem.write_u64(reloc.offset as usize, target)?;
                },
                RelocKind::CallRel32 => {
                    if !matches!(self.arch, TargetArch::X86_64) {
                        return Err(Error::from("CallRel32 relocation only supported on x86_64"));
                    }
                    let next = location
                        .checked_add(4)
                        .ok_or_else(|| Error::from("relocation overflow"))?;
                    let disp = target as i64 - next as i64;
                    let disp32 = i32::try_from(disp)
                        .map_err(|_| Error::from("call relocation out of range"))?;
                    unsafe {
                        section_mem.write_i32(reloc.offset as usize, disp32)?;
                    }
                }
                RelocKind::Aarch64AdrpAdd => {
                    if !matches!(self.arch, TargetArch::Aarch64) {
                        return Err(Error::from(
                            "Aarch64AdrpAdd relocation only supported on aarch64",
                        ));
                    }
                    self.apply_aarch64_adrp_add(section_mem, reloc.offset as usize, location, target)?;
                }
            }
        }
        Ok(())
    }

    fn resolve_symbol_addr(
        &self,
        plan: &EmitPlan,
        symbol: &str,
        text_base: u64,
        rodata_base: u64,
    ) -> Result<u64> {
        if symbol == ".rodata" {
            return Ok(rodata_base);
        }
        if let Some(offset) = plan.symbols.get(symbol) {
            return Ok(text_base + *offset);
        }
        if let Some(offset) = plan.rodata_symbols.get(symbol) {
            return Ok(rodata_base + *offset);
        }
        let ptr = self.lib.symbol(symbol)? as u64;
        Ok(ptr)
    }

    fn apply_aarch64_adrp_add(
        &self,
        section: &JitMemory,
        offset: usize,
        insn_addr: u64,
        target: u64,
    ) -> Result<()> {
        if offset + 8 > section.len {
            return Err(Error::from("relocation offset out of range"));
        }
        let orig_adrp = unsafe { section.read_u32(offset)? };
        let orig_add = unsafe { section.read_u32(offset + 4)? };

        let rd = orig_adrp & 0x1f;
        let rn = (orig_add >> 5) & 0x1f;

        let pc_page = insn_addr & !0xfffu64;
        let target_page = target & !0xfffu64;
        let delta = target_page as i64 - pc_page as i64;
        let imm = delta >> 12;
        let immlo = (imm as u32) & 0x3;
        let immhi = ((imm as u32) >> 2) & 0x7ffff;
        let adrp = 0x9000_0000u32 | (immlo << 29) | (immhi << 5) | rd;

        let imm12 = (target & 0xfff) as u32;
        let add = 0x9100_0000u32 | (imm12 << 10) | (rn << 5) | (orig_add & 0x1f);

        unsafe {
            section.write_u32(offset, adrp)?;
            section.write_u32(offset + 4, add)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct JitMemory {
    ptr: *mut u8,
    len: usize,
    kind: MemoryKind,
}

#[derive(Debug, Clone, Copy)]
enum MemoryKind {
    Text,
    Rodata,
}

impl JitMemory {
    fn new(len: usize, kind: MemoryKind) -> Result<Self> {
        let len = len.max(1);
        let len = align_up(len, page_size()?);
        let ptr = unsafe { alloc_pages(len)? };
        Ok(Self { ptr, len, kind })
    }

    unsafe fn write_bytes(&self, bytes: &[u8]) -> Result<()> {
        if bytes.len() > self.len {
            return Err(Error::from("jit buffer overflow"));
        }
        unsafe {
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), self.ptr, bytes.len());
        }
        Ok(())
    }

    unsafe fn write_u64(&self, offset: usize, value: u64) -> Result<()> {
        if offset + 8 > self.len {
            return Err(Error::from("jit relocation overflow"));
        }
        unsafe {
            let dst = self.ptr.add(offset) as *mut u64;
            dst.write_unaligned(value.to_le());
        }
        Ok(())
    }

    unsafe fn write_i32(&self, offset: usize, value: i32) -> Result<()> {
        if offset + 4 > self.len {
            return Err(Error::from("jit relocation overflow"));
        }
        unsafe {
            let dst = self.ptr.add(offset) as *mut i32;
            dst.write_unaligned(value.to_le());
        }
        Ok(())
    }

    unsafe fn write_u32(&self, offset: usize, value: u32) -> Result<()> {
        if offset + 4 > self.len {
            return Err(Error::from("jit relocation overflow"));
        }
        unsafe {
            let dst = self.ptr.add(offset) as *mut u32;
            dst.write_unaligned(value.to_le());
        }
        Ok(())
    }

    unsafe fn read_u32(&self, offset: usize) -> Result<u32> {
        if offset + 4 > self.len {
            return Err(Error::from("jit relocation overflow"));
        }
        unsafe {
            let src = self.ptr.add(offset) as *const u32;
            Ok(u32::from_le(src.read_unaligned()))
        }
    }

    fn make_executable(&self) -> Result<()> {
        match self.kind {
            MemoryKind::Text => unsafe { protect_pages(self.ptr, self.len, MemoryProtection::ReadExecute) },
            MemoryKind::Rodata => Ok(()),
        }
    }

    fn make_readonly(&self) -> Result<()> {
        match self.kind {
            MemoryKind::Rodata => unsafe { protect_pages(self.ptr, self.len, MemoryProtection::ReadOnly) },
            MemoryKind::Text => Ok(()),
        }
    }
}

impl Drop for JitMemory {
    fn drop(&mut self) {
        unsafe {
            let _ = free_pages(self.ptr, self.len);
        }
    }
}

fn align_up(value: usize, align: usize) -> usize {
    if align == 0 {
        return value;
    }
    (value + (align - 1)) & !(align - 1)
}

#[cfg(test)]
mod tests {
    use super::JitEngine;
    use fp_core::lir::{
        CallingConvention, Linkage, LirBasicBlock, LirFunction, LirFunctionSignature, LirProgram,
        LirTerminator, LirType, Name,
    };

    fn minimal_program() -> LirProgram {
        let func = LirFunction {
            name: Name::new("main"),
            signature: LirFunctionSignature {
                params: Vec::new(),
                return_type: LirType::I32,
                is_variadic: false,
            },
            basic_blocks: vec![LirBasicBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: Vec::new(),
                terminator: LirTerminator::Return(None),
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention: CallingConvention::C,
            linkage: Linkage::External,
            is_declaration: false,
        };

        LirProgram {
            functions: vec![func],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        }
    }

    #[test]
    fn jit_smoke_compiles_minimal_program() {
        let program = minimal_program();
        let mut engine = JitEngine::new(None).expect("jit engine");
        let module = engine.compile(&program).expect("jit compile");
        assert!(!module.entry_ptr().is_null(), "entry pointer should be non-null");
        assert!(
            module.symbol_ptr("main").is_some(),
            "main symbol should be present"
        );
    }
}

#[derive(Debug, Clone, Copy)]
enum MemoryProtection {
    ReadWrite,
    ReadOnly,
    ReadExecute,
}

#[cfg(unix)]
unsafe fn alloc_pages(len: usize) -> Result<*mut u8> {
    let ptr = unsafe {
        mmap(
            std::ptr::null_mut(),
            len,
            PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANON,
            -1,
            0,
        )
    };
    if ptr == MAP_FAILED {
        return Err(Error::from("mmap failed"));
    }
    Ok(ptr as *mut u8)
}

#[cfg(unix)]
unsafe fn protect_pages(ptr: *mut u8, len: usize, prot: MemoryProtection) -> Result<()> {
    let flags = match prot {
        MemoryProtection::ReadWrite => PROT_READ | PROT_WRITE,
        MemoryProtection::ReadOnly => PROT_READ,
        MemoryProtection::ReadExecute => PROT_READ | PROT_EXEC,
    };
    let result = unsafe { mprotect(ptr as *mut c_void, len, flags) };
    if result != 0 {
        return Err(Error::from("mprotect failed"));
    }
    Ok(())
}

#[cfg(unix)]
unsafe fn free_pages(ptr: *mut u8, len: usize) -> Result<()> {
    let result = unsafe { munmap(ptr as *mut c_void, len) };
    if result != 0 {
        return Err(Error::from("munmap failed"));
    }
    Ok(())
}

#[cfg(unix)]
fn page_size() -> Result<usize> {
    let size = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };
    if size <= 0 {
        return Err(Error::from("sysconf failed"));
    }
    Ok(size as usize)
}

#[cfg(windows)]
unsafe fn alloc_pages(len: usize) -> Result<*mut u8> {
    let ptr = unsafe {
        VirtualAlloc(
            std::ptr::null_mut(),
            len,
            MEM_COMMIT | MEM_RESERVE,
            PAGE_READWRITE,
        )
    };
    if ptr.is_null() {
        return Err(Error::from("VirtualAlloc failed"));
    }
    Ok(ptr as *mut u8)
}

#[cfg(windows)]
unsafe fn protect_pages(ptr: *mut u8, len: usize, prot: MemoryProtection) -> Result<()> {
    let flags = match prot {
        MemoryProtection::ReadWrite => PAGE_READWRITE,
        MemoryProtection::ReadOnly => PAGE_READONLY,
        MemoryProtection::ReadExecute => PAGE_EXECUTE_READ,
    };
    let mut old = 0u32;
    let ok = unsafe { VirtualProtect(ptr as *mut c_void, len, flags, &mut old) };
    if ok == 0 {
        return Err(Error::from("VirtualProtect failed"));
    }
    Ok(())
}

#[cfg(windows)]
unsafe fn free_pages(ptr: *mut u8, _len: usize) -> Result<()> {
    let ok = unsafe { VirtualFree(ptr as *mut c_void, 0, MEM_RELEASE) };
    if ok == 0 {
        return Err(Error::from("VirtualFree failed"));
    }
    Ok(())
}

#[cfg(windows)]
fn page_size() -> Result<usize> {
    unsafe {
        let mut info = SYSTEM_INFO::default();
        GetSystemInfo(&mut info);
        Ok(info.dwPageSize as usize)
    }
}

#[cfg(unix)]
const PROT_READ: i32 = 0x1;
#[cfg(unix)]
const PROT_WRITE: i32 = 0x2;
#[cfg(unix)]
const PROT_EXEC: i32 = 0x4;
#[cfg(unix)]
const MAP_PRIVATE: i32 = 0x02;
#[cfg(unix)]
const MAP_ANON: i32 = 0x1000;
#[cfg(unix)]
const MAP_FAILED: *mut c_void = !0 as *mut c_void;

#[cfg(unix)]
unsafe extern "C" {
    fn mmap(addr: *mut c_void, len: usize, prot: i32, flags: i32, fd: i32, offset: isize) -> *mut c_void;
    fn mprotect(addr: *mut c_void, len: usize, prot: i32) -> i32;
    fn munmap(addr: *mut c_void, len: usize) -> i32;
}

#[cfg(windows)]
const MEM_COMMIT: u32 = 0x1000;
#[cfg(windows)]
const MEM_RESERVE: u32 = 0x2000;
#[cfg(windows)]
const MEM_RELEASE: u32 = 0x8000;
#[cfg(windows)]
const PAGE_READWRITE: u32 = 0x04;
#[cfg(windows)]
const PAGE_READONLY: u32 = 0x02;
#[cfg(windows)]
const PAGE_EXECUTE_READ: u32 = 0x20;

#[cfg(windows)]
#[repr(C)]
#[derive(Default)]
struct SYSTEM_INFO {
    wProcessorArchitecture: u16,
    wReserved: u16,
    dwPageSize: u32,
    lpMinimumApplicationAddress: *mut c_void,
    lpMaximumApplicationAddress: *mut c_void,
    dwActiveProcessorMask: usize,
    dwNumberOfProcessors: u32,
    dwProcessorType: u32,
    dwAllocationGranularity: u32,
    wProcessorLevel: u16,
    wProcessorRevision: u16,
}

#[cfg(windows)]
unsafe extern "system" {
    fn VirtualAlloc(addr: *mut c_void, size: usize, alloc_type: u32, protect: u32) -> *mut c_void;
    fn VirtualProtect(addr: *mut c_void, size: usize, new_protect: u32, old_protect: *mut u32) -> i32;
    fn VirtualFree(addr: *mut c_void, size: usize, free_type: u32) -> i32;
    fn GetSystemInfo(info: *mut SYSTEM_INFO);
}
