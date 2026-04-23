use crate::emit::{EmitPlan, RelocKind, RelocSection, TargetArch, TargetFormat};
use crate::ffi::DynamicLibrary;
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, LirInstructionKind, LirProgram, LirType};
#[cfg(unix)]
use libc;
use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::Arc;

const JIT_EXTERNAL_STUB_ALIGN: usize = 16;
const JIT_X86_64_EXTERNAL_STUB_BYTES: usize = 12;
const JIT_AARCH64_EXTERNAL_STUB_BYTES: usize = 16;

#[derive(Debug, Clone, PartialEq)]
pub enum HostScalar {
    Void,
    Bool(bool),
    I64(i64),
    F64(f64),
}

#[derive(Debug, Clone)]
struct FunctionMetadata {
    param_count: usize,
    return_type: LirType,
    calling_convention: CallingConvention,
}

#[derive(Debug)]
pub struct JitModule {
    _text: JitMemory,
    _rodata: JitMemory,
    entry: *const c_void,
    symbols: HashMap<String, *const c_void>,
    signatures: HashMap<String, FunctionMetadata>,
    _lib: Arc<DynamicLibrary>,
}

impl JitModule {
    pub fn entry_ptr(&self) -> *const c_void {
        self.entry
    }

    pub fn symbol_ptr(&self, name: &str) -> Option<*const c_void> {
        self.symbols.get(name).copied()
    }

    pub fn return_type(&self, name: &str) -> Option<&LirType> {
        self.signatures.get(name).map(|meta| &meta.return_type)
    }

    pub unsafe fn call0_scalar(&self, name: &str) -> Result<HostScalar> {
        let meta = self
            .signatures
            .get(name)
            .ok_or_else(|| Error::from(format!("missing JIT symbol metadata for {name}")))?;
        if meta.param_count != 0 {
            return Err(Error::from(format!(
                "JIT call helper requires zero-arg function, got {} args for {name}",
                meta.param_count
            )));
        }
        ensure_supported_host_call_conv(&meta.calling_convention)?;
        let ptr = self
            .symbol_ptr(name)
            .ok_or_else(|| Error::from(format!("missing JIT symbol {name}")))?;
        match &meta.return_type {
            LirType::Void => {
                let func: unsafe extern "C" fn() = unsafe { std::mem::transmute(ptr) };
                unsafe { func() };
                Ok(HostScalar::Void)
            }
            LirType::I1 => {
                let func: unsafe extern "C" fn() -> u8 = unsafe { std::mem::transmute(ptr) };
                Ok(HostScalar::Bool(unsafe { func() } != 0))
            }
            LirType::I8 => {
                let func: unsafe extern "C" fn() -> i8 = unsafe { std::mem::transmute(ptr) };
                Ok(HostScalar::I64(unsafe { func() } as i64))
            }
            LirType::I16 => {
                let func: unsafe extern "C" fn() -> i16 = unsafe { std::mem::transmute(ptr) };
                Ok(HostScalar::I64(unsafe { func() } as i64))
            }
            LirType::I32 => {
                let func: unsafe extern "C" fn() -> i32 = unsafe { std::mem::transmute(ptr) };
                Ok(HostScalar::I64(unsafe { func() } as i64))
            }
            LirType::I64 => {
                let func: unsafe extern "C" fn() -> i64 = unsafe { std::mem::transmute(ptr) };
                Ok(HostScalar::I64(unsafe { func() }))
            }
            LirType::F32 => {
                let func: unsafe extern "C" fn() -> f32 = unsafe { std::mem::transmute(ptr) };
                Ok(HostScalar::F64(unsafe { func() } as f64))
            }
            LirType::F64 => {
                let func: unsafe extern "C" fn() -> f64 = unsafe { std::mem::transmute(ptr) };
                Ok(HostScalar::F64(unsafe { func() }))
            }
            other => Err(Error::from(format!(
                "unsupported JIT scalar return type for {name}: {other:?}"
            ))),
        }
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
        validate_host_program(program)?;
        let plan = crate::emit::emit_plan(program, self.format, self.arch)?;
        self.compile_plan(&plan)
    }

    pub fn compile_plan(&mut self, plan: &EmitPlan) -> Result<JitModule> {
        self.compile_plan_with_symbols(plan, &[])
    }

    pub fn compile_plan_with_symbols(
        &mut self,
        plan: &EmitPlan,
        extra_symbols: &[(&str, *const c_void)],
    ) -> Result<JitModule> {
        let external_call_symbols = collect_external_call_symbols(plan);
        let text_len = jit_text_len(plan.text.len(), self.arch, external_call_symbols.len())?;
        let text = JitMemory::new(text_len, MemoryKind::Text)?;
        let rodata = JitMemory::new(plan.rodata.len(), MemoryKind::Rodata)?;

        unsafe {
            text.write_bytes(&plan.text)?;
            rodata.write_bytes(&plan.rodata)?;
        }

        let text_base = text.ptr as u64;
        let rodata_base = rodata.ptr as u64;
        let external_call_stubs = self.materialize_external_call_stubs(
            plan,
            external_call_symbols.as_slice(),
            text_base,
            rodata_base,
            &text,
            extra_symbols,
        )?;

        self.apply_relocations(
            plan,
            text_base,
            rodata_base,
            &text,
            &rodata,
            extra_symbols,
            &external_call_stubs,
        )?;

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
            _text: text,
            _rodata: rodata,
            entry,
            symbols,
            signatures: HashMap::new(),
            _lib: Arc::clone(&self.lib),
        })
    }

    pub fn compile_host(&mut self, program: &LirProgram) -> Result<JitModule> {
        let mut module = self.compile(program)?;
        module.signatures = collect_signatures(program);
        Ok(module)
    }

    fn apply_relocations(
        &self,
        plan: &EmitPlan,
        text_base: u64,
        rodata_base: u64,
        text: &JitMemory,
        rodata: &JitMemory,
        extra_symbols: &[(&str, *const c_void)],
        external_call_stubs: &HashMap<String, u64>,
    ) -> Result<()> {
        for reloc in &plan.relocs {
            let (section_base, section_mem) = match reloc.section {
                RelocSection::Text => (text_base, text),
                RelocSection::Rdata => (rodata_base, rodata),
                RelocSection::Data => {
                    return Err(Error::from(
                        "JIT does not support relocations against writable .data",
                    ));
                }
            };
            let location = section_base
                .checked_add(reloc.offset)
                .ok_or_else(|| Error::from("relocation offset overflow"))?;

            let target = self.resolve_symbol_addr(
                plan,
                &reloc.symbol,
                text_base,
                rodata_base,
                extra_symbols,
            )?;
            let target = target
                .checked_add(reloc.addend as u64)
                .ok_or_else(|| Error::from("relocation addend overflow"))?;

            match reloc.kind {
                RelocKind::Abs64 => unsafe {
                    section_mem.write_u64(reloc.offset as usize, target)?;
                },
                RelocKind::CallRel32 => {
                    let branch_target = external_call_stubs
                        .get(&reloc.symbol)
                        .copied()
                        .unwrap_or(target);
                    self.apply_call_rel32(
                        section_mem,
                        reloc.offset as usize,
                        location,
                        branch_target,
                    )?;
                }
                RelocKind::Aarch64AdrpAdd => {
                    if !matches!(self.arch, TargetArch::Aarch64) {
                        return Err(Error::from(
                            "Aarch64AdrpAdd relocation only supported on aarch64",
                        ));
                    }
                    self.apply_aarch64_adrp_add(
                        section_mem,
                        reloc.offset as usize,
                        location,
                        target,
                    )?;
                }
                RelocKind::Aarch64GotLoad => {
                    return Err(Error::from(
                        "Aarch64GotLoad relocations are not supported by the JIT",
                    ));
                }
            }
        }
        Ok(())
    }

    fn materialize_external_call_stubs(
        &self,
        plan: &EmitPlan,
        external_call_symbols: &[String],
        text_base: u64,
        rodata_base: u64,
        text: &JitMemory,
        extra_symbols: &[(&str, *const c_void)],
    ) -> Result<HashMap<String, u64>> {
        if external_call_symbols.is_empty() {
            return Ok(HashMap::new());
        }
        let stub_size = external_call_stub_size(self.arch);
        let stub_base = align_up(plan.text.len(), JIT_EXTERNAL_STUB_ALIGN);
        let mut stubs = HashMap::with_capacity(external_call_symbols.len());
        for (index, symbol) in external_call_symbols.iter().enumerate() {
            let stub_offset = stub_base
                .checked_add(
                    index
                        .checked_mul(stub_size)
                        .ok_or_else(|| Error::from("stub offset overflow"))?,
                )
                .ok_or_else(|| Error::from("stub offset overflow"))?;
            let stub_addr = text_base
                .checked_add(stub_offset as u64)
                .ok_or_else(|| Error::from("stub address overflow"))?;
            let target =
                self.resolve_symbol_addr(plan, symbol, text_base, rodata_base, extra_symbols)?;
            self.emit_external_call_stub(text, stub_offset, target)?;
            stubs.insert(symbol.clone(), stub_addr);
        }
        Ok(stubs)
    }

    fn emit_external_call_stub(
        &self,
        text: &JitMemory,
        stub_offset: usize,
        target: u64,
    ) -> Result<()> {
        match self.arch {
            TargetArch::X86_64 => unsafe {
                let mut stub = [0u8; JIT_X86_64_EXTERNAL_STUB_BYTES];
                stub[0] = 0x48;
                stub[1] = 0xB8;
                stub[2..10].copy_from_slice(&target.to_le_bytes());
                stub[10] = 0xFF;
                stub[11] = 0xE0;
                text.write_bytes_at(stub_offset, &stub)?;
            },
            TargetArch::Aarch64 => unsafe {
                let mut stub = [0u8; JIT_AARCH64_EXTERNAL_STUB_BYTES];
                let ldr = 0x5800_0050u32;
                let br = 0xD61F_0200u32;
                stub[0..4].copy_from_slice(&ldr.to_le_bytes());
                stub[4..8].copy_from_slice(&br.to_le_bytes());
                stub[8..16].copy_from_slice(&target.to_le_bytes());
                text.write_bytes_at(stub_offset, &stub)?;
            },
        }
        Ok(())
    }

    fn apply_call_rel32(
        &self,
        section: &JitMemory,
        offset: usize,
        location: u64,
        target: u64,
    ) -> Result<()> {
        match self.arch {
            TargetArch::X86_64 => {
                let next = location
                    .checked_add(4)
                    .ok_or_else(|| Error::from("relocation overflow"))?;
                let disp = target as i64 - next as i64;
                let disp32 =
                    i32::try_from(disp).map_err(|_| Error::from("call relocation out of range"))?;
                unsafe {
                    section.write_i32(offset, disp32)?;
                }
                Ok(())
            }
            TargetArch::Aarch64 => {
                let delta = target as i64 - location as i64;
                if delta & 0b11 != 0 {
                    return Err(Error::from("aarch64 call target must be 4-byte aligned"));
                }
                let imm = delta / 4;
                if imm < -(1 << 25) || imm > (1 << 25) - 1 {
                    return Err(Error::from("aarch64 call relocation out of range"));
                }
                let encoded = 0x9400_0000u32 | ((imm as u32) & 0x03FF_FFFF);
                unsafe {
                    section.write_u32(offset, encoded)?;
                }
                Ok(())
            }
        }
    }

    fn resolve_symbol_addr(
        &self,
        plan: &EmitPlan,
        symbol: &str,
        text_base: u64,
        rodata_base: u64,
        extra_symbols: &[(&str, *const c_void)],
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
        if let Some((_, ptr)) = extra_symbols.iter().find(|(name, _)| *name == symbol) {
            return Ok(*ptr as u64);
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

pub fn validate_host_program(program: &LirProgram) -> Result<()> {
    let _ = crate::emit::host_arch(None)?;
    validate_native_program(program)
}

pub fn validate_native_program(program: &LirProgram) -> Result<()> {
    if !program.queries.is_empty() {
        return Err(Error::from(
            "fp-native does not support query-bearing LIR programs",
        ));
    }
    for function in &program.functions {
        for block in &function.basic_blocks {
            for instruction in &block.instructions {
                if matches!(instruction.kind, LirInstructionKind::ExecQuery(_)) {
                    return Err(Error::from(format!(
                        "fp-native does not support ExecQuery in function {}",
                        function.name
                    )));
                }
            }
        }
    }
    Ok(())
}

fn collect_signatures(program: &LirProgram) -> HashMap<String, FunctionMetadata> {
    program
        .functions
        .iter()
        .filter(|function| !function.is_declaration)
        .map(|function| {
            (
                function.name.to_string(),
                FunctionMetadata {
                    param_count: function.signature.params.len(),
                    return_type: function.signature.return_type.clone(),
                    calling_convention: function.calling_convention.clone(),
                },
            )
        })
        .collect()
}

fn ensure_supported_host_call_conv(call_conv: &CallingConvention) -> Result<()> {
    match call_conv {
        CallingConvention::C
        | CallingConvention::Win64
        | CallingConvention::X86_64SysV
        | CallingConvention::AAPCS
        | CallingConvention::AAPCSVfp => Ok(()),
        other => Err(Error::from(format!(
            "unsupported JIT host entry calling convention: {other:?}"
        ))),
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

    unsafe fn write_bytes_at(&self, offset: usize, bytes: &[u8]) -> Result<()> {
        let end = offset
            .checked_add(bytes.len())
            .ok_or_else(|| Error::from("jit buffer overflow"))?;
        if end > self.len {
            return Err(Error::from("jit buffer overflow"));
        }
        unsafe {
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), self.ptr.add(offset), bytes.len());
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
            MemoryKind::Text => unsafe {
                protect_pages(self.ptr, self.len, MemoryProtection::ReadExecute)
            },
            MemoryKind::Rodata => Ok(()),
        }
    }

    fn make_readonly(&self) -> Result<()> {
        match self.kind {
            MemoryKind::Rodata => unsafe {
                protect_pages(self.ptr, self.len, MemoryProtection::ReadOnly)
            },
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

fn jit_text_len(
    base_text_len: usize,
    arch: TargetArch,
    external_stub_count: usize,
) -> Result<usize> {
    if external_stub_count == 0 {
        return Ok(base_text_len);
    }
    let stub_bytes = external_call_stub_size(arch)
        .checked_mul(external_stub_count)
        .ok_or_else(|| Error::from("jit external stub size overflow"))?;
    align_up(base_text_len, JIT_EXTERNAL_STUB_ALIGN)
        .checked_add(stub_bytes)
        .ok_or_else(|| Error::from("jit text size overflow"))
}

fn external_call_stub_size(arch: TargetArch) -> usize {
    match arch {
        TargetArch::X86_64 => JIT_X86_64_EXTERNAL_STUB_BYTES,
        TargetArch::Aarch64 => JIT_AARCH64_EXTERNAL_STUB_BYTES,
    }
}

fn collect_external_call_symbols(plan: &EmitPlan) -> Vec<String> {
    let mut symbols = Vec::new();
    for reloc in &plan.relocs {
        if reloc.kind != RelocKind::CallRel32 {
            continue;
        }
        if plan.symbols.contains_key(&reloc.symbol) {
            continue;
        }
        if symbols.iter().any(|existing| existing == &reloc.symbol) {
            continue;
        }
        symbols.push(reloc.symbol.clone());
    }
    symbols
}

#[cfg(test)]
mod tests {
    use super::{HostScalar, JitEngine, validate_native_program};
    use fp_core::lir::{
        CallingConvention, Linkage, LirBasicBlock, LirFunction, LirFunctionSignature,
        LirInstruction, LirInstructionKind, LirProgram, LirTerminator, LirType, LirValue, Name,
    };
    use std::ffi::c_void;

    fn minimal_program() -> LirProgram {
        let func = LirFunction {
            name: Name::new("main"),
            signature: LirFunctionSignature {
                params: Vec::new(),
                return_type: LirType::Void,
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
            queries: Vec::new(),
        }
    }

    #[test]
    fn jit_smoke_compiles_minimal_program() {
        let program = minimal_program();
        let mut engine = JitEngine::new(None).expect("jit engine");
        let module = engine.compile_host(&program).expect("jit compile");
        assert!(
            !module.entry_ptr().is_null(),
            "entry pointer should be non-null"
        );
        assert!(
            module.symbol_ptr("main").is_some(),
            "main symbol should be present"
        );
        let scalar = unsafe { module.call0_scalar("main") }.expect("call jit main");
        assert_eq!(scalar, HostScalar::Void);
    }

    #[test]
    fn validate_host_program_rejects_exec_query() {
        let mut program = minimal_program();
        program.functions[0].basic_blocks[0]
            .instructions
            .push(LirInstruction {
                id: 1,
                kind: LirInstructionKind::ExecQuery(fp_core::lir::LirQuery {
                    query_id: 1,
                    origin: fp_core::query::QueryOrigin::Fp,
                    ir: fp_core::query::QueryIrDocument::default(),
                    span: fp_core::span::Span::default(),
                }),
                type_hint: None,
                debug_info: None,
            });
        let err = validate_native_program(&program).expect_err("exec query must be rejected");
        assert!(
            err.to_string().contains("ExecQuery"),
            "unexpected error: {err}"
        );
    }

    extern "C" fn jit_test_add1() -> i64 {
        42
    }

    fn external_call_program() -> LirProgram {
        let callee = LirFunction {
            name: Name::new("jit_test_add1"),
            signature: LirFunctionSignature {
                params: Vec::new(),
                return_type: LirType::I64,
                is_variadic: false,
            },
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            stack_slots: Vec::new(),
            calling_convention: CallingConvention::C,
            linkage: Linkage::External,
            is_declaration: true,
        };
        let caller = LirFunction {
            name: Name::new("main"),
            signature: LirFunctionSignature {
                params: Vec::new(),
                return_type: LirType::I64,
                is_variadic: false,
            },
            basic_blocks: vec![LirBasicBlock {
                id: 0,
                label: Some(Name::new("entry")),
                instructions: vec![LirInstruction {
                    id: 1,
                    kind: LirInstructionKind::Call {
                        function: LirValue::Function("jit_test_add1".to_string()),
                        args: Vec::new(),
                        calling_convention: CallingConvention::C,
                        tail_call: false,
                    },
                    type_hint: Some(LirType::I64),
                    debug_info: None,
                }],
                terminator: LirTerminator::Return(Some(LirValue::Register(1))),
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
            functions: vec![callee, caller],
            globals: Vec::new(),
            type_definitions: Vec::new(),
            queries: Vec::new(),
        }
    }

    #[test]
    fn jit_compile_plan_with_external_symbol_call_works() {
        let program = external_call_program();
        let mut engine = JitEngine::new(None).expect("jit engine");
        let plan = crate::emit::emit_plan(&program, engine.format, engine.arch).expect("emit plan");
        let module = engine
            .compile_plan_with_symbols(&plan, &[("jit_test_add1", jit_test_add1 as *const c_void)])
            .expect("jit compile plan with symbols");
        let main = module.symbol_ptr("main").expect("main ptr");
        let func: unsafe extern "C" fn() -> i64 = unsafe { std::mem::transmute(main) };
        let value = unsafe { func() };
        assert_eq!(value, 42);
    }
}

#[derive(Debug, Clone, Copy)]
enum MemoryProtection {
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
    fn mmap(
        addr: *mut c_void,
        len: usize,
        prot: i32,
        flags: i32,
        fd: i32,
        offset: isize,
    ) -> *mut c_void;
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
    fn VirtualProtect(
        addr: *mut c_void,
        size: usize,
        new_protect: u32,
        old_protect: *mut u32,
    ) -> i32;
    fn VirtualFree(addr: *mut c_void, size: usize, free_type: u32) -> i32;
    fn GetSystemInfo(info: *mut SYSTEM_INFO);
}
