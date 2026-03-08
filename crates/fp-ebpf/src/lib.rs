use fp_core::error::{Error, Result};
use fp_core::lir::layout::{align_of, size_of};
use fp_core::lir::{
    LirConstant, LirFunction, LirInstruction, LirInstructionKind, LirProgram, LirTerminator,
    LirType, LirValue,
};
use fp_core::pretty::{PrettyOptions, pretty};
use object::write::{Object, Relocation, Symbol, SymbolSection};
use object::{
    Architecture, BinaryFormat, Endianness, RelocationEncoding, RelocationFlags, RelocationKind,
    SectionKind, SymbolFlags, SymbolKind, SymbolScope,
};
use std::collections::HashMap;
use std::path::Path;

use object::read::{File as ObjectFile, Object as _, ObjectSection as _};

pub mod runtime;
pub use runtime::{run_file, run_object};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EbpfHelperMetadata {
    pub id: u32,
    pub name: String,
    pub symbol: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EbpfFormatMetadata {
    pub id: u32,
    pub format: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EbpfCallsiteMetadata {
    pub function: String,
    pub offset: u32,
    pub helper_id: u32,
    pub helper_symbol: String,
    pub format_id: Option<u32>,
    pub arg_count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EbpfObjectMetadata {
    pub abi: String,
    pub helpers: Vec<EbpfHelperMetadata>,
    pub formats: Vec<EbpfFormatMetadata>,
    pub callsites: Vec<EbpfCallsiteMetadata>,
}

pub fn emit_assembly(program: &LirProgram) -> Result<String> {
    validate_program(program)?;
    let mut emitter = EbpfEmitter::new(program);
    emitter.emit_program()
}

pub fn emit_object(program: &LirProgram) -> Result<Vec<u8>> {
    validate_program(program)?;
    let runtime_abi = RuntimeAbi::from_program(program);

    let mut object = Object::new(BinaryFormat::Elf, Architecture::Bpf, Endianness::Little);
    let license = object.add_section(Vec::new(), b"license".to_vec(), SectionKind::ReadOnlyData);
    object.append_section_data(license, b"GPL\0", 1);
    let abi_section = object.add_section(
        Vec::new(),
        b".fp.ebpf.abi".to_vec(),
        SectionKind::ReadOnlyData,
    );
    object.append_section_data(abi_section, runtime_abi.encode_abi().as_bytes(), 1);
    let helper_section = object.add_section(
        Vec::new(),
        b".fp.ebpf.helpers".to_vec(),
        SectionKind::ReadOnlyData,
    );
    object.append_section_data(helper_section, &runtime_abi.encode_helpers(), 4);
    if !runtime_abi.formats.is_empty() {
        let fmt_section = object.add_section(
            Vec::new(),
            b".fp.ebpf.fmt".to_vec(),
            SectionKind::ReadOnlyData,
        );
        object.append_section_data(fmt_section, &runtime_abi.encode_formats(), 4);
    }

    let mut helper_symbols = HashMap::new();
    for helper in runtime_abi.helper_symbols() {
        let symbol_id = object.add_symbol(Symbol {
            name: helper.symbol.as_bytes().to_vec(),
            value: 0,
            size: 0,
            kind: SymbolKind::Text,
            scope: SymbolScope::Dynamic,
            weak: false,
            section: SymbolSection::Undefined,
            flags: SymbolFlags::None,
        });
        helper_symbols.insert(helper.id, symbol_id);
    }

    let mut callsites = Vec::new();

    for function in selected_functions(program) {
        let mut emitter = MachineFunctionEmitter::new(function, &runtime_abi)?;
        let emitted = emitter.emit()?;
        let section = object.add_section(
            Vec::new(),
            format!("prog/{}", function.name).into_bytes(),
            SectionKind::Text,
        );
        object.append_section_data(section, &emitted.code, 8);
        object.add_symbol(Symbol {
            name: function.name.to_string().into_bytes(),
            value: 0,
            size: emitted.code.len() as u64,
            kind: SymbolKind::Text,
            scope: SymbolScope::Dynamic,
            weak: false,
            section: SymbolSection::Section(section),
            flags: SymbolFlags::None,
        });
        for callsite in &emitted.callsites {
            let symbol = helper_symbols
                .get(&callsite.helper_id)
                .copied()
                .ok_or_else(|| {
                    Error::from(format!(
                        "missing helper symbol for helper id {}",
                        callsite.helper_id
                    ))
                })?;
            object
                .add_relocation(
                    section,
                    Relocation {
                        offset: u64::from(callsite.offset),
                        symbol,
                        addend: 0,
                        flags: RelocationFlags::Generic {
                            kind: RelocationKind::Absolute,
                            encoding: RelocationEncoding::Generic,
                            size: 32,
                        },
                    },
                )
                .map_err(|err| Error::from(err.to_string()))?;
        }
        callsites.extend(emitted.callsites);
    }

    if !callsites.is_empty() {
        let call_section = object.add_section(
            Vec::new(),
            b".fp.ebpf.calls".to_vec(),
            SectionKind::ReadOnlyData,
        );
        object.append_section_data(call_section, &encode_callsites(&callsites, &runtime_abi), 4);
    }

    object.write().map_err(|err| Error::from(err.to_string()))
}

pub fn write_object(path: &Path, program: &LirProgram) -> Result<()> {
    let bytes = emit_object(program)?;
    std::fs::write(path, bytes).map_err(Error::from)
}

pub fn read_object_metadata(bytes: &[u8]) -> Result<EbpfObjectMetadata> {
    let file = ObjectFile::parse(bytes).map_err(|err| Error::from(err.to_string()))?;
    let abi = read_utf8_section(&file, ".fp.ebpf.abi")?.unwrap_or_default();
    let helpers = if let Some(data) = read_binary_section(&file, ".fp.ebpf.helpers")? {
        decode_helpers(data)?
    } else {
        Vec::new()
    };
    let formats = if let Some(data) = read_binary_section(&file, ".fp.ebpf.fmt")? {
        decode_formats(data)?
    } else {
        Vec::new()
    };
    let callsites = if let Some(data) = read_binary_section(&file, ".fp.ebpf.calls")? {
        decode_callsites(data)?
    } else {
        Vec::new()
    };

    Ok(EbpfObjectMetadata {
        abi,
        helpers,
        formats,
        callsites,
    })
}

pub fn validate_program(program: &LirProgram) -> Result<()> {
    let mut errors = Vec::new();

    if !program.globals.is_empty() {
        errors.push("globals are not supported by fp-ebpf yet".to_string());
    }

    for function in selected_functions(program) {
        validate_function(function, &mut errors);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(Error::from(errors.join("; ")))
    }
}

struct EbpfEmitter<'a> {
    program: &'a LirProgram,
}

struct RuntimeAbi {
    formats: Vec<String>,
    format_ids: HashMap<String, u32>,
}

#[derive(Clone, Copy)]
struct RuntimeHelper {
    id: u32,
    key: &'static str,
    symbol: &'static str,
}

#[derive(Clone)]
struct HelperCallsite {
    function: String,
    offset: u32,
    helper_id: u32,
    format_id: Option<u32>,
    arg_count: u32,
}

struct EmittedMachineFunction {
    code: Vec<u8>,
    callsites: Vec<HelperCallsite>,
}

impl RuntimeAbi {
    fn from_program(program: &LirProgram) -> Self {
        let mut formats = Vec::new();
        let mut format_ids = HashMap::new();

        for function in selected_functions(program) {
            for instruction in function
                .basic_blocks
                .iter()
                .flat_map(|block| block.instructions.iter())
            {
                if let LirInstructionKind::IntrinsicCall { kind, format, .. } = &instruction.kind {
                    if matches!(
                        kind,
                        fp_core::lir::LirIntrinsicKind::Print
                            | fp_core::lir::LirIntrinsicKind::Println
                    ) {
                        if !format_ids.contains_key(format) {
                            let id = formats.len() as u32;
                            formats.push(format.clone());
                            format_ids.insert(format.clone(), id);
                        }
                    }
                }
            }
        }

        Self {
            formats,
            format_ids,
        }
    }

    fn format_id(&self, format: &str) -> Result<u32> {
        self.format_ids
            .get(format)
            .copied()
            .ok_or_else(|| Error::from(format!("missing runtime format id for {:?}", format)))
    }

    fn helper_symbols(&self) -> &'static [RuntimeHelper] {
        RUNTIME_HELPERS
    }

    fn helper_name(&self, helper_id: u32) -> Result<&'static str> {
        RUNTIME_HELPERS
            .iter()
            .find(|helper| helper.id == helper_id)
            .map(|helper| helper.key)
            .ok_or_else(|| Error::from(format!("unknown runtime helper id {}", helper_id)))
    }

    fn helper_symbol(&self, helper_id: u32) -> Result<&'static str> {
        RUNTIME_HELPERS
            .iter()
            .find(|helper| helper.id == helper_id)
            .map(|helper| helper.symbol)
            .ok_or_else(|| Error::from(format!("unknown runtime helper id {}", helper_id)))
    }

    fn encode_abi(&self) -> String {
        let mut out = String::from("version=1\n");
        for helper in self.helper_symbols() {
            out.push_str(&format!(
                "helper.{}={}\nhelper.{}.symbol={}\n",
                helper.key, helper.id, helper.key, helper.symbol
            ));
        }
        out.push_str(&format!("format.count={}\n", self.formats.len()));
        out
    }

    fn encode_helpers(&self) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend_from_slice(&(self.helper_symbols().len() as u32).to_le_bytes());
        for helper in self.helper_symbols() {
            out.extend_from_slice(&helper.id.to_le_bytes());
            out.extend_from_slice(&(helper.key.len() as u32).to_le_bytes());
            out.extend_from_slice(helper.key.as_bytes());
            out.extend_from_slice(&(helper.symbol.len() as u32).to_le_bytes());
            out.extend_from_slice(helper.symbol.as_bytes());
        }
        out
    }

    fn encode_formats(&self) -> Vec<u8> {
        let mut out = Vec::new();
        for (id, format) in self.formats.iter().enumerate() {
            out.extend_from_slice(&(id as u32).to_le_bytes());
            out.extend_from_slice(&(format.len() as u32).to_le_bytes());
            out.extend_from_slice(format.as_bytes());
        }
        out
    }
}

impl<'a> EbpfEmitter<'a> {
    fn new(program: &'a LirProgram) -> Self {
        Self { program }
    }

    fn emit_program(&mut self) -> Result<String> {
        let runtime_abi = RuntimeAbi::from_program(self.program);
        let mut out = String::new();
        out.push_str("; FerroPhase eBPF backend (experimental)\n");
        out.push_str("; Artifact kind: textual eBPF assembly subset\n");
        out.push_str("; Supported today: scalar locals, arithmetic, comparisons, stack slots, branches, returns\n");
        out.push_str(
            "; Not implemented yet: maps/BTF, relocations, verifier-specific metadata\n\n",
        );

        for global in &self.program.globals {
            out.push_str(&format!(
                "; unsupported global @{} : {:?}\n",
                global.name, global.ty
            ));
        }
        if !self.program.globals.is_empty() {
            out.push('\n');
        }

        out.push_str("; Runtime ABI\n");
        for helper in runtime_abi.helper_symbols() {
            out.push_str(&format!(
                ";   helper.{}={} symbol={}\n",
                helper.key, helper.id, helper.symbol
            ));
        }
        for (index, format) in runtime_abi.formats.iter().enumerate() {
            out.push_str(&format!(";   format[{}]={:?}\n", index, format));
        }
        out.push('\n');

        for (index, function) in selected_functions(self.program).into_iter().enumerate() {
            if index > 0 {
                out.push('\n');
            }
            let mut function_emitter = TextFunctionEmitter::new(function, &runtime_abi)?;
            out.push_str(&function_emitter.emit()?);
        }

        out.push_str("\n; Original LIR\n");
        let pretty_lir = format!("{}", pretty(self.program, PrettyOptions::default()));
        for line in pretty_lir.lines() {
            out.push_str("; ");
            out.push_str(line);
            out.push('\n');
        }

        Ok(out)
    }
}

struct TextFunctionEmitter<'a> {
    function: &'a LirFunction,
    runtime_abi: &'a RuntimeAbi,
    frame: FrameLayout,
    register_types: HashMap<u32, LirType>,
    pointer_offsets: HashMap<u32, i16>,
    label_counter: u32,
}

impl<'a> TextFunctionEmitter<'a> {
    fn new(function: &'a LirFunction, runtime_abi: &'a RuntimeAbi) -> Result<Self> {
        Ok(Self {
            function,
            runtime_abi,
            frame: FrameLayout::build(function)?,
            register_types: collect_register_types(function),
            pointer_offsets: HashMap::new(),
            label_counter: 0,
        })
    }

    fn emit(&mut self) -> Result<String> {
        let mut out = String::new();
        out.push_str(&format!(".section \"prog/{}\"\n", self.function.name));
        out.push_str(&format!(".globl {}\n", self.function.name));
        out.push_str(&format!("{}:\n", self.function.name));
        out.push_str("  ; r1-r5 carry arguments, r10 is frame pointer, r0 is return value\n");
        out.push_str(&format!("  ; frame_size={} bytes\n", self.frame.frame_size));

        if self.function.is_declaration {
            out.push_str("  r0 = 0\n");
            out.push_str("  exit\n");
            return Ok(out);
        }

        self.emit_prologue(&mut out)?;

        for block in &self.function.basic_blocks {
            out.push_str(&format!("LBB{}:\n", block.id));
            for instruction in &block.instructions {
                for line in self.emit_instruction(instruction)? {
                    out.push_str("  ");
                    out.push_str(&line);
                    out.push('\n');
                }
            }
            for line in self.emit_terminator(&block.terminator)? {
                out.push_str("  ");
                out.push_str(&line);
                out.push('\n');
            }
        }

        Ok(out)
    }

    fn emit_prologue(&self, out: &mut String) -> Result<()> {
        let mut arg_index = 0usize;
        for local in self
            .function
            .locals
            .iter()
            .filter(|local| local.is_argument)
        {
            let slot =
                self.frame.local_slots.get(&local.id).ok_or_else(|| {
                    Error::from(format!("missing frame slot for local {}", local.id))
                })?;
            out.push_str(&format!(
                "  {}\n",
                store_to_stack(arg_register(arg_index), slot.offset, &local.ty)
            ));
            arg_index += 1;
        }
        Ok(())
    }

    fn emit_instruction(&mut self, instruction: &LirInstruction) -> Result<Vec<String>> {
        use LirInstructionKind::*;

        let mut lines = Vec::new();
        match &instruction.kind {
            Add(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, "+=", &mut lines)?,
            Sub(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, "-=", &mut lines)?,
            Mul(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, "*=", &mut lines)?,
            Div(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, "/=", &mut lines)?,
            Rem(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, "%=", &mut lines)?,
            And(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, "&=", &mut lines)?,
            Or(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, "|=", &mut lines)?,
            Xor(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, "^=", &mut lines)?,
            Shl(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, "<<=", &mut lines)?,
            Shr(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, ">>=", &mut lines)?,
            Not(value) => {
                self.load_scalar("r6", value, &mut lines)?;
                lines.push("r6 ^= -1".to_string());
                self.store_register_result(
                    instruction.id,
                    "r6",
                    require_result_type(instruction)?,
                    &mut lines,
                )?;
            }
            Eq(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, "==", &mut lines)?,
            Ne(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, "!=", &mut lines)?,
            Lt(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, "<", &mut lines)?,
            Le(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, "<=", &mut lines)?,
            Gt(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, ">", &mut lines)?,
            Ge(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, ">=", &mut lines)?,
            Load { address, .. } => {
                let offset = self.resolve_stack_address(address)?;
                let ty = require_result_type(instruction)?;
                lines.push(load_from_stack("r6", offset, &ty));
                self.store_register_result(instruction.id, "r6", ty, &mut lines)?;
            }
            Store { value, address, .. } => {
                let offset = self.resolve_stack_address(address)?;
                self.load_scalar("r6", value, &mut lines)?;
                lines.push(store_to_stack("r6", offset, &self.value_type(value)?));
            }
            Alloca { .. } => {
                let offset = *self
                    .frame
                    .alloca_offsets
                    .get(&instruction.id)
                    .ok_or_else(|| {
                        Error::from(format!(
                            "missing alloca offset for register {}",
                            instruction.id
                        ))
                    })?;
                self.pointer_offsets.insert(instruction.id, offset);
                lines.push("r6 = r10".to_string());
                lines.push(format!("r6 += {}", offset));
                self.store_register_result(
                    instruction.id,
                    "r6",
                    require_result_type(instruction)?,
                    &mut lines,
                )?;
            }
            GetElementPtr { ptr, indices, .. } => {
                let mut offset = i32::from(self.resolve_stack_address(ptr)?);
                for index in indices {
                    match index {
                        LirValue::Constant(LirConstant::Int(value, _)) => offset += *value as i32,
                        LirValue::Constant(LirConstant::UInt(value, _)) => offset += *value as i32,
                        _ => unreachable!("validated earlier"),
                    }
                }
                let offset = i16::try_from(offset)
                    .map_err(|_| Error::from("stack offset exceeds i16 range"))?;
                self.pointer_offsets.insert(instruction.id, offset);
                lines.push("r6 = r10".to_string());
                lines.push(format!("r6 += {}", offset));
                self.store_register_result(
                    instruction.id,
                    "r6",
                    require_result_type(instruction)?,
                    &mut lines,
                )?;
            }
            PtrToInt(value)
            | IntToPtr(value)
            | Trunc(value, _)
            | ZExt(value, _)
            | SExt(value, _)
            | FPTrunc(value, _)
            | FPExt(value, _)
            | FPToUI(value, _)
            | FPToSI(value, _)
            | UIToFP(value, _)
            | SIToFP(value, _)
            | Bitcast(value, _)
            | SextOrTrunc(value, _)
            | Freeze(value) => {
                self.load_scalar("r6", value, &mut lines)?;
                self.store_register_result(
                    instruction.id,
                    "r6",
                    require_result_type(instruction)?,
                    &mut lines,
                )?;
            }
            IntrinsicCall { kind, format, args } => {
                self.emit_intrinsic(instruction, kind, format, args, &mut lines)?;
            }
            _ => unreachable!("validated earlier"),
        }
        Ok(lines)
    }

    fn emit_intrinsic(
        &mut self,
        instruction: &LirInstruction,
        kind: &fp_core::lir::LirIntrinsicKind,
        format: &str,
        args: &[LirValue],
        lines: &mut Vec<String>,
    ) -> Result<()> {
        match kind {
            fp_core::lir::LirIntrinsicKind::TimeNow => {
                lines.push(format!(
                    "call helper {} ; {} ({})",
                    HELPER_ID_TIME_NOW,
                    self.runtime_abi.helper_name(HELPER_ID_TIME_NOW)?,
                    self.runtime_abi.helper_symbol(HELPER_ID_TIME_NOW)?
                ));
                self.store_register_result(
                    instruction.id,
                    "r0",
                    require_result_type(instruction)?,
                    lines,
                )?;
            }
            fp_core::lir::LirIntrinsicKind::Print | fp_core::lir::LirIntrinsicKind::Println => {
                let helper = if matches!(kind, fp_core::lir::LirIntrinsicKind::Println) {
                    HELPER_ID_PRINTLN
                } else {
                    HELPER_ID_PRINT
                };
                let format_id = self.runtime_abi.format_id(format)?;
                lines.push(format!("r1 = {} ; format_id", format_id));
                for (index, arg) in args.iter().enumerate() {
                    let reg = format!("r{}", index + 2);
                    self.load_scalar(&reg, arg, lines)?;
                }
                lines.push(format!(
                    "call helper {} ; {} ({})",
                    helper,
                    self.runtime_abi.helper_name(helper)?,
                    self.runtime_abi.helper_symbol(helper)?
                ));
            }
            fp_core::lir::LirIntrinsicKind::Format => unreachable!("validated earlier"),
        }
        Ok(())
    }

    fn emit_binary_op(
        &mut self,
        instruction: &LirInstruction,
        lhs: &LirValue,
        rhs: &LirValue,
        op: &str,
        lines: &mut Vec<String>,
    ) -> Result<()> {
        self.load_scalar("r6", lhs, lines)?;
        if let Some(immediate) = immediate_scalar(rhs)? {
            lines.push(format!("r6 {} {}", op, immediate));
        } else {
            self.load_scalar("r7", rhs, lines)?;
            lines.push(format!("r6 {} r7", op));
        }
        self.store_register_result(
            instruction.id,
            "r6",
            require_result_type(instruction)?,
            lines,
        )
    }

    fn emit_compare_result(
        &mut self,
        instruction: &LirInstruction,
        lhs: &LirValue,
        rhs: &LirValue,
        op: &str,
        lines: &mut Vec<String>,
    ) -> Result<()> {
        let true_label = self.next_label("cmp_true");
        let end_label = self.next_label("cmp_end");
        self.load_scalar("r6", lhs, lines)?;
        lines.push("r0 = 0".to_string());
        if let Some(immediate) = immediate_scalar(rhs)? {
            lines.push(format!("if r6 {} {} goto {}", op, immediate, true_label));
        } else {
            self.load_scalar("r7", rhs, lines)?;
            lines.push(format!("if r6 {} r7 goto {}", op, true_label));
        }
        lines.push(format!("goto {}", end_label));
        lines.push(format!("{}:", true_label));
        lines.push("r0 = 1".to_string());
        lines.push(format!("{}:", end_label));
        self.store_register_result(instruction.id, "r0", LirType::I1, lines)
    }

    fn emit_terminator(&mut self, terminator: &LirTerminator) -> Result<Vec<String>> {
        let mut lines = Vec::new();
        match terminator {
            LirTerminator::Return(Some(value)) => {
                self.load_scalar("r0", value, &mut lines)?;
                lines.push("exit".to_string());
            }
            LirTerminator::Return(None) => {
                lines.push("r0 = 0".to_string());
                lines.push("exit".to_string());
            }
            LirTerminator::Br(target) => lines.push(format!("goto LBB{}", target)),
            LirTerminator::CondBr {
                condition,
                if_true,
                if_false,
            } => {
                self.load_scalar("r6", condition, &mut lines)?;
                lines.push(format!("if r6 != 0 goto LBB{}", if_true));
                lines.push(format!("goto LBB{}", if_false));
            }
            LirTerminator::Switch {
                value,
                default,
                cases,
            } => {
                self.load_scalar("r6", value, &mut lines)?;
                for (case_value, target) in cases {
                    lines.push(format!("if r6 == {} goto LBB{}", case_value, target));
                }
                lines.push(format!("goto LBB{}", default));
            }
            _ => unreachable!("validated earlier"),
        }
        Ok(lines)
    }

    fn load_scalar(
        &mut self,
        register: &str,
        value: &LirValue,
        lines: &mut Vec<String>,
    ) -> Result<()> {
        match value {
            LirValue::Constant(constant) => {
                lines.push(format!("{} = {}", register, constant_scalar(constant)?))
            }
            LirValue::Register(id) => {
                let slot = self.frame.register_slots.get(id).ok_or_else(|| {
                    Error::from(format!("missing frame slot for register {}", id))
                })?;
                lines.push(load_from_stack(register, slot.offset, &slot.ty));
            }
            LirValue::Local(id) => {
                let slot =
                    self.frame.local_slots.get(id).ok_or_else(|| {
                        Error::from(format!("missing frame slot for local {}", id))
                    })?;
                lines.push(load_from_stack(register, slot.offset, &slot.ty));
            }
            LirValue::StackSlot(id) => {
                let slot = self.frame.stack_slots.get(id).ok_or_else(|| {
                    Error::from(format!("missing frame slot for stack slot {}", id))
                })?;
                lines.push("r9 = r10".to_string());
                lines.push(format!("r9 += {}", slot.offset));
                if register != "r9" {
                    lines.push(format!("{} = r9", register));
                }
            }
            LirValue::Null(_) | LirValue::Undef(_) => lines.push(format!("{} = 0", register)),
            _ => unreachable!("validated earlier"),
        }
        Ok(())
    }

    fn resolve_stack_address(&self, value: &LirValue) -> Result<i16> {
        match value {
            LirValue::StackSlot(id) => self
                .frame
                .stack_slots
                .get(id)
                .map(|slot| slot.offset)
                .ok_or_else(|| Error::from(format!("missing frame slot for stack slot {}", id))),
            LirValue::Register(id) => self.pointer_offsets.get(id).copied().ok_or_else(|| {
                Error::from(format!(
                    "register {} is not known to carry a stack-backed pointer in fp-ebpf",
                    id
                ))
            }),
            LirValue::Local(id) => self
                .frame
                .local_slots
                .get(id)
                .map(|slot| slot.offset)
                .ok_or_else(|| Error::from(format!("missing frame slot for local {}", id))),
            _ => unreachable!("validated earlier"),
        }
    }

    fn store_register_result(
        &self,
        register_id: u32,
        source_register: &str,
        ty: LirType,
        lines: &mut Vec<String>,
    ) -> Result<()> {
        let slot = self.frame.register_slots.get(&register_id).ok_or_else(|| {
            Error::from(format!("missing frame slot for register {}", register_id))
        })?;
        lines.push(store_to_stack(source_register, slot.offset, &ty));
        Ok(())
    }

    fn value_type(&self, value: &LirValue) -> Result<LirType> {
        match value {
            LirValue::Constant(constant) => Ok(constant_type(constant)),
            LirValue::Register(id) => self
                .register_types
                .get(id)
                .cloned()
                .ok_or_else(|| Error::from(format!("missing type for register {}", id))),
            LirValue::Local(id) => self
                .function
                .locals
                .iter()
                .find(|local| local.id == *id)
                .map(|local| local.ty.clone())
                .ok_or_else(|| Error::from(format!("missing type for local {}", id))),
            LirValue::StackSlot(id) => self
                .frame
                .stack_slots
                .get(id)
                .map(|slot| LirType::Ptr(Box::new(slot.ty.clone())))
                .ok_or_else(|| Error::from(format!("missing type for stack slot {}", id))),
            LirValue::Null(ty) | LirValue::Undef(ty) => Ok(ty.clone()),
            _ => unreachable!("validated earlier"),
        }
    }

    fn next_label(&mut self, prefix: &str) -> String {
        let label = format!(".L{}_{}_{}", self.function.name, prefix, self.label_counter);
        self.label_counter += 1;
        label
    }
}

struct MachineFunctionEmitter<'a> {
    function: &'a LirFunction,
    runtime_abi: &'a RuntimeAbi,
    frame: FrameLayout,
    register_types: HashMap<u32, LirType>,
    pointer_offsets: HashMap<u32, i16>,
    callsites: Vec<HelperCallsite>,
    asm: BpfAssembler,
}

impl<'a> MachineFunctionEmitter<'a> {
    fn new(function: &'a LirFunction, runtime_abi: &'a RuntimeAbi) -> Result<Self> {
        Ok(Self {
            function,
            runtime_abi,
            frame: FrameLayout::build(function)?,
            register_types: collect_register_types(function),
            pointer_offsets: HashMap::new(),
            callsites: Vec::new(),
            asm: BpfAssembler::default(),
        })
    }

    fn emit(&mut self) -> Result<EmittedMachineFunction> {
        if self.function.is_declaration {
            self.asm.mov_imm64(REG_R0, 0);
            self.asm.exit();
            return Ok(EmittedMachineFunction {
                code: std::mem::take(&mut self.asm).finish()?,
                callsites: std::mem::take(&mut self.callsites),
            });
        }

        let mut arg_index = 0usize;
        for local in self
            .function
            .locals
            .iter()
            .filter(|local| local.is_argument)
        {
            let slot =
                self.frame.local_slots.get(&local.id).ok_or_else(|| {
                    Error::from(format!("missing frame slot for local {}", local.id))
                })?;
            self.asm
                .store_stack(arg_register_index(arg_index), slot.offset, &local.ty);
            arg_index += 1;
        }

        for block in &self.function.basic_blocks {
            self.asm.bind(format!("LBB{}", block.id));
            for instruction in &block.instructions {
                self.emit_instruction(instruction)?;
            }
            self.emit_terminator(&block.terminator)?;
        }

        Ok(EmittedMachineFunction {
            code: std::mem::take(&mut self.asm).finish()?,
            callsites: std::mem::take(&mut self.callsites),
        })
    }

    fn emit_instruction(&mut self, instruction: &LirInstruction) -> Result<()> {
        use LirInstructionKind::*;

        match &instruction.kind {
            Add(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, AluOp::Add)?,
            Sub(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, AluOp::Sub)?,
            Mul(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, AluOp::Mul)?,
            Div(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, AluOp::Div)?,
            Rem(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, AluOp::Mod)?,
            And(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, AluOp::And)?,
            Or(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, AluOp::Or)?,
            Xor(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, AluOp::Xor)?,
            Shl(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, AluOp::Lsh)?,
            Shr(lhs, rhs) => self.emit_binary_op(instruction, lhs, rhs, AluOp::Rsh)?,
            Not(value) => {
                self.load_scalar(REG_R6, value)?;
                self.asm.mov_imm64(REG_R7, -1);
                self.asm.alu64_reg(AluOp::Xor, REG_R6, REG_R7);
                self.store_register_result(
                    instruction.id,
                    REG_R6,
                    require_result_type(instruction)?,
                )?;
            }
            Eq(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, JumpOp::Jeq)?,
            Ne(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, JumpOp::Jne)?,
            Lt(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, JumpOp::Jlt)?,
            Le(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, JumpOp::Jle)?,
            Gt(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, JumpOp::Jgt)?,
            Ge(lhs, rhs) => self.emit_compare_result(instruction, lhs, rhs, JumpOp::Jge)?,
            Load { address, .. } => {
                let offset = self.resolve_stack_address(address)?;
                let ty = require_result_type(instruction)?;
                self.asm.load_stack(REG_R6, offset, &ty);
                self.store_register_result(instruction.id, REG_R6, ty)?;
            }
            Store { value, address, .. } => {
                let offset = self.resolve_stack_address(address)?;
                self.load_scalar(REG_R6, value)?;
                self.asm
                    .store_stack(REG_R6, offset, &self.value_type(value)?);
            }
            Alloca { .. } => {
                let offset = *self
                    .frame
                    .alloca_offsets
                    .get(&instruction.id)
                    .ok_or_else(|| {
                        Error::from(format!(
                            "missing alloca offset for register {}",
                            instruction.id
                        ))
                    })?;
                self.pointer_offsets.insert(instruction.id, offset);
                self.asm.mov_reg(REG_R6, REG_FP);
                self.asm.add_imm(REG_R6, i64::from(offset));
                self.store_register_result(
                    instruction.id,
                    REG_R6,
                    require_result_type(instruction)?,
                )?;
            }
            GetElementPtr { ptr, indices, .. } => {
                let mut offset = i32::from(self.resolve_stack_address(ptr)?);
                for index in indices {
                    match index {
                        LirValue::Constant(LirConstant::Int(value, _)) => offset += *value as i32,
                        LirValue::Constant(LirConstant::UInt(value, _)) => offset += *value as i32,
                        _ => unreachable!("validated earlier"),
                    }
                }
                let offset = i16::try_from(offset)
                    .map_err(|_| Error::from("stack offset exceeds i16 range"))?;
                self.pointer_offsets.insert(instruction.id, offset);
                self.asm.mov_reg(REG_R6, REG_FP);
                self.asm.add_imm(REG_R6, i64::from(offset));
                self.store_register_result(
                    instruction.id,
                    REG_R6,
                    require_result_type(instruction)?,
                )?;
            }
            PtrToInt(value)
            | IntToPtr(value)
            | Trunc(value, _)
            | ZExt(value, _)
            | SExt(value, _)
            | FPTrunc(value, _)
            | FPExt(value, _)
            | FPToUI(value, _)
            | FPToSI(value, _)
            | UIToFP(value, _)
            | SIToFP(value, _)
            | Bitcast(value, _)
            | SextOrTrunc(value, _)
            | Freeze(value) => {
                self.load_scalar(REG_R6, value)?;
                self.store_register_result(
                    instruction.id,
                    REG_R6,
                    require_result_type(instruction)?,
                )?;
            }
            IntrinsicCall { kind, format, args } => {
                self.emit_intrinsic(instruction, kind, format, args)?
            }
            _ => unreachable!("validated earlier"),
        }
        Ok(())
    }

    fn emit_intrinsic(
        &mut self,
        instruction: &LirInstruction,
        kind: &fp_core::lir::LirIntrinsicKind,
        format: &str,
        args: &[LirValue],
    ) -> Result<()> {
        match kind {
            fp_core::lir::LirIntrinsicKind::TimeNow => {
                let offset = self.asm.call(0);
                self.callsites.push(HelperCallsite {
                    function: self.function.name.to_string(),
                    offset,
                    helper_id: HELPER_ID_TIME_NOW,
                    format_id: None,
                    arg_count: 0,
                });
                self.store_register_result(
                    instruction.id,
                    REG_R0,
                    require_result_type(instruction)?,
                )?;
            }
            fp_core::lir::LirIntrinsicKind::Print | fp_core::lir::LirIntrinsicKind::Println => {
                let helper = if matches!(kind, fp_core::lir::LirIntrinsicKind::Println) {
                    HELPER_ID_PRINTLN
                } else {
                    HELPER_ID_PRINT
                };
                let format_id = self.runtime_abi.format_id(format)?;
                self.asm.mov_imm64(REG_R1, i64::from(format_id));
                for (index, arg) in args.iter().enumerate() {
                    self.load_scalar(arg_register_index(index + 1), arg)?;
                }
                let offset = self.asm.call(0);
                self.callsites.push(HelperCallsite {
                    function: self.function.name.to_string(),
                    offset,
                    helper_id: helper,
                    format_id: Some(format_id),
                    arg_count: args.len() as u32,
                });
            }
            fp_core::lir::LirIntrinsicKind::Format => unreachable!("validated earlier"),
        }
        Ok(())
    }

    fn emit_binary_op(
        &mut self,
        instruction: &LirInstruction,
        lhs: &LirValue,
        rhs: &LirValue,
        op: AluOp,
    ) -> Result<()> {
        self.load_scalar(REG_R6, lhs)?;
        if let Some(immediate) = immediate_scalar(rhs)? {
            self.asm
                .alu64_imm_or_reg(op, REG_R6, immediate, Some(REG_R7));
        } else {
            self.load_scalar(REG_R7, rhs)?;
            self.asm.alu64_reg(op, REG_R6, REG_R7);
        }
        self.store_register_result(instruction.id, REG_R6, require_result_type(instruction)?)
    }

    fn emit_compare_result(
        &mut self,
        instruction: &LirInstruction,
        lhs: &LirValue,
        rhs: &LirValue,
        op: JumpOp,
    ) -> Result<()> {
        let true_label = format!(".Lcmp_true_{}", instruction.id);
        let end_label = format!(".Lcmp_end_{}", instruction.id);
        self.load_scalar(REG_R6, lhs)?;
        self.asm.mov_imm64(REG_R0, 0);
        if let Some(immediate) = immediate_scalar(rhs)? {
            self.asm
                .jump_imm(op, REG_R6, immediate, true_label.clone(), Some(REG_R7));
        } else {
            self.load_scalar(REG_R7, rhs)?;
            self.asm.jump_reg(op, REG_R6, REG_R7, true_label.clone());
        }
        self.asm.jump(end_label.clone());
        self.asm.bind(true_label);
        self.asm.mov_imm64(REG_R0, 1);
        self.asm.bind(end_label);
        self.store_register_result(instruction.id, REG_R0, LirType::I1)
    }

    fn emit_terminator(&mut self, terminator: &LirTerminator) -> Result<()> {
        match terminator {
            LirTerminator::Return(Some(value)) => {
                self.load_scalar(REG_R0, value)?;
                self.asm.exit();
            }
            LirTerminator::Return(None) => {
                self.asm.mov_imm64(REG_R0, 0);
                self.asm.exit();
            }
            LirTerminator::Br(target) => self.asm.jump(format!("LBB{}", target)),
            LirTerminator::CondBr {
                condition,
                if_true,
                if_false,
            } => {
                self.load_scalar(REG_R6, condition)?;
                self.asm.jump_imm(
                    JumpOp::Jne,
                    REG_R6,
                    0,
                    format!("LBB{}", if_true),
                    Some(REG_R7),
                );
                self.asm.jump(format!("LBB{}", if_false));
            }
            LirTerminator::Switch {
                value,
                default,
                cases,
            } => {
                self.load_scalar(REG_R6, value)?;
                for (case_value, target) in cases {
                    self.asm.jump_imm(
                        JumpOp::Jeq,
                        REG_R6,
                        *case_value as i64,
                        format!("LBB{}", target),
                        Some(REG_R7),
                    );
                }
                self.asm.jump(format!("LBB{}", default));
            }
            _ => unreachable!("validated earlier"),
        }
        Ok(())
    }

    fn load_scalar(&mut self, register: u8, value: &LirValue) -> Result<()> {
        match value {
            LirValue::Constant(constant) => {
                self.asm.mov_imm64(register, constant_scalar(constant)?)
            }
            LirValue::Register(id) => {
                let slot = self.frame.register_slots.get(id).ok_or_else(|| {
                    Error::from(format!("missing frame slot for register {}", id))
                })?;
                self.asm.load_stack(register, slot.offset, &slot.ty);
            }
            LirValue::Local(id) => {
                let slot =
                    self.frame.local_slots.get(id).ok_or_else(|| {
                        Error::from(format!("missing frame slot for local {}", id))
                    })?;
                self.asm.load_stack(register, slot.offset, &slot.ty);
            }
            LirValue::StackSlot(id) => {
                let slot = self.frame.stack_slots.get(id).ok_or_else(|| {
                    Error::from(format!("missing frame slot for stack slot {}", id))
                })?;
                self.asm.mov_reg(register, REG_FP);
                self.asm.add_imm(register, i64::from(slot.offset));
            }
            LirValue::Null(_) | LirValue::Undef(_) => self.asm.mov_imm64(register, 0),
            _ => unreachable!("validated earlier"),
        }
        Ok(())
    }

    fn resolve_stack_address(&self, value: &LirValue) -> Result<i16> {
        match value {
            LirValue::StackSlot(id) => self
                .frame
                .stack_slots
                .get(id)
                .map(|slot| slot.offset)
                .ok_or_else(|| Error::from(format!("missing frame slot for stack slot {}", id))),
            LirValue::Register(id) => self.pointer_offsets.get(id).copied().ok_or_else(|| {
                Error::from(format!(
                    "register {} is not known to carry a stack-backed pointer in fp-ebpf",
                    id
                ))
            }),
            LirValue::Local(id) => self
                .frame
                .local_slots
                .get(id)
                .map(|slot| slot.offset)
                .ok_or_else(|| Error::from(format!("missing frame slot for local {}", id))),
            _ => unreachable!("validated earlier"),
        }
    }

    fn store_register_result(
        &mut self,
        register_id: u32,
        source_register: u8,
        ty: LirType,
    ) -> Result<()> {
        let slot = self.frame.register_slots.get(&register_id).ok_or_else(|| {
            Error::from(format!("missing frame slot for register {}", register_id))
        })?;
        self.asm.store_stack(source_register, slot.offset, &ty);
        Ok(())
    }

    fn value_type(&self, value: &LirValue) -> Result<LirType> {
        match value {
            LirValue::Constant(constant) => Ok(constant_type(constant)),
            LirValue::Register(id) => self
                .register_types
                .get(id)
                .cloned()
                .ok_or_else(|| Error::from(format!("missing type for register {}", id))),
            LirValue::Local(id) => self
                .function
                .locals
                .iter()
                .find(|local| local.id == *id)
                .map(|local| local.ty.clone())
                .ok_or_else(|| Error::from(format!("missing type for local {}", id))),
            LirValue::StackSlot(id) => self
                .frame
                .stack_slots
                .get(id)
                .map(|slot| LirType::Ptr(Box::new(slot.ty.clone())))
                .ok_or_else(|| Error::from(format!("missing type for stack slot {}", id))),
            LirValue::Null(ty) | LirValue::Undef(ty) => Ok(ty.clone()),
            _ => unreachable!("validated earlier"),
        }
    }
}

#[derive(Clone)]
struct FrameSlot {
    offset: i16,
    ty: LirType,
}

struct FrameLayout {
    frame_size: i32,
    local_slots: HashMap<u32, FrameSlot>,
    register_slots: HashMap<u32, FrameSlot>,
    stack_slots: HashMap<u32, FrameSlot>,
    alloca_offsets: HashMap<u32, i16>,
}

impl FrameLayout {
    fn build(function: &LirFunction) -> Result<Self> {
        let mut builder = FrameBuilder::default();
        let mut local_slots = HashMap::new();
        let mut register_slots = HashMap::new();
        let mut stack_slots = HashMap::new();
        let mut alloca_offsets = HashMap::new();

        for local in &function.locals {
            let offset = builder.allocate(
                typed_storage_size(&local.ty),
                typed_storage_align(&local.ty),
            )?;
            local_slots.insert(
                local.id,
                FrameSlot {
                    offset,
                    ty: local.ty.clone(),
                },
            );
        }

        for slot in &function.stack_slots {
            let offset = builder.allocate(slot.size.max(8), slot.alignment.max(8))?;
            stack_slots.insert(
                slot.id,
                FrameSlot {
                    offset,
                    ty: LirType::Array(Box::new(LirType::I8), slot.size as u64),
                },
            );
        }

        for instruction in function
            .basic_blocks
            .iter()
            .flat_map(|block| block.instructions.iter())
        {
            if let Some(result_ty) = instruction_result_type(instruction) {
                let offset = builder.allocate(
                    typed_storage_size(&result_ty),
                    typed_storage_align(&result_ty),
                )?;
                register_slots.insert(
                    instruction.id,
                    FrameSlot {
                        offset,
                        ty: result_ty,
                    },
                );
            }

            if let LirInstructionKind::Alloca { size, alignment } = &instruction.kind {
                let bytes = match size {
                    LirValue::Constant(LirConstant::Int(value, _)) if *value >= 0 => *value as u32,
                    LirValue::Constant(LirConstant::UInt(value, _)) => *value as u32,
                    _ => {
                        return Err(Error::from(
                            "eBPF alloca currently requires a constant non-negative integer size",
                        ));
                    }
                };
                let offset = builder.allocate(bytes.max(8), (*alignment).max(8))?;
                alloca_offsets.insert(instruction.id, offset);
            }
        }

        Ok(Self {
            frame_size: builder.frame_size(),
            local_slots,
            register_slots,
            stack_slots,
            alloca_offsets,
        })
    }
}

#[derive(Default)]
struct FrameBuilder {
    next_offset: i32,
}

impl FrameBuilder {
    fn allocate(&mut self, size: u32, align: u32) -> Result<i16> {
        let align = align.max(1) as i32;
        self.next_offset = align_up(self.next_offset, align);
        self.next_offset += size.max(1) as i32;
        let offset = -self.next_offset;
        i16::try_from(offset).map_err(|_| Error::from("eBPF stack offset exceeds i16 range"))
    }

    fn frame_size(&self) -> i32 {
        align_up(self.next_offset, 8)
    }
}

fn validate_function(function: &LirFunction, errors: &mut Vec<String>) {
    if function.signature.is_variadic {
        errors.push(format!(
            "function {}: variadic signatures are not supported",
            function.name
        ));
    }

    let arg_count = function
        .locals
        .iter()
        .filter(|local| local.is_argument)
        .count();
    if arg_count > 5 {
        errors.push(format!(
            "function {}: more than 5 arguments is not supported",
            function.name
        ));
    }

    if let Err(err) = validate_type(&function.signature.return_type) {
        errors.push(format!(
            "function {}: invalid return type: {}",
            function.name, err
        ));
    }

    for local in &function.locals {
        if let Err(err) = validate_type(&local.ty) {
            errors.push(format!(
                "function {} local {}: {}",
                function.name, local.id, err
            ));
        }
    }

    for slot in &function.stack_slots {
        if slot.size > 512 {
            errors.push(format!(
                "function {} stack slot {} exceeds 512 bytes",
                function.name, slot.id
            ));
        }
    }

    for block in &function.basic_blocks {
        for instruction in &block.instructions {
            validate_instruction(function, instruction, errors);
        }
        validate_terminator(function, &block.terminator, errors);
    }

    match FrameLayout::build(function) {
        Ok(layout) if layout.frame_size > 512 => errors.push(format!(
            "function {} requires {} bytes of stack, exceeds eBPF 512-byte limit",
            function.name, layout.frame_size
        )),
        Err(err) => errors.push(format!("function {}: {}", function.name, err)),
        _ => {}
    }
}

fn validate_type(ty: &LirType) -> Result<()> {
    match ty {
        LirType::I1
        | LirType::I8
        | LirType::I16
        | LirType::I32
        | LirType::I64
        | LirType::Ptr(_)
        | LirType::Void => Ok(()),
        _ => Err(Error::from(format!(
            "type {:?} is not supported by fp-ebpf",
            ty
        ))),
    }
}

fn validate_instruction(
    function: &LirFunction,
    instruction: &LirInstruction,
    errors: &mut Vec<String>,
) {
    use LirInstructionKind::*;

    let result = match &instruction.kind {
        Add(lhs, rhs)
        | Sub(lhs, rhs)
        | Mul(lhs, rhs)
        | Div(lhs, rhs)
        | Rem(lhs, rhs)
        | And(lhs, rhs)
        | Or(lhs, rhs)
        | Xor(lhs, rhs)
        | Shl(lhs, rhs)
        | Shr(lhs, rhs)
        | Eq(lhs, rhs)
        | Ne(lhs, rhs)
        | Lt(lhs, rhs)
        | Le(lhs, rhs)
        | Gt(lhs, rhs)
        | Ge(lhs, rhs) => validate_scalar_pair(lhs, rhs),
        Not(value)
        | PtrToInt(value)
        | IntToPtr(value)
        | Trunc(value, _)
        | ZExt(value, _)
        | SExt(value, _)
        | FPTrunc(value, _)
        | FPExt(value, _)
        | FPToUI(value, _)
        | FPToSI(value, _)
        | UIToFP(value, _)
        | SIToFP(value, _)
        | Bitcast(value, _)
        | SextOrTrunc(value, _)
        | Freeze(value) => validate_scalar_value(value),
        Load { address, .. } => validate_address_value(address),
        Store { value, address, .. } => {
            validate_scalar_value(value).and_then(|_| validate_address_value(address))
        }
        Alloca { size, .. } => match size {
            LirValue::Constant(LirConstant::Int(value, _)) if *value >= 0 => Ok(()),
            LirValue::Constant(LirConstant::UInt(_, _)) => Ok(()),
            _ => Err(Error::from(
                "alloca requires a constant non-negative integer size",
            )),
        },
        GetElementPtr { ptr, indices, .. } => validate_address_value(ptr).and_then(|_| {
            if indices.iter().all(|index| {
                matches!(
                    index,
                    LirValue::Constant(LirConstant::Int(_, _) | LirConstant::UInt(_, _))
                )
            }) {
                Ok(())
            } else {
                Err(Error::from(
                    "getelementptr requires constant integer indices",
                ))
            }
        }),
        Call { .. } => Err(Error::from("calls are not supported in fp-ebpf yet")),
        IntrinsicCall { kind, args, .. } => match kind {
            fp_core::lir::LirIntrinsicKind::TimeNow => {
                if args.is_empty() {
                    Ok(())
                } else {
                    Err(Error::from("TimeNow does not accept arguments"))
                }
            }
            fp_core::lir::LirIntrinsicKind::Print | fp_core::lir::LirIntrinsicKind::Println => {
                if args.len() > 4 {
                    Err(Error::from(
                        "print helpers support at most 4 scalar arguments",
                    ))
                } else {
                    for arg in args {
                        if let Err(err) = validate_scalar_value(arg) {
                            return errors.push(format!(
                                "function {} instruction {}: {}",
                                function.name, instruction.id, err
                            ));
                        }
                    }
                    Ok(())
                }
            }
            fp_core::lir::LirIntrinsicKind::Format => Err(Error::from(
                "Format is not supported by the current fp-ebpf runtime ABI",
            )),
        },
        ExtractValue { .. } | InsertValue { .. } => Err(Error::from(
            "aggregate operations are not supported in fp-ebpf",
        )),
        Phi { .. } => Err(Error::from("phi nodes must be lowered before fp-ebpf")),
        Select { .. } => Err(Error::from("select must be lowered before fp-ebpf")),
        InlineAsm { .. } => Err(Error::from("inline asm is not supported in fp-ebpf")),
        LandingPad { .. } | Unreachable => Err(Error::from(
            "exception/unreachable instructions are not supported in fp-ebpf",
        )),
    };

    if let Err(err) = result {
        errors.push(format!(
            "function {} instruction {}: {}",
            function.name, instruction.id, err
        ));
    }
}

fn validate_terminator(
    function: &LirFunction,
    terminator: &LirTerminator,
    errors: &mut Vec<String>,
) {
    let result = match terminator {
        LirTerminator::Return(Some(value)) => validate_scalar_value(value),
        LirTerminator::Return(None) | LirTerminator::Br(_) => Ok(()),
        LirTerminator::CondBr { condition, .. } => validate_scalar_value(condition),
        LirTerminator::Switch { value, .. } => validate_scalar_value(value),
        _ => Err(Error::from("terminator is not supported in fp-ebpf")),
    };

    if let Err(err) = result {
        errors.push(format!(
            "function {} terminator {:?}: {}",
            function.name, terminator, err
        ));
    }
}

fn validate_scalar_pair(lhs: &LirValue, rhs: &LirValue) -> Result<()> {
    validate_scalar_value(lhs)?;
    validate_scalar_value(rhs)
}

fn validate_scalar_value(value: &LirValue) -> Result<()> {
    match value {
        LirValue::Constant(LirConstant::Int(_, ty))
        | LirValue::Constant(LirConstant::UInt(_, ty))
        | LirValue::Constant(LirConstant::Null(ty))
        | LirValue::Constant(LirConstant::Undef(ty)) => validate_type(ty),
        LirValue::Constant(LirConstant::Bool(_)) => Ok(()),
        LirValue::Register(_)
        | LirValue::Local(_)
        | LirValue::StackSlot(_)
        | LirValue::Null(_)
        | LirValue::Undef(_) => Ok(()),
        _ => Err(Error::from(format!(
            "value {:?} is not a supported scalar fp-ebpf operand",
            value
        ))),
    }
}

fn validate_address_value(value: &LirValue) -> Result<()> {
    match value {
        LirValue::Register(_) | LirValue::Local(_) | LirValue::StackSlot(_) => Ok(()),
        _ => Err(Error::from(format!(
            "value {:?} is not a supported stack-backed address",
            value
        ))),
    }
}

fn selected_functions(program: &LirProgram) -> Vec<&LirFunction> {
    let main_functions = program
        .functions
        .iter()
        .filter(|function| !function.is_declaration && function.name.as_str() == "main")
        .collect::<Vec<_>>();
    if !main_functions.is_empty() {
        return main_functions;
    }

    program
        .functions
        .iter()
        .filter(|function| !function.is_declaration)
        .collect()
}

fn collect_register_types(function: &LirFunction) -> HashMap<u32, LirType> {
    let mut types = HashMap::new();
    for instruction in function
        .basic_blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
    {
        if let Some(ty) = instruction_result_type(instruction) {
            types.insert(instruction.id, ty);
        }
    }
    types
}

fn require_result_type(instruction: &LirInstruction) -> Result<LirType> {
    instruction_result_type(instruction).ok_or_else(|| {
        Error::from(format!(
            "instruction {} requires an explicit result type for fp-ebpf lowering",
            instruction.id
        ))
    })
}

fn instruction_result_type(instruction: &LirInstruction) -> Option<LirType> {
    use LirInstructionKind::*;

    if let Some(ty) = &instruction.type_hint {
        return Some(ty.clone());
    }

    match &instruction.kind {
        Eq(_, _) | Ne(_, _) | Lt(_, _) | Le(_, _) | Gt(_, _) | Ge(_, _) => Some(LirType::I1),
        Alloca { .. } | GetElementPtr { .. } | IntToPtr(_) => {
            Some(LirType::Ptr(Box::new(LirType::I8)))
        }
        PtrToInt(_) => Some(LirType::I64),
        _ => None,
    }
}

fn immediate_scalar(value: &LirValue) -> Result<Option<i64>> {
    match value {
        LirValue::Constant(constant) => Ok(Some(constant_scalar(constant)?)),
        LirValue::Null(_) => Ok(Some(0)),
        _ => Ok(None),
    }
}

fn constant_scalar(constant: &LirConstant) -> Result<i64> {
    match constant {
        LirConstant::Int(value, _) => Ok(*value),
        LirConstant::UInt(value, _) => Ok(*value as i64),
        LirConstant::Bool(value) => Ok(i64::from(*value)),
        LirConstant::Null(_) | LirConstant::Undef(_) => Ok(0),
        other => Err(Error::from(format!(
            "constant {:?} is not a scalar immediate supported by fp-ebpf",
            other
        ))),
    }
}

fn constant_type(constant: &LirConstant) -> LirType {
    match constant {
        LirConstant::Int(_, ty)
        | LirConstant::UInt(_, ty)
        | LirConstant::Float(_, ty)
        | LirConstant::Null(ty)
        | LirConstant::Undef(ty) => ty.clone(),
        LirConstant::Bool(_) => LirType::I1,
        LirConstant::String(value) => LirType::Array(Box::new(LirType::I8), value.len() as u64),
        LirConstant::Array(_, ty)
        | LirConstant::Struct(_, ty)
        | LirConstant::GlobalRef(_, ty, _) => ty.clone(),
        LirConstant::FunctionRef(_, ty) => ty.clone(),
    }
}

fn typed_storage_size(ty: &LirType) -> u32 {
    size_of(ty).max(8) as u32
}

fn typed_storage_align(ty: &LirType) -> u32 {
    align_of(ty).max(8)
}

fn width_suffix(ty: &LirType) -> &'static str {
    match size_of(ty) {
        0 | 1 => "u8",
        2 => "u16",
        4 => "u32",
        _ => "u64",
    }
}

fn store_to_stack(source_register: &str, offset: i16, ty: &LirType) -> String {
    format!(
        "*({} *)(r10 {:+}) = {}",
        width_suffix(ty),
        offset,
        source_register
    )
}

fn load_from_stack(target_register: &str, offset: i16, ty: &LirType) -> String {
    format!(
        "{} = *({} *)(r10 {:+})",
        target_register,
        width_suffix(ty),
        offset
    )
}

fn align_up(value: i32, align: i32) -> i32 {
    if align <= 1 {
        return value;
    }
    let rem = value % align;
    if rem == 0 {
        value
    } else {
        value + (align - rem)
    }
}

fn arg_register(index: usize) -> &'static str {
    match index {
        0 => "r1",
        1 => "r2",
        2 => "r3",
        3 => "r4",
        4 => "r5",
        _ => unreachable!("checked by caller"),
    }
}

fn arg_register_index(index: usize) -> u8 {
    match index {
        0 => REG_R1,
        1 => REG_R2,
        2 => REG_R3,
        3 => REG_R4,
        4 => REG_R5,
        _ => unreachable!("checked by caller"),
    }
}

const REG_R0: u8 = 0;
const REG_R1: u8 = 1;
const REG_R2: u8 = 2;
const REG_R3: u8 = 3;
const REG_R4: u8 = 4;
const REG_R5: u8 = 5;
const REG_R6: u8 = 6;
const REG_R7: u8 = 7;
const REG_FP: u8 = 10;

const HELPER_ID_TIME_NOW: u32 = 1;
const HELPER_ID_PRINT: u32 = 2;
const HELPER_ID_PRINTLN: u32 = 3;

const RUNTIME_HELPERS: &[RuntimeHelper] = &[
    RuntimeHelper {
        id: HELPER_ID_TIME_NOW,
        key: "time_now",
        symbol: "__fp_helper_time_now",
    },
    RuntimeHelper {
        id: HELPER_ID_PRINT,
        key: "print",
        symbol: "__fp_helper_print",
    },
    RuntimeHelper {
        id: HELPER_ID_PRINTLN,
        key: "println",
        symbol: "__fp_helper_println",
    },
];

const BPF_ALU64: u8 = 0x07;
const BPF_JMP: u8 = 0x05;
const BPF_LD: u8 = 0x00;
const BPF_LDX: u8 = 0x01;
const BPF_STX: u8 = 0x03;
const BPF_K: u8 = 0x00;
const BPF_X: u8 = 0x08;
const BPF_MEM: u8 = 0x60;
const BPF_DW: u8 = 0x18;
const BPF_W: u8 = 0x00;
const BPF_H: u8 = 0x08;
const BPF_B: u8 = 0x10;

#[derive(Clone, Copy)]
enum AluOp {
    Add = 0x00,
    Sub = 0x10,
    Mul = 0x20,
    Div = 0x30,
    Or = 0x40,
    And = 0x50,
    Lsh = 0x60,
    Rsh = 0x70,
    Mod = 0x90,
    Xor = 0xa0,
    Mov = 0xb0,
}

#[derive(Clone, Copy)]
enum JumpOp {
    Jeq = 0x10,
    Jgt = 0x20,
    Jge = 0x30,
    Jne = 0x50,
    Jlt = 0xa0,
    Jle = 0xb0,
}

#[derive(Clone, Copy)]
struct BpfInsn {
    code: u8,
    dst: u8,
    src: u8,
    off: i16,
    imm: i32,
}

#[derive(Default)]
struct BpfAssembler {
    insns: Vec<BpfInsn>,
    labels: HashMap<String, usize>,
    fixups: Vec<(usize, String)>,
}

impl BpfAssembler {
    fn bind(&mut self, label: String) {
        self.labels.insert(label, self.insns.len());
    }

    fn emit(&mut self, insn: BpfInsn) {
        self.insns.push(insn);
    }

    fn mov_reg(&mut self, dst: u8, src: u8) {
        self.emit(BpfInsn {
            code: BPF_ALU64 | AluOp::Mov as u8 | BPF_X,
            dst,
            src,
            off: 0,
            imm: 0,
        });
    }

    fn mov_imm64(&mut self, dst: u8, imm: i64) {
        if let Ok(imm32) = i32::try_from(imm) {
            self.emit(BpfInsn {
                code: BPF_ALU64 | AluOp::Mov as u8 | BPF_K,
                dst,
                src: 0,
                off: 0,
                imm: imm32,
            });
        } else {
            self.emit(BpfInsn {
                code: BPF_LD | BPF_DW,
                dst,
                src: 0,
                off: 0,
                imm: imm as i32,
            });
            self.emit(BpfInsn {
                code: 0,
                dst: 0,
                src: 0,
                off: 0,
                imm: (imm >> 32) as i32,
            });
        }
    }

    fn alu64_reg(&mut self, op: AluOp, dst: u8, src: u8) {
        self.emit(BpfInsn {
            code: BPF_ALU64 | op as u8 | BPF_X,
            dst,
            src,
            off: 0,
            imm: 0,
        });
    }

    fn alu64_imm_or_reg(&mut self, op: AluOp, dst: u8, imm: i64, scratch: Option<u8>) {
        if let Ok(imm32) = i32::try_from(imm) {
            self.emit(BpfInsn {
                code: BPF_ALU64 | op as u8 | BPF_K,
                dst,
                src: 0,
                off: 0,
                imm: imm32,
            });
        } else {
            let scratch = scratch.expect("scratch register required for wide immediate");
            self.mov_imm64(scratch, imm);
            self.alu64_reg(op, dst, scratch);
        }
    }

    fn add_imm(&mut self, dst: u8, imm: i64) {
        self.alu64_imm_or_reg(AluOp::Add, dst, imm, Some(REG_R7));
    }

    fn load_stack(&mut self, dst: u8, offset: i16, ty: &LirType) {
        self.emit(BpfInsn {
            code: BPF_LDX | BPF_MEM | size_code(ty),
            dst,
            src: REG_FP,
            off: offset,
            imm: 0,
        });
    }

    fn store_stack(&mut self, src: u8, offset: i16, ty: &LirType) {
        self.emit(BpfInsn {
            code: BPF_STX | BPF_MEM | size_code(ty),
            dst: REG_FP,
            src,
            off: offset,
            imm: 0,
        });
    }

    fn jump(&mut self, label: String) {
        let index = self.insns.len();
        self.emit(BpfInsn {
            code: BPF_JMP,
            dst: 0,
            src: 0,
            off: 0,
            imm: 0,
        });
        self.fixups.push((index, label));
    }

    fn jump_reg(&mut self, op: JumpOp, dst: u8, src: u8, label: String) {
        let index = self.insns.len();
        self.emit(BpfInsn {
            code: BPF_JMP | op as u8 | BPF_X,
            dst,
            src,
            off: 0,
            imm: 0,
        });
        self.fixups.push((index, label));
    }

    fn jump_imm(&mut self, op: JumpOp, dst: u8, imm: i64, label: String, scratch: Option<u8>) {
        if let Ok(imm32) = i32::try_from(imm) {
            let index = self.insns.len();
            self.emit(BpfInsn {
                code: BPF_JMP | op as u8 | BPF_K,
                dst,
                src: 0,
                off: 0,
                imm: imm32,
            });
            self.fixups.push((index, label));
        } else {
            let scratch = scratch.expect("scratch register required for wide jump immediate");
            self.mov_imm64(scratch, imm);
            self.jump_reg(op, dst, scratch, label);
        }
    }

    fn exit(&mut self) {
        self.emit(BpfInsn {
            code: BPF_JMP | 0x90,
            dst: 0,
            src: 0,
            off: 0,
            imm: 0,
        });
    }

    fn call(&mut self, imm: i32) -> u32 {
        let offset = (self.insns.len() as u32) * 8;
        self.emit(BpfInsn {
            code: BPF_JMP | 0x80,
            dst: 0,
            src: 0,
            off: 0,
            imm,
        });
        offset
    }

    fn finish(mut self) -> Result<Vec<u8>> {
        for (index, label) in self.fixups {
            let target = self
                .labels
                .get(&label)
                .copied()
                .ok_or_else(|| Error::from(format!("unknown eBPF label {}", label)))?;
            let rel = isize::try_from(target).unwrap() - isize::try_from(index).unwrap() - 1;
            self.insns[index].off = i16::try_from(rel)
                .map_err(|_| Error::from("eBPF jump offset exceeds i16 range"))?;
        }

        let mut out = Vec::with_capacity(self.insns.len() * 8);
        for insn in self.insns {
            out.push(insn.code);
            out.push((insn.dst & 0x0f) | ((insn.src & 0x0f) << 4));
            out.extend_from_slice(&insn.off.to_le_bytes());
            out.extend_from_slice(&insn.imm.to_le_bytes());
        }
        Ok(out)
    }
}

fn size_code(ty: &LirType) -> u8 {
    match size_of(ty) {
        0 | 1 => BPF_B,
        2 => BPF_H,
        4 => BPF_W,
        _ => BPF_DW,
    }
}

fn encode_callsites(callsites: &[HelperCallsite], runtime_abi: &RuntimeAbi) -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(&(callsites.len() as u32).to_le_bytes());
    for callsite in callsites {
        out.extend_from_slice(&(callsite.function.len() as u32).to_le_bytes());
        out.extend_from_slice(callsite.function.as_bytes());
        out.extend_from_slice(&callsite.offset.to_le_bytes());
        out.extend_from_slice(&callsite.helper_id.to_le_bytes());
        let helper_symbol = runtime_abi
            .helper_symbol(callsite.helper_id)
            .expect("known helper id for callsite");
        out.extend_from_slice(&(helper_symbol.len() as u32).to_le_bytes());
        out.extend_from_slice(helper_symbol.as_bytes());
        out.extend_from_slice(&callsite.format_id.unwrap_or(u32::MAX).to_le_bytes());
        out.extend_from_slice(&callsite.arg_count.to_le_bytes());
    }
    out
}

fn read_utf8_section(file: &ObjectFile<'_>, name: &str) -> Result<Option<String>> {
    let Some(section) = file.section_by_name(name) else {
        return Ok(None);
    };
    let data = section.data().map_err(|err| Error::from(err.to_string()))?;
    let text = std::str::from_utf8(data).map_err(|err| Error::from(err.to_string()))?;
    Ok(Some(text.to_string()))
}

fn read_binary_section<'a>(file: &'a ObjectFile<'a>, name: &str) -> Result<Option<&'a [u8]>> {
    let Some(section) = file.section_by_name(name) else {
        return Ok(None);
    };
    section
        .data()
        .map(Some)
        .map_err(|err| Error::from(err.to_string()))
}

fn decode_helpers(mut data: &[u8]) -> Result<Vec<EbpfHelperMetadata>> {
    let count = read_u32(&mut data)? as usize;
    let mut helpers = Vec::with_capacity(count);
    for _ in 0..count {
        let id = read_u32(&mut data)?;
        let name = read_string(&mut data)?;
        let symbol = read_string(&mut data)?;
        helpers.push(EbpfHelperMetadata { id, name, symbol });
    }
    ensure_section_consumed(data, ".fp.ebpf.helpers")?;
    Ok(helpers)
}

fn decode_formats(mut data: &[u8]) -> Result<Vec<EbpfFormatMetadata>> {
    let mut formats = Vec::new();
    while !data.is_empty() {
        let id = read_u32(&mut data)?;
        let format = read_string(&mut data)?;
        formats.push(EbpfFormatMetadata { id, format });
    }
    Ok(formats)
}

fn decode_callsites(mut data: &[u8]) -> Result<Vec<EbpfCallsiteMetadata>> {
    let count = read_u32(&mut data)? as usize;
    let mut callsites = Vec::with_capacity(count);
    for _ in 0..count {
        let function = read_string(&mut data)?;
        let offset = read_u32(&mut data)?;
        let helper_id = read_u32(&mut data)?;
        let helper_symbol = read_string(&mut data)?;
        let format_id = match read_u32(&mut data)? {
            u32::MAX => None,
            value => Some(value),
        };
        let arg_count = read_u32(&mut data)?;
        callsites.push(EbpfCallsiteMetadata {
            function,
            offset,
            helper_id,
            helper_symbol,
            format_id,
            arg_count,
        });
    }
    ensure_section_consumed(data, ".fp.ebpf.calls")?;
    Ok(callsites)
}

fn read_u32(data: &mut &[u8]) -> Result<u32> {
    if data.len() < 4 {
        return Err(Error::from("truncated eBPF metadata section"));
    }
    let (prefix, rest) = data.split_at(4);
    *data = rest;
    Ok(u32::from_le_bytes(prefix.try_into().expect("u32 width")))
}

fn read_string(data: &mut &[u8]) -> Result<String> {
    let len = read_u32(data)? as usize;
    if data.len() < len {
        return Err(Error::from("truncated eBPF metadata string"));
    }
    let (prefix, rest) = data.split_at(len);
    *data = rest;
    std::str::from_utf8(prefix)
        .map(|value| value.to_string())
        .map_err(|err| Error::from(err.to_string()))
}

fn ensure_section_consumed(data: &[u8], section: &str) -> Result<()> {
    if data.is_empty() {
        Ok(())
    } else {
        Err(Error::from(format!(
            "unexpected trailing bytes in {} metadata section",
            section
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::{emit_assembly, emit_object, read_object_metadata, validate_program};
    use fp_core::lir::{
        CallingConvention, Linkage, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature,
        LirInstruction, LirInstructionKind, LirLocal, LirProgram, LirTerminator, LirType, LirValue,
    };
    use object::read::{
        File, Object as _, ObjectSection as _, ObjectSymbol as _, RelocationTarget,
    };

    fn base_function(name: &str) -> LirFunction {
        LirFunction {
            name: fp_core::lir::Name::new(name),
            signature: LirFunctionSignature {
                params: vec![LirType::I64, LirType::I64],
                return_type: LirType::I64,
                is_variadic: false,
            },
            basic_blocks: Vec::new(),
            locals: vec![
                LirLocal {
                    id: 0,
                    ty: LirType::I64,
                    name: Some("lhs".to_string()),
                    is_argument: true,
                },
                LirLocal {
                    id: 1,
                    ty: LirType::I64,
                    name: Some("rhs".to_string()),
                    is_argument: true,
                },
            ],
            stack_slots: Vec::new(),
            calling_convention: CallingConvention::C,
            linkage: Linkage::External,
            is_declaration: false,
        }
    }

    fn addition_program() -> LirProgram {
        let mut function = base_function("main");
        function.basic_blocks.push(LirBasicBlock {
            id: 0,
            label: Some(fp_core::lir::Name::new("entry")),
            instructions: vec![LirInstruction {
                id: 1,
                kind: LirInstructionKind::Add(LirValue::Local(0), LirValue::Local(1)),
                type_hint: Some(LirType::I64),
                debug_info: None,
            }],
            terminator: LirTerminator::Return(Some(LirValue::Register(1))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        });

        LirProgram {
            functions: vec![function],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        }
    }

    #[test]
    fn emits_addition_with_argument_spills() {
        let assembly = emit_assembly(&addition_program()).unwrap();
        assert!(assembly.contains("*(u64 *)(r10 -8) = r1"));
        assert!(assembly.contains("*(u64 *)(r10 -16) = r2"));
        assert!(assembly.contains("r6 = *(u64 *)(r10 -8)"));
        assert!(assembly.contains("r7 = *(u64 *)(r10 -16)"));
        assert!(assembly.contains("r6 += r7"));
        assert!(assembly.contains("r0 = *(u64 *)(r10 -24)"));
        assert!(assembly.contains("exit"));
    }

    #[test]
    fn emits_compare_and_branch() {
        let mut function = base_function("branchy");
        function.basic_blocks = vec![
            LirBasicBlock {
                id: 0,
                label: Some(fp_core::lir::Name::new("entry")),
                instructions: vec![LirInstruction {
                    id: 1,
                    kind: LirInstructionKind::Eq(
                        LirValue::Local(0),
                        LirValue::Constant(LirConstant::Int(0, LirType::I64)),
                    ),
                    type_hint: Some(LirType::I1),
                    debug_info: None,
                }],
                terminator: LirTerminator::CondBr {
                    condition: LirValue::Register(1),
                    if_true: 1,
                    if_false: 2,
                },
                predecessors: Vec::new(),
                successors: vec![1, 2],
            },
            LirBasicBlock {
                id: 1,
                label: Some(fp_core::lir::Name::new("then")),
                instructions: Vec::new(),
                terminator: LirTerminator::Return(Some(LirValue::Constant(LirConstant::Int(
                    1,
                    LirType::I64,
                )))),
                predecessors: vec![0],
                successors: Vec::new(),
            },
            LirBasicBlock {
                id: 2,
                label: Some(fp_core::lir::Name::new("else")),
                instructions: Vec::new(),
                terminator: LirTerminator::Return(Some(LirValue::Constant(LirConstant::Int(
                    2,
                    LirType::I64,
                )))),
                predecessors: vec![0],
                successors: Vec::new(),
            },
        ];

        let program = LirProgram {
            functions: vec![function],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        };

        let assembly = emit_assembly(&program).unwrap();
        assert!(assembly.contains("if r6 == 0 goto .Lbranchy_cmp_true_0"));
        assert!(assembly.contains("if r6 != 0 goto LBB1"));
        assert!(assembly.contains("goto LBB2"));
    }

    #[test]
    fn validates_unsupported_call() {
        let mut function = base_function("bad");
        function.basic_blocks.push(LirBasicBlock {
            id: 0,
            label: None,
            instructions: vec![LirInstruction {
                id: 1,
                kind: LirInstructionKind::Call {
                    function: LirValue::Function("helper".to_string()),
                    args: Vec::new(),
                    calling_convention: CallingConvention::C,
                    tail_call: false,
                },
                type_hint: Some(LirType::I64),
                debug_info: None,
            }],
            terminator: LirTerminator::Return(None),
            predecessors: Vec::new(),
            successors: Vec::new(),
        });
        let program = LirProgram {
            functions: vec![function],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        };

        let err = validate_program(&program).unwrap_err().to_string();
        assert!(err.contains("calls are not supported"));
    }

    #[test]
    fn validates_format_intrinsic_as_unsupported() {
        let mut function = base_function("bad_format");
        function.basic_blocks.push(LirBasicBlock {
            id: 0,
            label: None,
            instructions: vec![LirInstruction {
                id: 1,
                kind: LirInstructionKind::IntrinsicCall {
                    kind: fp_core::lir::LirIntrinsicKind::Format,
                    format: "value={}".to_string(),
                    args: vec![LirValue::Constant(LirConstant::Int(1, LirType::I64))],
                },
                type_hint: Some(LirType::I64),
                debug_info: None,
            }],
            terminator: LirTerminator::Return(None),
            predecessors: Vec::new(),
            successors: Vec::new(),
        });
        let program = LirProgram {
            functions: vec![function],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        };

        let err = validate_program(&program).unwrap_err().to_string();
        assert!(err.contains("Format is not supported"));
    }

    #[test]
    fn emits_runtime_helper_metadata_sections() {
        let mut function = base_function("main");
        function.basic_blocks.push(LirBasicBlock {
            id: 0,
            label: Some(fp_core::lir::Name::new("entry")),
            instructions: vec![LirInstruction {
                id: 1,
                kind: LirInstructionKind::IntrinsicCall {
                    kind: fp_core::lir::LirIntrinsicKind::Println,
                    format: "value={}".to_string(),
                    args: vec![LirValue::Constant(LirConstant::Int(7, LirType::I64))],
                },
                type_hint: Some(LirType::Void),
                debug_info: None,
            }],
            terminator: LirTerminator::Return(Some(LirValue::Constant(LirConstant::Int(
                0,
                LirType::I64,
            )))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        });
        let program = LirProgram {
            functions: vec![function],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        };

        let bytes = emit_object(&program).unwrap();
        let metadata = read_object_metadata(&bytes).unwrap();
        let file = File::parse(&*bytes).unwrap();
        assert!(file.section_by_name(".fp.ebpf.abi").is_some());
        assert!(file.section_by_name(".fp.ebpf.helpers").is_some());
        assert!(file.section_by_name(".fp.ebpf.fmt").is_some());
        assert!(file.section_by_name(".fp.ebpf.calls").is_some());
        assert!(
            file.symbols()
                .any(|symbol| symbol.name() == Ok("__fp_helper_println"))
        );

        let abi = file
            .section_by_name(".fp.ebpf.abi")
            .unwrap()
            .data()
            .unwrap();
        let abi = std::str::from_utf8(abi).unwrap();
        assert!(abi.contains("helper.println=3"));
        assert!(abi.contains("helper.println.symbol=__fp_helper_println"));

        let calls = file
            .section_by_name(".fp.ebpf.calls")
            .unwrap()
            .data()
            .unwrap();
        assert!(calls.windows(b"main".len()).any(|window| window == b"main"));
        assert!(
            calls
                .windows(b"__fp_helper_println".len())
                .any(|window| window == b"__fp_helper_println")
        );
        assert_eq!(metadata.helpers.len(), 3);
        assert_eq!(metadata.formats.len(), 1);
        assert_eq!(metadata.callsites.len(), 1);
        assert_eq!(metadata.callsites[0].function, "main");
        assert_eq!(metadata.callsites[0].helper_symbol, "__fp_helper_println");

        let program_section = file.section_by_name("prog/main").unwrap();
        let relocations: Vec<_> = program_section.relocations().collect();
        assert_eq!(relocations.len(), 1);
        let (_, relocation) = &relocations[0];
        let RelocationTarget::Symbol(symbol_index) = relocation.target() else {
            panic!("expected symbol relocation");
        };
        let symbol = file.symbol_by_index(symbol_index).unwrap();
        assert_eq!(symbol.name(), Ok("__fp_helper_println"));

        let program_bytes = program_section.data().unwrap();
        let call_offset = metadata.callsites[0].offset as usize;
        assert_eq!(
            i32::from_le_bytes(
                program_bytes[call_offset + 4..call_offset + 8]
                    .try_into()
                    .unwrap()
            ),
            0
        );
    }

    #[test]
    fn emits_helper_metadata_in_text_assembly() {
        let mut function = base_function("main");
        function.basic_blocks.push(LirBasicBlock {
            id: 0,
            label: Some(fp_core::lir::Name::new("entry")),
            instructions: vec![LirInstruction {
                id: 1,
                kind: LirInstructionKind::IntrinsicCall {
                    kind: fp_core::lir::LirIntrinsicKind::TimeNow,
                    format: String::new(),
                    args: Vec::new(),
                },
                type_hint: Some(LirType::I64),
                debug_info: None,
            }],
            terminator: LirTerminator::Return(Some(LirValue::Register(1))),
            predecessors: Vec::new(),
            successors: Vec::new(),
        });
        let program = LirProgram {
            functions: vec![function],
            globals: Vec::new(),
            type_definitions: Vec::new(),
        };

        let assembly = emit_assembly(&program).unwrap();
        assert!(assembly.contains("helper.time_now=1 symbol=__fp_helper_time_now"));
        assert!(assembly.contains("call helper 1 ; time_now (__fp_helper_time_now)"));
    }

    #[test]
    fn emits_elf_object() {
        let bytes = emit_object(&addition_program()).unwrap();
        let file = File::parse(&*bytes).unwrap();
        assert!(file.section_by_name("prog/main").is_some());
        assert!(file.section_by_name("license").is_some());
        assert!(file.symbols().any(|symbol| symbol.name() == Ok("main")));
    }
}
