use crate::{EbpfCallsiteMetadata, EbpfObjectMetadata, read_object_metadata};
use fp_core::error::{Error, Result};
use object::ObjectSection;
use object::read::{File as ObjectFile, Object as _, ObjectSymbol as _, RelocationTarget};
use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

pub fn run_file(path: &Path, function: &str, stdout: &mut dyn Write) -> Result<i32> {
    let bytes = std::fs::read(path).map_err(Error::from)?;
    run_object(&bytes, function, stdout)
}

pub fn run_object(bytes: &[u8], function: &str, stdout: &mut dyn Write) -> Result<i32> {
    let metadata = read_object_metadata(bytes)?;
    let file = ObjectFile::parse(bytes).map_err(|err| Error::from(err.to_string()))?;
    let section_name = format!("prog/{function}");
    let section = file
        .section_by_name(&section_name)
        .ok_or_else(|| Error::from(format!("missing eBPF program section {section_name}")))?;
    let code = section.data().map_err(|err| Error::from(err.to_string()))?;
    let relocations = read_call_relocations(&file, &section)?;

    let mut runtime = Runtime::new(code, metadata, relocations, function, stdout)?;
    runtime.run()
}

struct Runtime<'a> {
    code: &'a [u8],
    registers: [u64; 11],
    stack: [u8; STACK_SIZE],
    callsites: HashMap<u32, EbpfCallsiteMetadata>,
    formats: HashMap<u32, String>,
    stdout: &'a mut dyn Write,
}

impl<'a> Runtime<'a> {
    fn new(
        code: &'a [u8],
        metadata: EbpfObjectMetadata,
        relocations: HashMap<u32, String>,
        function: &str,
        stdout: &'a mut dyn Write,
    ) -> Result<Self> {
        let callsites = metadata
            .callsites
            .into_iter()
            .filter(|callsite| callsite.function == function)
            .map(|mut callsite| {
                if let Some(symbol) = relocations.get(&callsite.offset) {
                    callsite.helper_symbol = symbol.clone();
                }
                (callsite.offset, callsite)
            })
            .collect();
        let formats = metadata
            .formats
            .into_iter()
            .map(|format| (format.id, format.format))
            .collect();
        let mut registers = [0u64; 11];
        registers[10] = STACK_SIZE as u64;
        Ok(Self {
            code,
            registers,
            stack: [0; STACK_SIZE],
            callsites,
            formats,
            stdout,
        })
    }

    fn run(&mut self) -> Result<i32> {
        let mut pc = 0usize;
        loop {
            let instruction = decode_instruction(self.code, pc)?;
            match instruction {
                DecodedInstruction::LoadImm64 { dst, imm, width } => {
                    self.registers[dst as usize] = imm as u64;
                    pc += width;
                }
                DecodedInstruction::Alu64Imm { op, dst, imm } => {
                    let value = apply_alu(op, self.registers[dst as usize], imm as u64)?;
                    self.registers[dst as usize] = value;
                    pc += 1;
                }
                DecodedInstruction::Alu64Reg { op, dst, src } => {
                    let rhs = self.registers[src as usize];
                    let value = apply_alu(op, self.registers[dst as usize], rhs)?;
                    self.registers[dst as usize] = value;
                    pc += 1;
                }
                DecodedInstruction::LoadStack { dst, offset, width } => {
                    self.registers[dst as usize] = self.read_stack(offset, width)?;
                    pc += 1;
                }
                DecodedInstruction::StoreStack { src, offset, width } => {
                    self.write_stack(offset, width, self.registers[src as usize])?;
                    pc += 1;
                }
                DecodedInstruction::Jump { offset } => {
                    pc = jump_target(pc, offset)?;
                }
                DecodedInstruction::JumpImm {
                    op,
                    dst,
                    imm,
                    offset,
                } => {
                    if compare_jump(op, self.registers[dst as usize], imm as u64) {
                        pc = jump_target(pc, offset)?;
                    } else {
                        pc += 1;
                    }
                }
                DecodedInstruction::JumpReg {
                    op,
                    dst,
                    src,
                    offset,
                } => {
                    if compare_jump(
                        op,
                        self.registers[dst as usize],
                        self.registers[src as usize],
                    ) {
                        pc = jump_target(pc, offset)?;
                    } else {
                        pc += 1;
                    }
                }
                DecodedInstruction::Call { byte_offset } => {
                    self.dispatch_call(byte_offset)?;
                    pc += 1;
                }
                DecodedInstruction::Exit => return Ok(self.registers[0] as i32),
            }
        }
    }

    fn dispatch_call(&mut self, byte_offset: u32) -> Result<()> {
        let callsite = self
            .callsites
            .get(&byte_offset)
            .ok_or_else(|| {
                Error::from(format!(
                    "missing helper metadata for callsite at byte offset {byte_offset}"
                ))
            })?
            .clone();

        match callsite.helper_symbol.as_str() {
            "__fp_helper_time_now" => {
                self.registers[0] = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .map_err(|err| Error::from(err.to_string()))?
                    .as_nanos() as u64;
            }
            "__fp_helper_print" | "__fp_helper_println" => {
                let format_id = callsite.format_id.ok_or_else(|| {
                    Error::from(format!(
                        "missing format id for helper call {} at byte offset {}",
                        callsite.helper_symbol, callsite.offset
                    ))
                })?;
                let format = self.formats.get(&format_id).ok_or_else(|| {
                    Error::from(format!(
                        "missing format string {} for helper call {}",
                        format_id, callsite.helper_symbol
                    ))
                })?;
                let rendered = render_format(
                    format,
                    &self.registers[2..(2 + callsite.arg_count as usize).min(6)],
                )?;
                write!(self.stdout, "{rendered}").map_err(Error::from)?;
                if callsite.helper_symbol == "__fp_helper_println" {
                    writeln!(self.stdout).map_err(Error::from)?;
                }
                self.registers[0] = 0;
            }
            other => {
                return Err(Error::from(format!(
                    "unsupported helper symbol {} at byte offset {}",
                    other, callsite.offset
                )));
            }
        }

        Ok(())
    }

    fn read_stack(&self, offset: i16, width: usize) -> Result<u64> {
        let base = stack_index(offset, width)?;
        let bytes = &self.stack[base..base + width];
        let value = match width {
            1 => u64::from(bytes[0]),
            2 => u64::from(u16::from_le_bytes(bytes.try_into().expect("u16 width"))),
            4 => u64::from(u32::from_le_bytes(bytes.try_into().expect("u32 width"))),
            8 => u64::from_le_bytes(bytes.try_into().expect("u64 width")),
            _ => return Err(Error::from(format!("unsupported stack read width {width}"))),
        };
        Ok(value)
    }

    fn write_stack(&mut self, offset: i16, width: usize, value: u64) -> Result<()> {
        let base = stack_index(offset, width)?;
        let bytes = &mut self.stack[base..base + width];
        match width {
            1 => bytes.copy_from_slice(&[value as u8]),
            2 => bytes.copy_from_slice(&(value as u16).to_le_bytes()),
            4 => bytes.copy_from_slice(&(value as u32).to_le_bytes()),
            8 => bytes.copy_from_slice(&value.to_le_bytes()),
            _ => {
                return Err(Error::from(format!(
                    "unsupported stack write width {width}"
                )));
            }
        }
        Ok(())
    }
}

fn read_call_relocations<'data, S>(
    file: &ObjectFile<'data>,
    section: &S,
) -> Result<HashMap<u32, String>>
where
    S: ObjectSection<'data>,
{
    let mut relocations = HashMap::new();
    for (offset, relocation) in section.relocations() {
        let RelocationTarget::Symbol(symbol_index) = relocation.target() else {
            continue;
        };
        let symbol = file
            .symbol_by_index(symbol_index)
            .map_err(|err| Error::from(err.to_string()))?;
        let name = symbol.name().map_err(|err| Error::from(err.to_string()))?;
        relocations.insert(offset as u32, name.to_string());
    }
    Ok(relocations)
}

fn render_format(template: &str, args: &[u64]) -> Result<String> {
    let mut out = String::new();
    let mut chars = template.chars().peekable();
    let mut index = 0usize;

    while let Some(ch) = chars.next() {
        match ch {
            '{' => match chars.peek() {
                Some('{') => {
                    chars.next();
                    out.push('{');
                }
                Some('}') => {
                    chars.next();
                    let value = args.get(index).copied().ok_or_else(|| {
                        Error::from(format!(
                            "missing argument {} for format {:?}",
                            index, template
                        ))
                    })?;
                    out.push_str(&value.to_string());
                    index += 1;
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported format template {:?}",
                        template
                    )));
                }
            },
            '}' => match chars.peek() {
                Some('}') => {
                    chars.next();
                    out.push('}');
                }
                _ => {
                    return Err(Error::from(format!(
                        "unsupported format template {:?}",
                        template
                    )));
                }
            },
            other => out.push(other),
        }
    }

    if index != args.len() {
        return Err(Error::from(format!(
            "unused arguments in format {:?}: expected {}, got {}",
            template,
            index,
            args.len()
        )));
    }

    Ok(out)
}

fn stack_index(offset: i16, width: usize) -> Result<usize> {
    let end = i32::try_from(STACK_SIZE).expect("stack size fits i32") + i32::from(offset);
    let start = end - i32::try_from(width).expect("width fits i32") + 1;
    if start < 0 || end < 0 || end as usize >= STACK_SIZE || start as usize >= STACK_SIZE {
        return Err(Error::from(format!(
            "stack access out of range: offset {}, width {}",
            offset, width
        )));
    }
    Ok(start as usize)
}

fn jump_target(pc: usize, offset: i16) -> Result<usize> {
    let target = i64::try_from(pc).expect("pc fits i64") + i64::from(offset) + 1;
    if target < 0 {
        return Err(Error::from("jump target before start of program"));
    }
    usize::try_from(target).map_err(|_| Error::from("jump target out of range"))
}

fn apply_alu(op: AluOp, lhs: u64, rhs: u64) -> Result<u64> {
    let value = match op {
        AluOp::Add => lhs.wrapping_add(rhs),
        AluOp::Sub => lhs.wrapping_sub(rhs),
        AluOp::Mul => lhs.wrapping_mul(rhs),
        AluOp::Div => {
            if rhs == 0 {
                return Err(Error::from("division by zero in eBPF runtime"));
            }
            lhs / rhs
        }
        AluOp::Or => lhs | rhs,
        AluOp::And => lhs & rhs,
        AluOp::Lsh => lhs.wrapping_shl(rhs as u32),
        AluOp::Rsh => lhs.wrapping_shr(rhs as u32),
        AluOp::Mod => {
            if rhs == 0 {
                return Err(Error::from("remainder by zero in eBPF runtime"));
            }
            lhs % rhs
        }
        AluOp::Xor => lhs ^ rhs,
        AluOp::Mov => rhs,
    };
    Ok(value)
}

fn compare_jump(op: JumpOp, lhs: u64, rhs: u64) -> bool {
    match op {
        JumpOp::Jeq => lhs == rhs,
        JumpOp::Jgt => lhs > rhs,
        JumpOp::Jge => lhs >= rhs,
        JumpOp::Jne => lhs != rhs,
        JumpOp::Jlt => lhs < rhs,
        JumpOp::Jle => lhs <= rhs,
    }
}

fn decode_instruction(code: &[u8], pc: usize) -> Result<DecodedInstruction> {
    let offset = pc * INSN_WIDTH;
    if code.len() < offset + INSN_WIDTH {
        return Err(Error::from(format!("instruction {} is out of bounds", pc)));
    }
    let insn = read_insn(&code[offset..offset + INSN_WIDTH]);
    let class = insn.code & 0x07;

    match class {
        BPF_LD if insn.code == (BPF_LD | BPF_DW) => {
            let next_offset = offset + INSN_WIDTH;
            if code.len() < next_offset + INSN_WIDTH {
                return Err(Error::from("truncated lddw instruction"));
            }
            let upper = read_insn(&code[next_offset..next_offset + INSN_WIDTH]);
            let imm = ((i64::from(upper.imm)) << 32) | i64::from(insn.imm as u32);
            Ok(DecodedInstruction::LoadImm64 {
                dst: insn.dst,
                imm,
                width: 2,
            })
        }
        BPF_LDX if (insn.code & BPF_MEM) == BPF_MEM => Ok(DecodedInstruction::LoadStack {
            dst: insn.dst,
            offset: insn.off,
            width: decode_width(insn.code)?,
        }),
        BPF_STX if (insn.code & BPF_MEM) == BPF_MEM => Ok(DecodedInstruction::StoreStack {
            src: insn.src,
            offset: insn.off,
            width: decode_width(insn.code)?,
        }),
        BPF_ALU64 => {
            let op = decode_alu_op(insn.code)?;
            if (insn.code & BPF_X) == BPF_X {
                Ok(DecodedInstruction::Alu64Reg {
                    op,
                    dst: insn.dst,
                    src: insn.src,
                })
            } else {
                Ok(DecodedInstruction::Alu64Imm {
                    op,
                    dst: insn.dst,
                    imm: insn.imm as i64,
                })
            }
        }
        BPF_JMP => decode_jump(insn, offset as u32),
        _ => Err(Error::from(format!(
            "unsupported eBPF opcode 0x{:02x} at pc {}",
            insn.code, pc
        ))),
    }
}

fn decode_jump(insn: RawInsn, byte_offset: u32) -> Result<DecodedInstruction> {
    let op = insn.code & 0xf0;
    match op {
        0x00 => Ok(DecodedInstruction::Jump { offset: insn.off }),
        0x10 | 0x20 | 0x30 | 0x50 | 0xa0 | 0xb0 => {
            let jump_op = decode_jump_op(insn.code)?;
            if (insn.code & BPF_X) == BPF_X {
                Ok(DecodedInstruction::JumpReg {
                    op: jump_op,
                    dst: insn.dst,
                    src: insn.src,
                    offset: insn.off,
                })
            } else {
                Ok(DecodedInstruction::JumpImm {
                    op: jump_op,
                    dst: insn.dst,
                    imm: insn.imm as i64,
                    offset: insn.off,
                })
            }
        }
        0x80 => Ok(DecodedInstruction::Call { byte_offset }),
        0x90 => Ok(DecodedInstruction::Exit),
        _ => Err(Error::from(format!(
            "unsupported jump opcode 0x{:02x} at byte offset {}",
            insn.code, byte_offset
        ))),
    }
}

fn decode_width(code: u8) -> Result<usize> {
    match code & 0x18 {
        0x00 => Ok(4),
        0x08 => Ok(2),
        0x10 => Ok(1),
        0x18 => Ok(8),
        value => Err(Error::from(format!(
            "unsupported load/store width 0x{value:02x}"
        ))),
    }
}

fn decode_alu_op(code: u8) -> Result<AluOp> {
    match code & 0xf0 {
        0x00 => Ok(AluOp::Add),
        0x10 => Ok(AluOp::Sub),
        0x20 => Ok(AluOp::Mul),
        0x30 => Ok(AluOp::Div),
        0x40 => Ok(AluOp::Or),
        0x50 => Ok(AluOp::And),
        0x60 => Ok(AluOp::Lsh),
        0x70 => Ok(AluOp::Rsh),
        0x90 => Ok(AluOp::Mod),
        0xa0 => Ok(AluOp::Xor),
        0xb0 => Ok(AluOp::Mov),
        value => Err(Error::from(format!("unsupported ALU op 0x{value:02x}"))),
    }
}

fn decode_jump_op(code: u8) -> Result<JumpOp> {
    match code & 0xf0 {
        0x10 => Ok(JumpOp::Jeq),
        0x20 => Ok(JumpOp::Jgt),
        0x30 => Ok(JumpOp::Jge),
        0x50 => Ok(JumpOp::Jne),
        0xa0 => Ok(JumpOp::Jlt),
        0xb0 => Ok(JumpOp::Jle),
        value => Err(Error::from(format!(
            "unsupported jump compare op 0x{value:02x}"
        ))),
    }
}

fn read_insn(bytes: &[u8]) -> RawInsn {
    RawInsn {
        code: bytes[0],
        dst: bytes[1] & 0x0f,
        src: (bytes[1] >> 4) & 0x0f,
        off: i16::from_le_bytes([bytes[2], bytes[3]]),
        imm: i32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]),
    }
}

#[derive(Clone, Copy)]
struct RawInsn {
    code: u8,
    dst: u8,
    src: u8,
    off: i16,
    imm: i32,
}

enum DecodedInstruction {
    LoadImm64 {
        dst: u8,
        imm: i64,
        width: usize,
    },
    Alu64Imm {
        op: AluOp,
        dst: u8,
        imm: i64,
    },
    Alu64Reg {
        op: AluOp,
        dst: u8,
        src: u8,
    },
    LoadStack {
        dst: u8,
        offset: i16,
        width: usize,
    },
    StoreStack {
        src: u8,
        offset: i16,
        width: usize,
    },
    Jump {
        offset: i16,
    },
    JumpImm {
        op: JumpOp,
        dst: u8,
        imm: i64,
        offset: i16,
    },
    JumpReg {
        op: JumpOp,
        dst: u8,
        src: u8,
        offset: i16,
    },
    Call {
        byte_offset: u32,
    },
    Exit,
}

#[derive(Clone, Copy)]
enum AluOp {
    Add,
    Sub,
    Mul,
    Div,
    Or,
    And,
    Lsh,
    Rsh,
    Mod,
    Xor,
    Mov,
}

#[derive(Clone, Copy)]
enum JumpOp {
    Jeq,
    Jgt,
    Jge,
    Jne,
    Jlt,
    Jle,
}

const STACK_SIZE: usize = 512;
const INSN_WIDTH: usize = 8;

const BPF_LD: u8 = 0x00;
const BPF_LDX: u8 = 0x01;
const BPF_STX: u8 = 0x03;
const BPF_JMP: u8 = 0x05;
const BPF_ALU64: u8 = 0x07;
const BPF_X: u8 = 0x08;
const BPF_MEM: u8 = 0x60;
const BPF_DW: u8 = 0x18;

#[cfg(test)]
mod tests {
    use super::run_object;
    use fp_core::lir::{
        CallingConvention, Linkage, LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature,
        LirInstruction, LirInstructionKind, LirLocal, LirProgram, LirTerminator, LirType, LirValue,
    };

    fn base_program(instructions: Vec<LirInstruction>, terminator: LirTerminator) -> LirProgram {
        LirProgram {
            functions: vec![LirFunction {
                name: fp_core::lir::Name::new("main"),
                signature: LirFunctionSignature {
                    params: Vec::new(),
                    return_type: LirType::I64,
                    is_variadic: false,
                },
                basic_blocks: vec![LirBasicBlock {
                    id: 0,
                    label: Some(fp_core::lir::Name::new("entry")),
                    instructions,
                    terminator,
                    predecessors: Vec::new(),
                    successors: Vec::new(),
                }],
                locals: Vec::<LirLocal>::new(),
                stack_slots: Vec::new(),
                calling_convention: CallingConvention::C,
                linkage: Linkage::External,
                is_declaration: false,
            }],
            globals: Vec::new(),
            type_definitions: Vec::new(),
            queries: Vec::new(),
        }
    }

    #[test]
    fn runs_println_via_metadata_runtime() {
        let program = base_program(
            vec![LirInstruction {
                id: 1,
                kind: LirInstructionKind::IntrinsicCall {
                    kind: fp_core::lir::LirIntrinsicKind::Println,
                    format: "value={}".to_string(),
                    args: vec![LirValue::Constant(LirConstant::Int(7, LirType::I64))],
                },
                type_hint: Some(LirType::Void),
                debug_info: None,
            }],
            LirTerminator::Return(Some(LirValue::Constant(LirConstant::Int(0, LirType::I64)))),
        );
        let bytes = crate::emit_object(&program).unwrap();
        let mut stdout = Vec::new();
        let code = run_object(&bytes, "main", &mut stdout).unwrap();
        assert_eq!(code, 0);
        assert_eq!(String::from_utf8(stdout).unwrap(), "value=7\n");
    }

    #[test]
    fn runs_arithmetic_program() {
        let program = base_program(
            vec![LirInstruction {
                id: 1,
                kind: LirInstructionKind::Add(
                    LirValue::Constant(LirConstant::Int(2, LirType::I64)),
                    LirValue::Constant(LirConstant::Int(3, LirType::I64)),
                ),
                type_hint: Some(LirType::I64),
                debug_info: None,
            }],
            LirTerminator::Return(Some(LirValue::Register(1))),
        );
        let bytes = crate::emit_object(&program).unwrap();
        let mut stdout = Vec::new();
        let code = run_object(&bytes, "main", &mut stdout).unwrap();
        assert_eq!(code, 5);
        assert!(stdout.is_empty());
    }
}
