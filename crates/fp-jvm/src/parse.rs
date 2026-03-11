use crate::error::JvmError;
use fp_core::lir::{
    LirBasicBlock, LirConstant, LirFunction, LirFunctionSignature, LirInstruction,
    LirInstructionKind, LirProgram, LirTerminator, LirType, LirValue, Name,
};

fn invalid(message: impl std::fmt::Display) -> JvmError {
    JvmError::InvalidClassFile(message.to_string())
}

/// Parse a JVM classfile emitted by `fp-jvm` and lower a narrow subset into `LirProgram`.
///
/// Scope:
/// - Intended for FerroPhase-emitted `.class` files.
/// - Supports a small integer/control-flow subset.
pub fn parse_class_to_lir(bytes: &[u8]) -> Result<LirProgram, JvmError> {
    let mut class = ClassReader::new(bytes)?;
    let methods = class.read_methods()?;

    let mut program = LirProgram::new();
    for method in methods {
        if method.name == "<init>" {
            continue;
        }
        if let Some(function) = lower_method_to_lir(&method)? {
            program.add_function(function);
        }
    }
    Ok(program)
}

struct ParsedMethod {
    name: String,
    descriptor: String,
    code: Vec<u8>,
    max_locals: u16,
}

fn lower_method_to_lir(method: &ParsedMethod) -> Result<Option<LirFunction>, JvmError> {
    // Only handle 0-arg integer-returning methods for now.
    if !method.descriptor.ends_with(")I") && !method.descriptor.ends_with(")V") {
        return Ok(None);
    }

    let mut stack: Vec<LirValue> = Vec::new();
    let mut next_reg: u32 = 0;
    let mut instructions = Vec::new();

    let mut pc = 0usize;
    while pc < method.code.len() {
        let op = method.code[pc];
        match op {
            0x01 => {
                // aconst_null
                stack.push(LirValue::Null(LirType::Ptr(Box::new(LirType::I8))));
                pc += 1;
            }
            0x03..=0x08 => {
                // iconst_0..iconst_5
                let value = (op - 0x03) as i64;
                stack.push(LirValue::Constant(LirConstant::Int(value, LirType::I64)));
                pc += 1;
            }
            0x10 => {
                // bipush
                let imm = method
                    .code
                    .get(pc + 1)
                    .copied()
                    .ok_or_else(|| invalid(format!("{}: truncated bipush", method.name)))? as i8;
                stack.push(LirValue::Constant(LirConstant::Int(imm as i64, LirType::I64)));
                pc += 2;
            }
            0x15 => {
                // iload
                let idx = method
                    .code
                    .get(pc + 1)
                    .copied()
                    .ok_or_else(|| invalid(format!("{}: truncated iload", method.name)))?;
                stack.push(LirValue::Local(idx as u32));
                pc += 2;
            }
            0x36 => {
                // istore
                let idx = method
                    .code
                    .get(pc + 1)
                    .copied()
                    .ok_or_else(|| invalid(format!("{}: truncated istore", method.name)))?;
                let value = stack
                    .pop()
                    .ok_or_else(|| invalid(format!("{}: istore missing value", method.name)))?;
                let id = next_reg;
                next_reg += 1;
                instructions.push(LirInstruction {
                    id,
                    kind: LirInstructionKind::Freeze(value),
                    type_hint: Some(LirType::I64),
                    debug_info: None,
                });
                // We don't model local assignment yet; keep local reads as LirValue::Local.
                let _ = idx;
                pc += 2;
            }
            0x60 | 0x64 | 0x68 | 0x6C => {
                // iadd/isub/imul/idiv
                let rhs = stack
                    .pop()
                    .ok_or_else(|| invalid(format!("{}: binary op missing rhs", method.name)))?;
                let lhs = stack
                    .pop()
                    .ok_or_else(|| invalid(format!("{}: binary op missing lhs", method.name)))?;
                let id = next_reg;
                next_reg += 1;
                let kind = match op {
                    0x60 => LirInstructionKind::Add(lhs, rhs),
                    0x64 => LirInstructionKind::Sub(lhs, rhs),
                    0x68 => LirInstructionKind::Mul(lhs, rhs),
                    _ => LirInstructionKind::Div(lhs, rhs),
                };
                instructions.push(LirInstruction {
                    id,
                    kind,
                    type_hint: Some(LirType::I64),
                    debug_info: None,
                });
                stack.push(LirValue::Register(id));
                pc += 1;
            }
            0xAC => {
                // ireturn
                let value = stack.pop();
                return Ok(Some(LirFunction {
                    name: Name::new(method.name.clone()),
                    signature: LirFunctionSignature {
                        params: Vec::new(),
                        return_type: LirType::I64,
                        is_variadic: false,
                    },
                    basic_blocks: vec![LirBasicBlock {
                        id: 0,
                        label: Some(Name::new("entry")),
                        instructions,
                        terminator: LirTerminator::Return(value),
                        predecessors: Vec::new(),
                        successors: Vec::new(),
                    }],
                    locals: (0..method.max_locals as u32)
                        .map(|id| fp_core::lir::LirLocal {
                            id,
                            ty: LirType::I64,
                            name: Some(format!("loc{id}")),
                            is_argument: false,
                        })
                        .collect(),
                    stack_slots: Vec::new(),
                    calling_convention: fp_core::lir::CallingConvention::C,
                    linkage: fp_core::lir::Linkage::External,
                    is_declaration: false,
                }));
            }
            0xB1 => {
                // return
                return Ok(Some(LirFunction {
                    name: Name::new(method.name.clone()),
                    signature: LirFunctionSignature {
                        params: Vec::new(),
                        return_type: LirType::Void,
                        is_variadic: false,
                    },
                    basic_blocks: vec![LirBasicBlock {
                        id: 0,
                        label: Some(Name::new("entry")),
                        instructions,
                        terminator: LirTerminator::Return(None),
                        predecessors: Vec::new(),
                        successors: Vec::new(),
                    }],
                    locals: Vec::new(),
                    stack_slots: Vec::new(),
                    calling_convention: fp_core::lir::CallingConvention::C,
                    linkage: fp_core::lir::Linkage::External,
                    is_declaration: false,
                }));
            }
            _ => {
                return Err(invalid(format!(
                    "{}: unsupported JVM opcode 0x{op:02x}",
                    method.name
                )));
            }
        }
    }

    Ok(None)
}

struct ClassReader<'a> {
    bytes: &'a [u8],
    offset: usize,
    constant_pool: Vec<CpEntry>,
}

#[derive(Debug, Clone)]
enum CpEntry {
    Utf8(String),
    Class { name_index: u16 },
    NameAndType { name_index: u16, descriptor_index: u16 },
    Methodref { class_index: u16, name_and_type_index: u16 },
    Unused,
}

impl<'a> ClassReader<'a> {
    fn new(bytes: &'a [u8]) -> Result<Self, JvmError> {
        if bytes.len() < 10 || &bytes[0..4] != b"\xCA\xFE\xBA\xBE" {
            return Err(invalid("class: missing CAFEBABE header"));
        }
        let mut reader = Self {
            bytes,
            offset: 4,
            constant_pool: Vec::new(),
        };
        let _minor = reader.read_u16()?;
        let _major = reader.read_u16()?;
        reader.read_constant_pool()?;
        Ok(reader)
    }

    fn read_methods(&mut self) -> Result<Vec<ParsedMethod>, JvmError> {
        let _access = self.read_u16()?;
        let _this_class = self.read_u16()?;
        let _super_class = self.read_u16()?;
        let interfaces_count = self.read_u16()?;
        for _ in 0..interfaces_count {
            let _ = self.read_u16()?;
        }
        let fields_count = self.read_u16()?;
        for _ in 0..fields_count {
            self.skip_member()?;
        }
        let methods_count = self.read_u16()?;
        let mut methods = Vec::new();
        for _ in 0..methods_count {
            let _access = self.read_u16()?;
            let name_index = self.read_u16()?;
            let desc_index = self.read_u16()?;
            let attributes_count = self.read_u16()?;
            let name = self.utf8(name_index)?;
            let descriptor = self.utf8(desc_index)?;

            let mut code = None;
            let mut max_locals = 0u16;
            for _ in 0..attributes_count {
                let attr_name_index = self.read_u16()?;
                let attr_len = self.read_u32()? as usize;
                let attr_name = self.utf8(attr_name_index)?;
                if attr_name == "Code" {
                    let _max_stack = self.read_u16()?;
                    max_locals = self.read_u16()?;
                    let code_len = self.read_u32()? as usize;
                    let code_bytes = self.read_bytes(code_len)?;
                    code = Some(code_bytes.to_vec());
                    let exception_table_len = self.read_u16()?;
                    for _ in 0..exception_table_len {
                        let _ = self.read_u16()?;
                        let _ = self.read_u16()?;
                        let _ = self.read_u16()?;
                        let _ = self.read_u16()?;
                    }
                    let sub_attr_count = self.read_u16()?;
                    for _ in 0..sub_attr_count {
                        let _ = self.read_u16()?;
                        let len = self.read_u32()? as usize;
                        self.skip(len)?;
                    }
                } else {
                    self.skip(attr_len)?;
                }
            }

            if let Some(code) = code {
                methods.push(ParsedMethod {
                    name,
                    descriptor,
                    code,
                    max_locals,
                });
            }
        }
        Ok(methods)
    }

    fn skip_member(&mut self) -> Result<(), JvmError> {
        let _access = self.read_u16()?;
        let _name = self.read_u16()?;
        let _desc = self.read_u16()?;
        let attributes = self.read_u16()?;
        for _ in 0..attributes {
            let _name = self.read_u16()?;
            let len = self.read_u32()? as usize;
            self.skip(len)?;
        }
        Ok(())
    }

    fn read_constant_pool(&mut self) -> Result<(), JvmError> {
        let count = self.read_u16()?;
        self.constant_pool = vec![CpEntry::Unused; count as usize];
        let mut index = 1u16;
        while index < count {
            let tag = self.read_u8()?;
            let entry = match tag {
                1 => {
                    let len = self.read_u16()? as usize;
                    let bytes = self.read_bytes(len)?;
                    let value = String::from_utf8_lossy(bytes).to_string();
                    CpEntry::Utf8(value)
                }
                7 => {
                    let name_index = self.read_u16()?;
                    CpEntry::Class { name_index }
                }
                12 => {
                    let name_index = self.read_u16()?;
                    let descriptor_index = self.read_u16()?;
                    CpEntry::NameAndType {
                        name_index,
                        descriptor_index,
                    }
                }
                10 => {
                    let class_index = self.read_u16()?;
                    let name_and_type_index = self.read_u16()?;
                    CpEntry::Methodref {
                        class_index,
                        name_and_type_index,
                    }
                }
                3 => {
                    let _ = self.read_u32()?;
                    CpEntry::Unused
                }
                5 | 6 => {
                    // long/double occupy two slots.
                    let _ = self.read_u32()?;
                    let _ = self.read_u32()?;
                    self.constant_pool[index as usize] = CpEntry::Unused;
                    index += 1;
                    CpEntry::Unused
                }
                other => {
                    return Err(invalid(format!("cp: unsupported constant pool tag {other}")))
                }
            };
            self.constant_pool[index as usize] = entry;
            index += 1;
        }
        Ok(())
    }

    fn utf8(&self, index: u16) -> Result<String, JvmError> {
        match self
            .constant_pool
            .get(index as usize)
            .ok_or_else(|| invalid("cp: bad constant pool index"))?
        {
            CpEntry::Utf8(value) => Ok(value.clone()),
            _ => Err(invalid("cp: expected Utf8")),
        }
    }

    fn read_u8(&mut self) -> Result<u8, JvmError> {
        let value = *self
            .bytes
            .get(self.offset)
            .ok_or_else(|| invalid("class: unexpected eof"))?;
        self.offset += 1;
        Ok(value)
    }

    fn read_u16(&mut self) -> Result<u16, JvmError> {
        let bytes = self.read_bytes(2)?;
        Ok(u16::from_be_bytes([bytes[0], bytes[1]]))
    }

    fn read_u32(&mut self) -> Result<u32, JvmError> {
        let bytes = self.read_bytes(4)?;
        Ok(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    fn read_bytes(&mut self, len: usize) -> Result<&'a [u8], JvmError> {
        let bytes = self
            .bytes
            .get(self.offset..self.offset + len)
            .ok_or_else(|| invalid("class: unexpected eof"))?;
        self.offset += len;
        Ok(bytes)
    }

    fn skip(&mut self, len: usize) -> Result<(), JvmError> {
        self.offset = self
            .offset
            .checked_add(len)
            .ok_or_else(|| invalid("class: offset overflow"))?;
        if self.offset > self.bytes.len() {
            return Err(invalid("class: unexpected eof"));
        }
        Ok(())
    }
}

