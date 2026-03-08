use crate::error::JvmError;
use crate::jir::{JvmClass, JvmCode, JvmInstr, JvmMethod, JvmProgram};
use std::collections::BTreeMap;

const ACC_PUBLIC: u16 = 0x0001;
const ACC_SUPER: u16 = 0x0020;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmittedClass {
    pub internal_name: String,
    pub bytes: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CpEntry {
    Utf8(String),
    Class(u16),
    NameAndType {
        name_index: u16,
        descriptor_index: u16,
    },
    Methodref {
        class_index: u16,
        name_and_type_index: u16,
    },
}

#[derive(Default)]
struct ConstantPool {
    entries: Vec<CpEntry>,
    utf8: BTreeMap<String, u16>,
    classes: BTreeMap<String, u16>,
    name_and_type: BTreeMap<(String, String), u16>,
    methodref: BTreeMap<(String, String, String), u16>,
}

impl ConstantPool {
    fn utf8(&mut self, value: &str) -> u16 {
        if let Some(index) = self.utf8.get(value) {
            return *index;
        }
        let index = self.push(CpEntry::Utf8(value.to_string()));
        self.utf8.insert(value.to_string(), index);
        index
    }

    fn class(&mut self, internal_name: &str) -> u16 {
        if let Some(index) = self.classes.get(internal_name) {
            return *index;
        }
        let name_index = self.utf8(internal_name);
        let index = self.push(CpEntry::Class(name_index));
        self.classes.insert(internal_name.to_string(), index);
        index
    }

    fn name_and_type(&mut self, name: &str, descriptor: &str) -> u16 {
        let key = (name.to_string(), descriptor.to_string());
        if let Some(index) = self.name_and_type.get(&key) {
            return *index;
        }
        let name_index = self.utf8(name);
        let descriptor_index = self.utf8(descriptor);
        let index = self.push(CpEntry::NameAndType {
            name_index,
            descriptor_index,
        });
        self.name_and_type.insert(key, index);
        index
    }

    fn methodref(&mut self, class: &str, name: &str, descriptor: &str) -> u16 {
        let key = (class.to_string(), name.to_string(), descriptor.to_string());
        if let Some(index) = self.methodref.get(&key) {
            return *index;
        }
        let class_index = self.class(class);
        let name_and_type_index = self.name_and_type(name, descriptor);
        let index = self.push(CpEntry::Methodref {
            class_index,
            name_and_type_index,
        });
        self.methodref.insert(key, index);
        index
    }

    fn push(&mut self, entry: CpEntry) -> u16 {
        self.entries.push(entry);
        self.entries.len() as u16
    }
}

pub fn emit_class_files(program: &JvmProgram) -> Result<Vec<EmittedClass>, JvmError> {
    Ok(vec![EmittedClass {
        internal_name: program.class.name.clone(),
        bytes: emit_class(&program.class)?,
    }])
}

fn emit_class(class: &JvmClass) -> Result<Vec<u8>, JvmError> {
    let mut cp = ConstantPool::default();
    let this_class_index = cp.class(&class.name);
    let super_class_index = cp.class(&class.super_name);
    let code_attr_index = cp.utf8("Code");

    for method in &class.methods {
        cp.utf8(&method.name);
        cp.utf8(&method.descriptor);
        for instr in &method.code.instructions {
            if let JvmInstr::Invokestatic {
                class,
                method,
                descriptor,
            } = instr
            {
                cp.methodref(class, method, descriptor);
            }
        }
    }

    let mut out = Vec::new();
    write_u32(&mut out, 0xCAFEBABE);
    write_u16(&mut out, 0);
    write_u16(&mut out, 52);
    write_u16(&mut out, (cp.entries.len() + 1) as u16);
    for entry in &cp.entries {
        write_cp_entry(&mut out, entry);
    }
    write_u16(&mut out, ACC_PUBLIC | ACC_SUPER);
    write_u16(&mut out, this_class_index);
    write_u16(&mut out, super_class_index);
    write_u16(&mut out, 0);
    write_u16(&mut out, 0);
    write_u16(&mut out, class.methods.len() as u16);

    for method in &class.methods {
        write_method(&mut out, method, code_attr_index, &mut cp)?;
    }

    write_u16(&mut out, 0);
    Ok(out)
}

fn write_method(
    out: &mut Vec<u8>,
    method: &JvmMethod,
    code_attr_index: u16,
    cp: &mut ConstantPool,
) -> Result<(), JvmError> {
    let name_index = cp.utf8(&method.name);
    let descriptor_index = cp.utf8(&method.descriptor);
    let code = assemble_code(&method.code, cp)?;

    write_u16(out, method.access_flags);
    write_u16(out, name_index);
    write_u16(out, descriptor_index);
    write_u16(out, 1);
    write_u16(out, code_attr_index);
    write_u32(out, (12 + code.len()) as u32);
    write_u16(out, method.code.max_stack);
    write_u16(out, method.code.max_locals);
    write_u32(out, code.len() as u32);
    out.extend_from_slice(&code);
    write_u16(out, 0);
    write_u16(out, 0);
    Ok(())
}

fn assemble_code(code: &JvmCode, cp: &mut ConstantPool) -> Result<Vec<u8>, JvmError> {
    let mut labels = BTreeMap::new();
    let mut offset = 0usize;
    for instr in &code.instructions {
        match instr {
            JvmInstr::Label(name) => {
                labels.insert(name.clone(), offset as i32);
            }
            _ => offset += instruction_len(instr),
        }
    }

    let mut out = Vec::new();
    let mut pc = 0i32;
    for instr in &code.instructions {
        match instr {
            JvmInstr::Label(_) => {}
            JvmInstr::AConstNull => {
                out.push(0x01);
                pc += 1;
            }
            JvmInstr::IConst(value) => {
                write_iconst(&mut out, *value);
                pc += instruction_len(instr) as i32;
            }
            JvmInstr::LConst0 => {
                out.push(0x09);
                pc += 1;
            }
            JvmInstr::ILoad(slot) => {
                write_var_op(&mut out, 0x15, *slot);
                pc += instruction_len(instr) as i32;
            }
            JvmInstr::IStore(slot) => {
                write_var_op(&mut out, 0x36, *slot);
                pc += instruction_len(instr) as i32;
            }
            JvmInstr::IAdd => {
                out.push(0x60);
                pc += 1;
            }
            JvmInstr::ISub => {
                out.push(0x64);
                pc += 1;
            }
            JvmInstr::IMul => {
                out.push(0x68);
                pc += 1;
            }
            JvmInstr::IDiv => {
                out.push(0x6c);
                pc += 1;
            }
            JvmInstr::Goto(label) => {
                out.push(0xa7);
                write_jump(&mut out, pc, 3, label, &labels)?;
                pc += 3;
            }
            JvmInstr::IfEq(label) => {
                out.push(0x99);
                write_jump(&mut out, pc, 3, label, &labels)?;
                pc += 3;
            }
            JvmInstr::IfNe(label) => {
                out.push(0x9a);
                write_jump(&mut out, pc, 3, label, &labels)?;
                pc += 3;
            }
            JvmInstr::Invokestatic {
                class,
                method,
                descriptor,
            } => {
                out.push(0xb8);
                write_u16(&mut out, cp.methodref(class, method, descriptor));
                pc += 3;
            }
            JvmInstr::Return => {
                out.push(0xb1);
                pc += 1;
            }
            JvmInstr::IReturn => {
                out.push(0xac);
                pc += 1;
            }
        }
    }
    Ok(out)
}

fn write_iconst(out: &mut Vec<u8>, value: i32) {
    match value {
        -1 => out.push(0x02),
        0 => out.push(0x03),
        1 => out.push(0x04),
        2 => out.push(0x05),
        3 => out.push(0x06),
        4 => out.push(0x07),
        5 => out.push(0x08),
        -128..=127 => {
            out.push(0x10);
            out.push(value as i8 as u8);
        }
        _ => {
            out.push(0x11);
            write_u16(out, value as i16 as u16);
        }
    }
}

fn write_var_op(out: &mut Vec<u8>, base: u8, slot: u16) {
    match (base, slot) {
        (0x15, 0) => out.push(0x1a),
        (0x15, 1) => out.push(0x1b),
        (0x15, 2) => out.push(0x1c),
        (0x15, 3) => out.push(0x1d),
        (0x36, 0) => out.push(0x3b),
        (0x36, 1) => out.push(0x3c),
        (0x36, 2) => out.push(0x3d),
        (0x36, 3) => out.push(0x3e),
        _ => {
            out.push(base);
            out.push(slot as u8);
        }
    }
}

fn write_jump(
    out: &mut Vec<u8>,
    pc: i32,
    len: i32,
    label: &str,
    labels: &BTreeMap<String, i32>,
) -> Result<(), JvmError> {
    let target = labels
        .get(label)
        .ok_or_else(|| JvmError::Lowering(format!("unknown label {label}")))?;
    let delta = *target - (pc + len);
    write_u16(out, delta as i16 as u16);
    Ok(())
}

fn instruction_len(instr: &JvmInstr) -> usize {
    match instr {
        JvmInstr::Label(_) => 0,
        JvmInstr::AConstNull => 1,
        JvmInstr::IConst(value) => match value {
            -1..=5 => 1,
            -128..=127 => 2,
            _ => 3,
        },
        JvmInstr::LConst0 => 1,
        JvmInstr::ILoad(slot) | JvmInstr::IStore(slot) => {
            if *slot <= 3 {
                1
            } else {
                2
            }
        }
        JvmInstr::IAdd | JvmInstr::ISub | JvmInstr::IMul | JvmInstr::IDiv => 1,
        JvmInstr::Goto(_)
        | JvmInstr::IfEq(_)
        | JvmInstr::IfNe(_)
        | JvmInstr::Invokestatic { .. } => 3,
        JvmInstr::Return | JvmInstr::IReturn => 1,
    }
}

fn write_cp_entry(out: &mut Vec<u8>, entry: &CpEntry) {
    match entry {
        CpEntry::Utf8(value) => {
            out.push(1);
            write_u16(out, value.len() as u16);
            out.extend_from_slice(value.as_bytes());
        }
        CpEntry::Class(index) => {
            out.push(7);
            write_u16(out, *index);
        }
        CpEntry::NameAndType {
            name_index,
            descriptor_index,
        } => {
            out.push(12);
            write_u16(out, *name_index);
            write_u16(out, *descriptor_index);
        }
        CpEntry::Methodref {
            class_index,
            name_and_type_index,
        } => {
            out.push(10);
            write_u16(out, *class_index);
            write_u16(out, *name_and_type_index);
        }
    }
}

fn write_u16(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_be_bytes());
}

fn write_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_be_bytes());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jir::{JvmClass, JvmCode, JvmInstr, JvmMethod, JvmProgram};

    #[test]
    fn emits_class_header() {
        let program = JvmProgram {
            class: JvmClass {
                name: "Main".to_string(),
                super_name: "java/lang/Object".to_string(),
                methods: vec![JvmMethod {
                    name: "main".to_string(),
                    descriptor: "()I".to_string(),
                    access_flags: 0x0009,
                    code: JvmCode {
                        max_stack: 1,
                        max_locals: 0,
                        instructions: vec![JvmInstr::IConst(1), JvmInstr::IReturn],
                    },
                }],
            },
        };

        let emitted = emit_class_files(&program).expect("emit class");
        assert_eq!(&emitted[0].bytes[0..4], &[0xCA, 0xFE, 0xBA, 0xBE]);
    }
}
