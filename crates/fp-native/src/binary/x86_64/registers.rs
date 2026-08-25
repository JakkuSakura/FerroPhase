use super::*;

pub(super) fn relocation_at<'a>(
    relocs: &'a [TextRelocation],
    offset: u64,
) -> Option<&'a TextRelocation> {
    relocs.iter().find(|reloc| reloc.offset == offset)
}

pub(super) fn relocation_is_external_call(reloc: &TextRelocation) -> bool {
    reloc.is_import
}

pub(super) fn base_symbol_name(name: &str) -> &str {
    name.split_once('@').map(|(head, _)| head).unwrap_or(name)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ExternalCallReturnModel {
    I32,
    I64,
}

pub(super) fn external_call_return_model(name: &str) -> Option<ExternalCallReturnModel> {
    Some(match base_symbol_name(name) {
        "dcgettext" | "dgettext" | "gettext" => ExternalCallReturnModel::I64,

        // getopt family returns an `int` in EAX.
        "getopt" | "getopt_long" | "getopt_long_only" => ExternalCallReturnModel::I32,

        // Common libc helpers that return pointers.
        "strchr" | "strrchr" | "memchr" | "strstr" | "strpbrk" => ExternalCallReturnModel::I64,
        _ => return None,
    })
}

pub(super) struct RegisterLiftContext {
    pub(super) locals: Vec<AsmLocal>,
    pub(super) locals_by_register: std::collections::HashMap<u8, u32>,
    pub(super) registers: std::collections::HashMap<u8, AsmValue>,
    pub(super) vec_locals_by_register: std::collections::HashMap<u8, u32>,
    pub(super) vec_registers: std::collections::HashMap<u8, AsmValue>,
    pub(super) x87_stack: Vec<AsmValue>,
    pub(super) next_local_id: u32,
    pub(super) code_base_address: u64,
    pub(super) rip_symbols: HashMap<u64, RipSymbol>,
    pub(super) plt_targets: HashMap<u64, String>,
    pub(super) rodata_cstrings: HashMap<String, String>,
    pub(super) rodata_cstrings_by_addr: HashMap<u64, String>,
    pub(super) data_regions: Vec<DataRegion>,
    pub(super) direct_call_targets: Vec<u64>,
    pub(super) gpr_slot_by_reg: std::collections::HashMap<u8, u32>,
    pub(super) pending_jump_table_index: std::collections::HashMap<u64, AsmValue>,
    pub(super) mark_sysv_args: bool,
    pub(super) use_lifted_regfile_calls: bool,
}

pub(super) fn synthesized_annotations(reason: &str) -> Vec<AsmAnnotation> {
    vec![AsmAnnotation {
        key: "fp.synthesized".to_string(),
        value: reason.to_string(),
    }]
}

impl RegisterLiftContext {
    pub(super) fn new(
        code_base_address: u64,
        rip_symbols: Option<&HashMap<u64, RipSymbol>>,
        plt_targets: Option<&HashMap<u64, String>>,
        rodata_cstrings: Option<&HashMap<String, String>>,
        rodata_cstrings_by_addr: Option<&HashMap<u64, String>>,
        data_regions: Option<&[DataRegion]>,
        mark_sysv_args: bool,
        use_lifted_regfile_calls: bool,
    ) -> Self {
        Self {
            locals: Vec::new(),
            locals_by_register: std::collections::HashMap::new(),
            registers: std::collections::HashMap::new(),
            vec_locals_by_register: std::collections::HashMap::new(),
            vec_registers: std::collections::HashMap::new(),
            x87_stack: Vec::new(),
            next_local_id: 0,
            code_base_address,
            rip_symbols: rip_symbols.cloned().unwrap_or_default(),
            plt_targets: plt_targets.cloned().unwrap_or_default(),
            rodata_cstrings: rodata_cstrings.cloned().unwrap_or_default(),
            rodata_cstrings_by_addr: rodata_cstrings_by_addr.cloned().unwrap_or_default(),
            data_regions: data_regions
                .map(|regions| regions.to_vec())
                .unwrap_or_default(),
            direct_call_targets: Vec::new(),
            gpr_slot_by_reg: std::collections::HashMap::new(),
            pending_jump_table_index: std::collections::HashMap::new(),
            mark_sysv_args,
            use_lifted_regfile_calls,
        }
    }

    pub(super) fn initialize_reg_file_slots(&mut self) -> Vec<fp_core::asmir::AsmStackSlot> {
        let mut slots = Vec::new();

        for reg in 0u8..=15 {
            let slot_id = u32::from(reg);
            self.gpr_slot_by_reg.insert(reg, slot_id);
            let is_argument = self.mark_sysv_args && matches!(reg, 7 | 6 | 2 | 1 | 8 | 9);
            self.ensure_local(reg, is_argument);
            slots.push(fp_core::asmir::AsmStackSlot {
                id: slot_id,
                size: 8,
                alignment: 8,
                name: Some(format!("x86.{}", reg_name(reg))),
            });
        }

        slots
    }

    pub(super) fn emit_reg_file_init_stores(
        &mut self,
        instructions: &mut Vec<AsmInstruction>,
        next_id: &mut u32,
    ) -> Result<()> {
        for reg in 0u8..=15 {
            let local_id = *self
                .locals_by_register
                .get(&reg)
                .ok_or_else(|| Error::from("missing x86 register local"))?;
            let slot_id = *self
                .gpr_slot_by_reg
                .get(&reg)
                .ok_or_else(|| Error::from("missing x86 register slot"))?;

            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value: AsmValue::Local(local_id),
                    address: AsmValue::StackSlot(slot_id),
                    alignment: Some(8),
                    volatile: false,
                },
                ty: AsmType::Void,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: synthesized_annotations("x86.regfile.init"),
            });
            *next_id += 1;
        }
        Ok(())
    }

    pub(super) fn begin_block(
        &mut self,
        instructions: &mut Vec<AsmInstruction>,
        next_id: &mut u32,
    ) -> Result<()> {
        self.registers.clear();
        self.vec_registers.clear();
        self.x87_stack.clear();
        self.pending_jump_table_index.clear();

        for reg in 0u8..=15 {
            let slot_id = *self
                .gpr_slot_by_reg
                .get(&reg)
                .ok_or_else(|| Error::from("missing x86 register slot"))?;
            let load_id = *next_id;
            instructions.push(AsmInstruction {
                id: load_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: AsmValue::StackSlot(slot_id),
                    alignment: Some(8),
                    volatile: false,
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: synthesized_annotations("x86.regfile.begin"),
            });
            *next_id += 1;
            self.registers.insert(reg, AsmValue::Register(load_id));
        }

        Ok(())
    }

    pub(super) fn end_block(
        &mut self,
        instructions: &mut Vec<AsmInstruction>,
        next_id: &mut u32,
    ) -> Result<()> {
        for reg in 0u8..=15 {
            let slot_id = *self
                .gpr_slot_by_reg
                .get(&reg)
                .ok_or_else(|| Error::from("missing x86 register slot"))?;
            let value = if let Some(value) = self.registers.get(&reg).cloned() {
                value
            } else {
                let local_id = *self
                    .locals_by_register
                    .get(&reg)
                    .ok_or_else(|| Error::from("missing x86 register local"))?;
                AsmValue::Local(local_id)
            };

            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value,
                    address: AsmValue::StackSlot(slot_id),
                    alignment: Some(8),
                    volatile: false,
                },
                ty: AsmType::Void,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: synthesized_annotations("x86.regfile.end"),
            });
            *next_id += 1;
        }

        Ok(())
    }

    pub(super) fn resolve_data_region(&self, address: u64) -> Option<(&DataRegion, u64)> {
        self.data_regions.iter().find_map(|region| {
            if address >= region.start && address < region.end {
                Some((region, address - region.start))
            } else {
                None
            }
        })
    }

    pub(super) fn resolve_rip_symbol(
        &self,
        memory: &X86Memory,
        inst_offset: u64,
        inst_len: usize,
    ) -> Option<&RipSymbol> {
        if memory.base != Some(16) || memory.index.is_some() {
            return None;
        }
        let pc = (self.code_base_address as i64)
            .checked_add(inst_offset as i64)?
            .checked_add(inst_len as i64)?;
        let target = pc.checked_add(memory.displacement)? as u64;
        self.rip_symbols.get(&target)
    }

    pub(super) fn resolve_disp32_symbol(
        &self,
        memory: &X86Memory,
        inst_offset: u64,
        inst_len: usize,
    ) -> Option<&RipSymbol> {
        if memory.base.is_some() || memory.index.is_some() {
            return None;
        }
        let pc = (self.code_base_address as i64)
            .checked_add(inst_offset as i64)?
            .checked_add(inst_len as i64)?;
        let target = pc.checked_add(memory.displacement)? as u64;
        self.rip_symbols.get(&target)
    }

    pub(super) fn x87_push(&mut self, value: AsmValue) -> Result<()> {
        if self.x87_stack.len() >= 8 {
            return Err(Error::from("x87 stack overflow"));
        }
        self.x87_stack.push(value);
        Ok(())
    }

    pub(super) fn x87_pop(&mut self) -> Result<AsmValue> {
        Ok(self
            .x87_stack
            .pop()
            .unwrap_or_else(|| AsmValue::Undef(AsmType::F64)))
    }

    pub(super) fn x87_peek(&self, index: u8) -> Result<AsmValue> {
        let index = usize::from(index);
        if index >= self.x87_stack.len() {
            return Ok(AsmValue::Undef(AsmType::F64));
        }
        Ok(self.x87_stack[self.x87_stack.len() - 1 - index].clone())
    }

    pub(super) fn x87_set(&mut self, index: u8, value: AsmValue) -> Result<()> {
        let index = usize::from(index);
        while self.x87_stack.len() <= index {
            self.x87_stack.push(AsmValue::Undef(AsmType::F64));
        }
        let slot = self.x87_stack.len() - 1 - index;
        self.x87_stack[slot] = value;
        Ok(())
    }

    pub(super) fn x87_swap(&mut self, index: u8) -> Result<()> {
        let index = usize::from(index);
        while self.x87_stack.len() <= index {
            self.x87_stack.push(AsmValue::Undef(AsmType::F64));
        }
        let top = self.x87_stack.len() - 1;
        let other = self.x87_stack.len() - 1 - index;
        self.x87_stack.swap(top, other);
        Ok(())
    }

    pub(super) fn read_return_value(&mut self) -> Option<AsmValue> {
        self.registers.get(&0).cloned().or_else(|| {
            self.ensure_local(0, false);
            Some(AsmValue::Local(*self.locals_by_register.get(&0)?))
        })
    }

    pub(super) fn read_gpr(&mut self, reg: u8) -> Result<AsmValue> {
        if let Some(value) = self.registers.get(&reg).cloned() {
            return Ok(value);
        }

        let is_argument = self.mark_sysv_args && matches!(reg, 7 | 6 | 2 | 1 | 8 | 9);
        self.ensure_local(reg, is_argument);
        let local_id = *self
            .locals_by_register
            .get(&reg)
            .ok_or_else(|| Error::from("missing local"))?;
        let value = AsmValue::Local(local_id);
        self.registers.insert(reg, value.clone());
        Ok(value)
    }

    pub(super) fn write_gpr(&mut self, reg: u8, value: AsmValue) {
        self.registers.insert(reg, value);
    }

    pub(super) fn read_vec(&mut self, reg: u8) -> Result<AsmValue> {
        if let Some(value) = self.vec_registers.get(&reg).cloned() {
            return Ok(value);
        }

        self.ensure_vec_local(reg);
        let local_id = *self
            .vec_locals_by_register
            .get(&reg)
            .ok_or_else(|| Error::from("missing x86_64 vector local"))?;
        let value = AsmValue::Local(local_id);
        self.vec_registers.insert(reg, value.clone());
        Ok(value)
    }

    pub(super) fn write_vec(&mut self, reg: u8, value: AsmValue) {
        self.vec_registers.insert(reg, value);
    }

    pub(super) fn ensure_local(&mut self, reg: u8, is_argument: bool) {
        if let Some(local_id) = self.locals_by_register.get(&reg).copied() {
            if is_argument {
                if let Some(local) = self.locals.iter_mut().find(|local| local.id == local_id) {
                    local.is_argument = true;
                }
            }
            return;
        }

        let local_id = self.next_local_id;
        self.next_local_id += 1;
        self.locals_by_register.insert(reg, local_id);
        self.locals.push(AsmLocal {
            id: local_id,
            ty: AsmType::I64,
            name: Some(reg_name(reg).to_string()),
            is_argument,
        });
    }

    pub(super) fn ensure_vec_local(&mut self, reg: u8) {
        if self.vec_locals_by_register.contains_key(&reg) {
            return;
        }

        let local_id = self.next_local_id;
        self.next_local_id += 1;
        self.vec_locals_by_register.insert(reg, local_id);
        self.locals.push(AsmLocal {
            id: local_id,
            ty: AsmType::Vector(Box::new(AsmType::I64), 2),
            name: Some(format!("xmm{reg}")),
            is_argument: false,
        });
    }
}

pub(super) fn reg_name(index: u8) -> &'static str {
    match index {
        0 => "rax",
        1 => "rcx",
        2 => "rdx",
        3 => "rbx",
        4 => "rsp",
        5 => "rbp",
        6 => "rsi",
        7 => "rdi",
        8 => "r8",
        9 => "r9",
        10 => "r10",
        11 => "r11",
        12 => "r12",
        13 => "r13",
        14 => "r14",
        15 => "r15",
        _ => "r0",
    }
}
