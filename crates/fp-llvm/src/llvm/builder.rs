use super::context::LlvmContext;
use llvm_ir::instruction::{
    Add, Alloca, BitCast, FAdd, FDiv, FMul, FRem, FSub, GetElementPtr, ICmp, InsertValue, Load,
    Mul, SExt, Store, Sub, Trunc, UDiv, ZExt,
};
use llvm_ir::terminator::{CondBr, Ret};
use llvm_ir::{BasicBlock, Instruction, IntPredicate, Name, Operand, Terminator, Type, TypeRef};

impl LlvmContext {
    /// Add an instruction to the current function's current basic block
    pub fn add_instruction(&mut self, instruction: Instruction) -> Result<(), String> {
        let func_name = self
            .current_function
            .clone()
            .ok_or("No current function set")?;

        let function = self
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == func_name)
            .ok_or("Current function not found")?;

        if function.basic_blocks.is_empty() {
            let entry_block = BasicBlock::new(Name::Name(Box::new("entry".to_string())));
            function.basic_blocks.push(entry_block);
        }

        let last_block_idx = function.basic_blocks.len() - 1;
        function.basic_blocks[last_block_idx]
            .instrs
            .push(instruction);
        Ok(())
    }
}

impl LlvmContext {
    /// Create an add instruction
    pub fn build_add(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::Add(Add {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            nuw: false,
            nsw: false,
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a floating-point add instruction
    pub fn build_fadd(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::FAdd(FAdd {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a subtract instruction
    pub fn build_sub(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::Sub(Sub {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            nsw: false,
            nuw: false,
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a floating-point subtract instruction
    pub fn build_fsub(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::FSub(FSub {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a multiply instruction
    pub fn build_mul(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::Mul(Mul {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            nuw: false,
            nsw: false,
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a floating-point multiply instruction
    pub fn build_fmul(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::FMul(FMul {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a divide instruction
    pub fn build_udiv(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::UDiv(UDiv {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            exact: false,
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a floating-point divide instruction
    pub fn build_fdiv(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::FDiv(FDiv {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a floating-point remainder instruction
    pub fn build_frem(
        &mut self,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::FRem(FRem {
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create an integer comparison instruction
    pub fn build_icmp(
        &mut self,
        predicate: IntPredicate,
        lhs: Operand,
        rhs: Operand,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::ICmp(ICmp {
            predicate,
            operand0: lhs,
            operand1: rhs,
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Zero-extend a value to a wider integer type
    pub fn build_zext(
        &mut self,
        operand: Operand,
        to_type: Type,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::ZExt(ZExt {
            operand,
            to_type: self.module.types.get_for_type(&to_type),
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Sign-extend a value to a wider integer type
    pub fn build_sext(
        &mut self,
        operand: Operand,
        to_type: Type,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::SExt(SExt {
            operand,
            to_type: self.module.types.get_for_type(&to_type),
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Truncate an integer value to a narrower type
    pub fn build_trunc(
        &mut self,
        operand: Operand,
        to_type: Type,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::Trunc(Trunc {
            operand,
            to_type: self.module.types.get_for_type(&to_type),
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    pub fn build_bitcast(
        &mut self,
        operand: Operand,
        target_type: Type,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::BitCast(BitCast {
            operand,
            to_type: self.module.types.get_for_type(&target_type),
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    pub fn build_inttoptr(
        &mut self,
        operand: Operand,
        target_ptr_type: Type,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::IntToPtr(llvm_ir::instruction::IntToPtr {
            operand,
            to_type: self.module.types.get_for_type(&target_ptr_type),
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    pub fn build_ptrtoint(
        &mut self,
        operand: Operand,
        target_int_type: Type,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::PtrToInt(llvm_ir::instruction::PtrToInt {
            operand,
            to_type: self.module.types.get_for_type(&target_int_type),
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    pub fn build_alloca(
        &mut self,
        allocated_type: TypeRef,
        num_elements: Operand,
        result_name: &str,
        alignment: u32,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::Alloca(Alloca {
            allocated_type,
            num_elements,
            dest: name.clone(),
            alignment,
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    pub fn build_load(
        &mut self,
        address: Operand,
        loaded_type: Type,
        result_name: &str,
        alignment: u32,
        volatile: bool,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::Load(Load {
            address,
            dest: name.clone(),
            loaded_ty: self.module.types.get_for_type(&loaded_type),
            volatile,
            atomicity: None,
            alignment,
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    pub fn build_store(
        &mut self,
        value: Operand,
        address: Operand,
        alignment: u32,
        volatile: bool,
    ) -> Result<(), String> {
        let instruction = Instruction::Store(Store {
            address,
            value,
            volatile,
            atomicity: None,
            alignment,
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(())
    }

    pub fn build_insert_value(
        &mut self,
        aggregate: Operand,
        element: Operand,
        indices: Vec<u32>,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::InsertValue(InsertValue {
            aggregate,
            element,
            indices,
            dest: name.clone(),
            debugloc: None,
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    pub fn build_gep(
        &mut self,
        address: Operand,
        element_type: Type,
        indices: Vec<Operand>,
        in_bounds: bool,
        result_name: &str,
    ) -> Result<Name, String> {
        let name = Name::Name(Box::new(result_name.to_string()));
        let instruction = Instruction::GetElementPtr(GetElementPtr {
            address,
            indices,
            dest: name.clone(),
            in_bounds,
            debugloc: None,
            source_element_type: self.module.types.get_for_type(&element_type),
        });
        self.add_instruction(instruction)?;
        Ok(name)
    }

    /// Create a return instruction
    pub fn build_return(&mut self, value: Option<Operand>) -> Result<(), String> {
        let func_name = self
            .current_function
            .clone()
            .ok_or("No current function set")?;
        let function = self
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == func_name)
            .ok_or("Current function not found")?;
        if function.basic_blocks.is_empty() {
            return Err("Function has no basic blocks".to_string());
        }
        let last_block_idx = function.basic_blocks.len() - 1;
        function.basic_blocks[last_block_idx].term = Terminator::Ret(Ret {
            return_operand: value,
            debugloc: None,
        });
        Ok(())
    }

    pub fn build_unconditional_branch(&mut self, target_label: &str) -> Result<(), String> {
        let func_name = self
            .current_function
            .clone()
            .ok_or("No current function set")?;
        let function = self
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == func_name)
            .ok_or("Current function not found")?;
        if function.basic_blocks.is_empty() {
            return Err("Function has no basic blocks".to_string());
        }
        let last_block_idx = function.basic_blocks.len() - 1;
        function.basic_blocks[last_block_idx].term = Terminator::Br(llvm_ir::terminator::Br {
            dest: Name::Name(Box::new(target_label.to_string())),
            debugloc: None,
        });
        Ok(())
    }

    pub fn build_conditional_branch(
        &mut self,
        condition: Operand,
        true_label: &str,
        false_label: &str,
    ) -> Result<(), String> {
        let func_name = self
            .current_function
            .clone()
            .ok_or("No current function set")?;
        let function = self
            .module
            .functions
            .iter_mut()
            .find(|f| f.name == func_name)
            .ok_or("Current function not found")?;
        if function.basic_blocks.is_empty() {
            return Err("Function has no basic blocks".to_string());
        }
        let last_block_idx = function.basic_blocks.len() - 1;
        function.basic_blocks[last_block_idx].term = Terminator::CondBr(CondBr {
            condition,
            true_dest: Name::Name(Box::new(true_label.to_string())),
            false_dest: Name::Name(Box::new(false_label.to_string())),
            debugloc: None,
        });
        Ok(())
    }
}
