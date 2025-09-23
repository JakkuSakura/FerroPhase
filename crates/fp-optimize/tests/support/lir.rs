use fp_core::lir::{self, LirBasicBlock, LirFunction, LirFunctionSignature, LirProgram, LirTerminator, LirType};

pub fn empty_program() -> LirProgram {
    LirProgram {
        functions: Vec::new(),
        globals: Vec::new(),
        type_definitions: Vec::new(),
    }
}

pub fn return_function(name: &str) -> LirFunction {
    LirFunction {
        name: name.to_string(),
        signature: LirFunctionSignature {
            params: Vec::new(),
            return_type: LirType::I32,
            is_variadic: false,
        },
        basic_blocks: vec![LirBasicBlock {
            id: 0,
            label: Some("entry".to_string()),
            instructions: Vec::new(),
            terminator: LirTerminator::Return(None),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }],
        locals: Vec::new(),
        stack_slots: Vec::new(),
        calling_convention: lir::CallingConvention::C,
        linkage: lir::Linkage::External,
    }
}
