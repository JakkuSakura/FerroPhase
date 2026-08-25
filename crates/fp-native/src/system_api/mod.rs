#![allow(dead_code)]

use fp_core::asmir::{
    AsmBlock, AsmConstant, AsmFunction, AsmFunctionSignature, AsmGenericOpcode, AsmGlobal,
    AsmGlobalRelocation, AsmInstruction, AsmInstructionKind, AsmLocal, AsmObjectFormat, AsmOpcode,
    AsmProgram, AsmRelocationKind, AsmSection, AsmSectionFlag, AsmSectionKind, AsmSysOp,
    AsmSyscallConvention, AsmTerminator, AsmType, AsmValue, PosixDirentStyle, PosixFlagStyle,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, Linkage, Name, Visibility};

pub(super) type SystemApiOp = AsmSysOp;

mod posix;
use posix::*;
mod windows_lowering;
use windows_lowering::*;
mod unix_lowering;
use unix_lowering::*;
mod ctype;
use ctype::*;
mod target_rewrite;
use target_rewrite::*;
mod matchers;
use matchers::*;
mod compat;
use compat::*;
mod compatibility;
pub(crate) use compatibility::rewrite_program_for_target;
use compatibility::*;
mod helpers;
use helpers::*;
mod syscalls;
use syscalls::*;

#[cfg(test)]
mod tests {
    use super::*;
    use fp_core::asmir::{AsmArchitecture, AsmEndianness, AsmTarget};
    use fp_core::container::{
        ContainerArchitecture, ContainerEndianness, ContainerFile, ContainerKind,
    };

    fn program(target_format: AsmObjectFormat) -> AsmProgram {
        let target = AsmTarget {
            architecture: AsmArchitecture::X86_64,
            object_format: target_format,
            endianness: AsmEndianness::Little,
            pointer_width: 64,
            default_calling_convention: None,
        };
        AsmProgram::new(target.clone(), target.data_layout())
    }

    #[test]
    fn rewrite_linux_readdir_call_to_darwin_shim() {
        let mut prog = program(AsmObjectFormat::MachO);
        prog.container = Some(ContainerFile::new(
            ContainerKind::Object,
            AsmObjectFormat::Elf,
            ContainerArchitecture::X86_64,
            ContainerEndianness::Little,
        ));

        let ptr_i8 = AsmType::Ptr(Box::new(AsmType::I8));
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: ptr_i8.clone(),
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("opendir".to_string()),
                            args: vec![AsmValue::Null(ptr_i8.clone())],
                            calling_convention: CallingConvention::C,
                            tail_call: false,
                        },
                        ty: ptr_i8.clone(),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("readdir".to_string()),
                            args: vec![AsmValue::Register(0)],
                            calling_convention: CallingConvention::C,
                            tail_call: false,
                        },
                        ty: ptr_i8.clone(),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(1))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        assert!(
            prog.functions
                .iter()
                .any(|f| f.name.as_str() == "fp_linux_readdir"),
            "expected fp_linux_readdir shim to be injected"
        );

        let block = &prog
            .functions
            .iter()
            .find(|f| f.name.as_str() == "main")
            .unwrap()
            .basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                &inst.kind,
                AsmInstructionKind::Call { function: AsmValue::Function(name), .. }
                    if name == "fp_linux_readdir"
            )
        }));
    }

    #[test]
    fn rewrite_linux_write_syscall_to_windows_writefile_sequence() {
        let mut prog = program(AsmObjectFormat::Coff);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
                    kind: AsmInstructionKind::Syscall {
                        convention: AsmSyscallConvention::LinuxX86_64,
                        number: AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                        args: vec![
                            AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                        ],
                    },
                    ty: AsmType::I64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "WriteFile"
        )));
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "GetStdHandle"
        )));
        assert!(matches!(
            block.terminator,
            fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0)))
        ));
    }

    #[test]
    fn rewrite_windows_writefile_sequence_back_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    // GetStdHandle(-11)
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!GetStdHandle".to_string()),
                            args: vec![AsmValue::Constant(AsmConstant::Int(-11, AsmType::I64))],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        ty: AsmType::Ptr(Box::new(AsmType::I8)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // alloca written
                    AsmInstruction {
                        id: 2,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Alloca),
                        kind: AsmInstructionKind::Alloca {
                            size: AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                            alignment: 8,
                        },
                        ty: AsmType::Ptr(Box::new(AsmType::I64)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // WriteFile(handle, null, 0, ptr, null)
                    AsmInstruction {
                        id: 3,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!WriteFile".to_string()),
                            args: vec![
                                AsmValue::Register(1),
                                AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                                AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                                AsmValue::Register(2),
                                AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            ],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        ty: AsmType::I1,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // load written -> id 0
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Load),
                        kind: AsmInstructionKind::Load {
                            address: AsmValue::Register(2),
                            alignment: Some(8),
                            volatile: false,
                        },
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                inst.kind,
                AsmInstructionKind::Syscall {
                    number: AsmValue::Constant(AsmConstant::UInt(1, _)),
                    ..
                }
            )
        }));
    }

    #[test]
    fn rewrite_windows_kernelbase_writefile_sequence_back_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernelbase!GetStdHandle".to_string()),
                            args: vec![AsmValue::Constant(AsmConstant::Int(-11, AsmType::I64))],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        ty: AsmType::Ptr(Box::new(AsmType::I8)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 2,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Alloca),
                        kind: AsmInstructionKind::Alloca {
                            size: AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                            alignment: 8,
                        },
                        ty: AsmType::Ptr(Box::new(AsmType::I64)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 3,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernelbase!WriteFile".to_string()),
                            args: vec![
                                AsmValue::Register(1),
                                AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                                AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                                AsmValue::Register(2),
                                AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            ],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        ty: AsmType::I1,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Load),
                        kind: AsmInstructionKind::Load {
                            address: AsmValue::Register(2),
                            alignment: Some(8),
                            volatile: false,
                        },
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                inst.kind,
                AsmInstructionKind::Syscall {
                    number: AsmValue::Constant(AsmConstant::UInt(1, _)),
                    ..
                }
            )
        }));
    }

    #[test]
    fn rewrite_ntdll_writefile_import_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("ntdll!NtWriteFile".to_string()),
                        args: vec![
                            AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                        ],
                        calling_convention: CallingConvention::Win64,
                        tail_call: false,
                    },
                    ty: AsmType::I64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                inst.kind,
                AsmInstructionKind::Syscall {
                    number: AsmValue::Constant(AsmConstant::UInt(1, _)),
                    ..
                }
            )
        }));
    }

    #[test]
    fn rewrite_ntdll_close_import_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("ntdll!ZwClose".to_string()),
                        args: vec![AsmValue::Constant(AsmConstant::UInt(3, AsmType::I64))],
                        calling_convention: CallingConvention::Win64,
                        tail_call: false,
                    },
                    ty: AsmType::I64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                inst.kind,
                AsmInstructionKind::Syscall {
                    number: AsmValue::Constant(AsmConstant::UInt(3, _)),
                    ..
                }
            )
        }));
    }

    #[test]
    fn rewrite_kernelbase_createfile_import_to_linux_open_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                    kind: AsmInstructionKind::Call {
                        function: AsmValue::Function("kernelbase!CreateFileA".to_string()),
                        args: vec![
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Constant(AsmConstant::Int(
                                0x8000_0000u32 as i64,
                                AsmType::I64,
                            )),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Constant(AsmConstant::Int(3, AsmType::I64)),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                        ],
                        calling_convention: CallingConvention::Win64,
                        tail_call: false,
                    },
                    ty: AsmType::I64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                inst.kind,
                AsmInstructionKind::Syscall {
                    number: AsmValue::Constant(AsmConstant::UInt(2, _)),
                    ..
                }
            )
        }));
    }

    #[test]
    fn rewrite_linux_read_syscall_to_windows_readfile_sequence() {
        let mut prog = program(AsmObjectFormat::Coff);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
                    kind: AsmInstructionKind::Syscall {
                        convention: AsmSyscallConvention::LinuxX86_64,
                        number: AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                        args: vec![
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                            AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                        ],
                    },
                    ty: AsmType::I64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "ReadFile"
        )));
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "GetStdHandle"
        )));
        assert!(matches!(
            block.terminator,
            fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0)))
        ));
    }

    #[test]
    fn rewrite_windows_readfile_sequence_back_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    // GetStdHandle(-10)
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!GetStdHandle".to_string()),
                            args: vec![AsmValue::Constant(AsmConstant::Int(-10, AsmType::I64))],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        ty: AsmType::Ptr(Box::new(AsmType::I8)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // alloca read
                    AsmInstruction {
                        id: 2,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Alloca),
                        kind: AsmInstructionKind::Alloca {
                            size: AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64)),
                            alignment: 8,
                        },
                        ty: AsmType::Ptr(Box::new(AsmType::I64)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // ReadFile(handle, null, 0, ptr, null)
                    AsmInstruction {
                        id: 3,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!ReadFile".to_string()),
                            args: vec![
                                AsmValue::Register(1),
                                AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                                AsmValue::Constant(AsmConstant::UInt(0, AsmType::I64)),
                                AsmValue::Register(2),
                                AsmValue::Null(AsmType::Ptr(Box::new(AsmType::I8))),
                            ],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        ty: AsmType::I1,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    // load read -> id 0
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Load),
                        kind: AsmInstructionKind::Load {
                            address: AsmValue::Register(2),
                            alignment: Some(8),
                            volatile: false,
                        },
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                inst.kind,
                AsmInstructionKind::Syscall {
                    number: AsmValue::Constant(AsmConstant::UInt(0, _)),
                    ..
                }
            )
        }));
    }

    #[test]
    fn rewrite_linux_close_syscall_to_windows_closehandle_sequence() {
        let mut prog = program(AsmObjectFormat::Coff);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![AsmInstruction {
                    id: 0,
                    opcode: AsmOpcode::Generic(AsmGenericOpcode::Syscall),
                    kind: AsmInstructionKind::Syscall {
                        convention: AsmSyscallConvention::LinuxX86_64,
                        number: AsmValue::Constant(AsmConstant::UInt(3, AsmType::I64)),
                        args: vec![AsmValue::Constant(AsmConstant::UInt(1, AsmType::I64))],
                    },
                    ty: AsmType::I64,
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                }],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "CloseHandle"
        )));
        assert!(block.instructions.iter().any(|inst| is_call_named(
            inst,
            "kernel32.dll",
            "GetStdHandle"
        )));
    }

    #[test]
    fn rewrite_windows_closehandle_sequence_back_to_linux_syscall() {
        let mut prog = program(AsmObjectFormat::Elf);
        prog.functions.push(fp_core::asmir::AsmFunction {
            name: fp_core::lir::Name::new("main"),
            signature: fp_core::asmir::AsmFunctionSignature {
                params: Vec::new(),
                return_type: AsmType::Void,
                is_variadic: false,
            },
            basic_blocks: vec![fp_core::asmir::AsmBlock {
                id: 0,
                label: None,
                instructions: vec![
                    AsmInstruction {
                        id: 1,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!GetStdHandle".to_string()),
                            args: vec![AsmValue::Constant(AsmConstant::Int(-11, AsmType::I64))],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        ty: AsmType::Ptr(Box::new(AsmType::I8)),
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 2,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Call),
                        kind: AsmInstructionKind::Call {
                            function: AsmValue::Function("kernel32!CloseHandle".to_string()),
                            args: vec![AsmValue::Register(1)],
                            calling_convention: CallingConvention::Win64,
                            tail_call: false,
                        },
                        ty: AsmType::I1,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 3,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Eq),
                        kind: AsmInstructionKind::Eq(
                            AsmValue::Register(2),
                            AsmValue::Constant(AsmConstant::Bool(false)),
                        ),
                        ty: AsmType::I1,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                    AsmInstruction {
                        id: 0,
                        opcode: AsmOpcode::Generic(AsmGenericOpcode::Select),
                        kind: AsmInstructionKind::Select {
                            condition: AsmValue::Register(3),
                            if_true: AsmValue::Constant(AsmConstant::Int(-1, AsmType::I64)),
                            if_false: AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        },
                        ty: AsmType::I64,
                        operands: Vec::new(),
                        implicit_uses: Vec::new(),
                        implicit_defs: Vec::new(),
                        encoding: None,
                        debug_info: None,
                        annotations: Vec::new(),
                    },
                ],
                terminator: fp_core::asmir::AsmTerminator::Return(Some(AsmValue::Register(0))),
                terminator_encoding: None,
                predecessors: Vec::new(),
                successors: Vec::new(),
            }],
            locals: Vec::new(),
            stack_slots: Vec::new(),
            frame: None,
            linkage: fp_core::lir::Linkage::External,
            visibility: fp_core::lir::Visibility::Default,
            calling_convention: None,
            section: None,
            is_declaration: false,
        });

        rewrite_program_for_target(&mut prog).unwrap();
        let block = &prog.functions[0].basic_blocks[0];
        assert!(block.instructions.iter().any(|inst| {
            matches!(
                inst.kind,
                AsmInstructionKind::Syscall {
                    number: AsmValue::Constant(AsmConstant::UInt(3, _)),
                    ..
                }
            )
        }));
    }
}
