pub mod aarch64;
mod cfg;
mod object_lift;
pub mod x86_64;

use fp_core::asmir::AsmProgram;
use fp_core::error::Result;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RipSymbolKind {
    Function,
    Data,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RipSymbol {
    pub name: String,
    pub kind: RipSymbolKind,
    pub import: Option<String>,
    pub is_got: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextRelocation {
    pub offset: u64,
    pub symbol: String,
    pub is_import: bool,
    pub addend: i64,
    pub kind: object::RelocationKind,
    pub encoding: object::RelocationEncoding,
    pub size: u8,
    pub flags: object::RelocationFlags,
    pub is_got: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataRegion {
    pub start: u64,
    pub end: u64,
    pub symbol: String,
    pub bytes: Vec<u8>,
}

pub struct LiftedFunction {
    pub basic_blocks: Vec<fp_core::asmir::AsmBlock>,
    pub locals: Vec<fp_core::asmir::AsmLocal>,
    pub stack_slots: Vec<fp_core::asmir::AsmStackSlot>,
    pub direct_call_targets: Vec<u64>,
}

/// Lift an object file's machine code into generic AsmIR.
///
/// Current scope: x86_64 + aarch64 object files with text symbols.
pub fn lift_object_to_asmir(bytes: &[u8]) -> Result<AsmProgram> {
    object_lift::lift_object_to_asmir(bytes)
}

#[cfg(test)]
mod tests {
    use super::{aarch64, x86_64};
    use super::{RipSymbol, RipSymbolKind};
    use super::TextRelocation;
    use fp_core::asmir::{AsmConstant, AsmInstructionKind};
    use fp_core::asmir::AsmSyscallConvention;
    use object::{RelocationEncoding, RelocationFlags, RelocationKind};
    use std::collections::HashMap;

    #[test]
    fn x86_64_lifter_splits_blocks_for_unconditional_jump() {
        let bytes = [0xEB, 0x01, 0xC3, 0xC3];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();
        assert_eq!(lifted.basic_blocks.len(), 2);
        assert_eq!(lifted.basic_blocks[0].id, 0);
        assert!(matches!(
            lifted.basic_blocks[0].terminator,
            fp_core::asmir::AsmTerminator::Br(1)
        ));
        assert!(matches!(
            lifted.basic_blocks[1].terminator,
            fp_core::asmir::AsmTerminator::Return(_)
        ));
    }

    #[test]
    fn x86_64_lifter_lowers_got_tail_jump_to_external_call() {
        // Encoding: `jmpq *disp32(%rip)`
        //   ff 25 <disp32>
        // Followed by an unreachable `ret` to keep the byte stream finite.
        let code_base_address = 0x1000u64;
        let got_slot_address = 0x2000u64;
        let jmp_len = 6u64;
        let disp = (got_slot_address as i64) - (code_base_address as i64 + jmp_len as i64);
        let disp32 = disp as i32;
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&[0xff, 0x25]);
        bytes.extend_from_slice(&disp32.to_le_bytes());
        bytes.push(0xc3);

        let mut rip_symbols = HashMap::new();
        rip_symbols.insert(
            got_slot_address,
            RipSymbol {
                name: "memmove".to_string(),
                kind: RipSymbolKind::Data,
                import: Some("memmove".to_string()),
                is_got: true,
            },
        );

        let lifted = x86_64::lift_function_bytes_with_symbols(
            &bytes,
            &[],
            None,
            code_base_address,
            Some(&rip_symbols),
            None,
            None,
            None,
            None,
            0,
            true,
            false,
        )
        .unwrap();

        assert_eq!(lifted.basic_blocks.len(), 1);
        let block = &lifted.basic_blocks[0];
        let mut saw_memmove_call = false;
        for inst in &block.instructions {
            if let AsmInstructionKind::Call { function, .. } = &inst.kind {
                if matches!(function, fp_core::asmir::AsmValue::Function(name) if name == "memmove") {
                    saw_memmove_call = true;
                }
            }
        }
        assert!(saw_memmove_call, "expected tail jump to become a call");

        assert!(matches!(
            block.terminator,
            fp_core::asmir::AsmTerminator::Return(Some(_))
        ));
    }

    #[test]
    fn x86_64_lifter_models_strchr_return_via_rax_for_following_calls() {
        let bytes = [
            0xE8, 0x00, 0x00, 0x00, 0x00, // call rel32 strchr
            0x48, 0x89, 0xC2, // mov rdx, rax
            0xE8, 0x00, 0x00, 0x00, 0x00, // call rel32 fprintf
            0xC3, // ret
        ];
        let relocs = [
            TextRelocation {
                offset: 1,
                symbol: "strchr".to_string(),
                is_import: true,
                addend: 0,
                kind: RelocationKind::Relative,
                encoding: RelocationEncoding::X86Branch,
                size: 32,
                flags: RelocationFlags::Elf {
                    r_type: object::elf::R_X86_64_PLT32,
                },
                is_got: false,
            },
            TextRelocation {
                offset: 9,
                symbol: "fprintf".to_string(),
                is_import: true,
                addend: 0,
                kind: RelocationKind::Relative,
                encoding: RelocationEncoding::X86Branch,
                size: 32,
                flags: RelocationFlags::Elf {
                    r_type: object::elf::R_X86_64_PLT32,
                },
                is_got: false,
            },
        ];

        let lifted = x86_64::lift_function_bytes(
            &bytes,
            &relocs,
            Some(AsmSyscallConvention::LinuxX86_64),
        )
        .unwrap();
        let instructions = &lifted.basic_blocks[0].instructions;

        let inst_by_id: HashMap<u32, &AsmInstructionKind> =
            instructions.iter().map(|inst| (inst.id, &inst.kind)).collect();

        let mut calls = instructions
            .iter()
            .filter_map(|inst| match &inst.kind {
                AsmInstructionKind::Call { args, .. } => Some((inst.id, args.clone())),
                _ => None,
            });
        let (strchr_call_id, _) = calls.next().expect("missing strchr call");
        let (_, fprintf_args) = calls.next().expect("missing fprintf call");

        let Some(fp_core::asmir::AsmValue::Register(arg_id)) = fprintf_args.get(2) else {
            panic!("expected fprintf third argument to be a register");
        };
        let arg_resolves_to_return = *arg_id == strchr_call_id
            || matches!(
                inst_by_id.get(arg_id),
                Some(AsmInstructionKind::Freeze(fp_core::asmir::AsmValue::Register(id))) if *id == strchr_call_id
            );
        assert!(
            arg_resolves_to_return,
            "expected fprintf third argument to read strchr return via rax"
        );
    }

    #[test]
    fn x86_64_lifter_models_dcgettext_return_via_rax_for_following_calls() {
        let bytes = [
            0xE8, 0x00, 0x00, 0x00, 0x00, // call rel32 dcgettext
            0x48, 0x89, 0xC2, // mov rdx, rax
            0xE8, 0x00, 0x00, 0x00, 0x00, // call rel32 fprintf
            0xC3, // ret
        ];
        let relocs = [
            TextRelocation {
                offset: 1,
                symbol: "dcgettext".to_string(),
                is_import: true,
                addend: 0,
                kind: RelocationKind::Relative,
                encoding: RelocationEncoding::X86Branch,
                size: 32,
                flags: RelocationFlags::Elf {
                    r_type: object::elf::R_X86_64_PLT32,
                },
                is_got: false,
            },
            TextRelocation {
                offset: 9,
                symbol: "fprintf".to_string(),
                is_import: true,
                addend: 0,
                kind: RelocationKind::Relative,
                encoding: RelocationEncoding::X86Branch,
                size: 32,
                flags: RelocationFlags::Elf {
                    r_type: object::elf::R_X86_64_PLT32,
                },
                is_got: false,
            },
        ];

        let lifted = x86_64::lift_function_bytes(
            &bytes,
            &relocs,
            Some(AsmSyscallConvention::LinuxX86_64),
        )
        .unwrap();
        let instructions = &lifted.basic_blocks[0].instructions;

        let inst_by_id: HashMap<u32, &AsmInstructionKind> =
            instructions.iter().map(|inst| (inst.id, &inst.kind)).collect();

        let mut calls = instructions
            .iter()
            .filter_map(|inst| match &inst.kind {
                AsmInstructionKind::Call { args, .. } => Some((inst.id, args.clone())),
                _ => None,
            });
        let (dcgettext_call_id, _) = calls.next().expect("missing dcgettext call");
        let (_, fprintf_args) = calls.next().expect("missing fprintf call");

        let Some(fp_core::asmir::AsmValue::Register(arg_id)) = fprintf_args.get(2) else {
            panic!("expected fprintf third argument to be a register");
        };
        let arg_resolves_to_return = *arg_id == dcgettext_call_id
            || matches!(
                inst_by_id.get(arg_id),
                Some(AsmInstructionKind::Freeze(fp_core::asmir::AsmValue::Register(id))) if *id == dcgettext_call_id
            );
        assert!(
            arg_resolves_to_return,
            "expected fprintf third argument to read dcgettext return via rax"
        );
    }

    #[test]
    fn x86_64_lifter_lifts_rip_relative_cstring_via_lea() {
        // lea rdi, [rip + disp32]
        // call <relative>
        // ret
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&[0x48, 0x8D, 0x3D, 0xF9, 0x0F, 0x00, 0x00]);
        bytes.extend_from_slice(&[0xE8, 0xF4, 0xFF, 0xFF, 0xFF]);
        bytes.push(0xC3);

        let mut cstrings = HashMap::new();
        cstrings.insert(0x2000u64, "hello".to_string());

        let lifted = x86_64::lift_function_bytes_with_symbols(
            &bytes,
            &[],
            None,
            0x1000,
            None,
            None,
            None,
            Some(&cstrings),
            None,
            0,
            true,
            true,
        )
        .unwrap();

        let call_args = lifted.basic_blocks[0]
            .instructions
            .iter()
            .find_map(|inst| match &inst.kind {
                AsmInstructionKind::Call { args, .. } => Some(args),
                _ => None,
            })
            .expect("missing call");
        assert!(call_args.is_empty(), "expected lifted internal call to carry no args");

        let string_value = lifted.basic_blocks[0]
            .instructions
            .iter()
            .find_map(|inst| match &inst.kind {
                AsmInstructionKind::Store { value, address, .. }
                    if matches!(address, fp_core::asmir::AsmValue::StackSlot(7)) =>
                {
                    match value {
                        fp_core::asmir::AsmValue::Constant(AsmConstant::String(text)) => {
                            Some(text.clone())
                        }
                        fp_core::asmir::AsmValue::Register(id) => lifted.basic_blocks[0]
                            .instructions
                            .iter()
                            .find(|inst| inst.id == *id)
                            .and_then(|inst| match &inst.kind {
                                AsmInstructionKind::Freeze(fp_core::asmir::AsmValue::Constant(
                                    AsmConstant::String(text),
                                )) => Some(text.clone()),
                                _ => None,
                            }),
                        _ => None,
                    }
                }
                _ => None,
            })
            .expect("expected rdi regfile slot to store string literal");
        assert_eq!(string_value, "hello");
    }

    #[test]
    fn x86_64_lifter_splits_blocks_for_conditional_jump() {
        // cmp rax, 0; je +1; ret; ret
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&[0x48, 0x3D, 0x00, 0x00, 0x00, 0x00]);
        bytes.extend_from_slice(&[0x74, 0x01]);
        bytes.push(0xC3);
        bytes.push(0xC3);

        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();
        assert_eq!(lifted.basic_blocks.len(), 3);
        assert!(matches!(
            lifted.basic_blocks[0].terminator,
            fp_core::asmir::AsmTerminator::CondBr {
                if_true: 2,
                if_false: 1,
                ..
            }
        ));
        assert!(matches!(
            lifted.basic_blocks[1].terminator,
            fp_core::asmir::AsmTerminator::Return(_)
        ));
        assert!(matches!(
            lifted.basic_blocks[2].terminator,
            fp_core::asmir::AsmTerminator::Return(_)
        ));
    }

    #[test]
    fn aarch64_lifter_splits_blocks_for_unconditional_branch() {
        let b = 0x1400_0001u32.to_le_bytes();
        let nop = 0xD503_201Fu32.to_le_bytes();
        let ret = 0xD65F_03C0u32.to_le_bytes();
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&b);
        bytes.extend_from_slice(&nop);
        bytes.extend_from_slice(&ret);

        let lifted =
            aarch64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxAarch64))
                .unwrap();
        assert_eq!(lifted.basic_blocks.len(), 3);
        assert!(matches!(
            lifted.basic_blocks[0].terminator,
            fp_core::asmir::AsmTerminator::Br(2)
        ));
        assert!(matches!(
            lifted.basic_blocks[1].terminator,
            fp_core::asmir::AsmTerminator::Br(2)
        ));
        assert!(matches!(
            lifted.basic_blocks[2].terminator,
            fp_core::asmir::AsmTerminator::Return(_)
        ));
    }

    #[test]
    fn aarch64_lifter_splits_blocks_for_conditional_branch() {
        // cmp x0, x0; b.eq +8; ret; ret
        let cmp = 0xEB00_001Fu32.to_le_bytes();
        let b_eq = 0x5400_0020u32.to_le_bytes();
        let ret = 0xD65F_03C0u32.to_le_bytes();

        let mut bytes = Vec::new();
        bytes.extend_from_slice(&cmp);
        bytes.extend_from_slice(&b_eq);
        bytes.extend_from_slice(&ret);
        bytes.extend_from_slice(&ret);

        let lifted =
            aarch64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxAarch64))
                .unwrap();
        assert_eq!(lifted.basic_blocks.len(), 3);
        assert!(matches!(
            lifted.basic_blocks[0].terminator,
            fp_core::asmir::AsmTerminator::CondBr {
                if_true: 2,
                if_false: 1,
                ..
            }
        ));
        assert!(matches!(
            lifted.basic_blocks[1].terminator,
            fp_core::asmir::AsmTerminator::Return(_)
        ));
        assert!(matches!(
            lifted.basic_blocks[2].terminator,
            fp_core::asmir::AsmTerminator::Return(_)
        ));
    }

    #[test]
    fn x86_64_lifter_synthesizes_symbol_for_rip_relative_without_relocation() {
        // mov rax, [rip + 0x10]; ret
        // - instruction starts at 0, length is 7 bytes
        // - next_ip is 7
        // - absolute = 7 + 0x10 = 0x17
        let bytes = [0x48, 0x8B, 0x05, 0x10, 0x00, 0x00, 0x00, 0xC3];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();

        let mut has_address_constant = false;
        for block in &lifted.basic_blocks {
            for instruction in &block.instructions {
                if let AsmInstructionKind::Freeze(value) = &instruction.kind {
                    if let fp_core::asmir::AsmValue::Constant(AsmConstant::UInt(value, _)) = value {
                        if *value == 0x17 {
                            has_address_constant = true;
                        }
                    }
                }
            }
        }

        assert!(has_address_constant);
    }

    #[test]
    fn x86_64_lifter_lifts_movq_xmm_mem_via_vector_lane_ops() {
        // movq xmm7, qword ptr [rip + 0x11223344]
        // movq qword ptr [rip + 0x11223344], xmm7
        // ret
        let bytes = [
            0xF3, 0x0F, 0x7E, 0x3D, 0x44, 0x33, 0x22, 0x11, 0x66, 0x0F, 0xD6, 0x3D, 0x44,
            0x33, 0x22, 0x11, 0xC3,
        ];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();

        let mut has_build_vector = false;
        let mut has_extract_lane = false;
        for block in &lifted.basic_blocks {
            for instruction in &block.instructions {
                match &instruction.kind {
                    AsmInstructionKind::BuildVector { .. } => has_build_vector = true,
                    AsmInstructionKind::ExtractLane { .. } => has_extract_lane = true,
                    _ => {}
                }
            }
        }
        assert!(has_build_vector);
        assert!(has_extract_lane);
    }

    #[test]
    fn x86_64_lifter_lifts_movd_xmm_mem32_via_load_and_build_vector() {
        // movd xmm0, dword ptr [r10 + 0x8]; ret
        let bytes = [0x66, 0x41, 0x0F, 0x6E, 0x42, 0x08, 0xC3];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();

        let mut saw_load = false;
        let mut saw_build_vector = false;
        for block in &lifted.basic_blocks {
            for instruction in &block.instructions {
                match &instruction.kind {
                    AsmInstructionKind::Load { .. } => saw_load = true,
                    AsmInstructionKind::BuildVector { .. } => saw_build_vector = true,
                    _ => {}
                }
            }
        }

        assert!(saw_load);
        assert!(saw_build_vector);
    }

    #[test]
    fn x86_64_lifter_lifts_movd_mem32_xmm_via_extract_and_store() {
        // movd dword ptr [rbp - 0x10], xmm1
        // ret
        let bytes = [0x66, 0x0F, 0x7E, 0x4D, 0xF0, 0xC3];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();

        let mut saw_extract_lane = false;
        let mut saw_store = false;
        for block in &lifted.basic_blocks {
            for instruction in &block.instructions {
                match &instruction.kind {
                    AsmInstructionKind::ExtractLane { .. } => saw_extract_lane = true,
                    AsmInstructionKind::Store { .. } => saw_store = true,
                    _ => {}
                }
            }
        }

        assert!(saw_extract_lane);
        assert!(saw_store);
    }

    #[test]
    fn x86_64_lifter_lifts_movd_gpr_from_xmm_via_extract_lane() {
        // movd eax, xmm0
        // ret
        let bytes = [0x66, 0x0F, 0x7E, 0xC0, 0xC3];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();

        let mut saw_extract_lane = false;
        for block in &lifted.basic_blocks {
            for instruction in &block.instructions {
                if matches!(instruction.kind, AsmInstructionKind::ExtractLane { .. }) {
                    saw_extract_lane = true;
                }
            }
        }

        assert!(saw_extract_lane);
    }

    #[test]
    fn x86_64_lifter_lifts_pextrq_via_extract_lane() {
        // pextrq rsi, xmm0, 1
        // ret
        let bytes = [0x66, 0x48, 0x0F, 0x3A, 0x16, 0xF0, 0x01, 0xC3];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();

        let mut saw_extract_lane_1 = false;
        for block in &lifted.basic_blocks {
            for instruction in &block.instructions {
                if let AsmInstructionKind::ExtractLane { lane, .. } = &instruction.kind {
                    if *lane == 1 {
                        saw_extract_lane_1 = true;
                    }
                }
            }
        }
        assert!(saw_extract_lane_1);
    }

    #[test]
    fn x86_64_lifter_lifts_movbe_via_byte_swap_sequence() {
        // movbe r10d, dword ptr [rsp + 0x20]
        // ret
        let bytes = [0x44, 0x0F, 0x38, 0xF0, 0x54, 0x24, 0x20, 0xC3];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();

        let mut has_load = false;
        let mut has_shift = false;
        for block in &lifted.basic_blocks {
            for instruction in &block.instructions {
                match &instruction.kind {
                    AsmInstructionKind::Load { .. } => has_load = true,
                    AsmInstructionKind::Shl(_, _) | AsmInstructionKind::Shr(_, _) => {
                        has_shift = true
                    }
                    _ => {}
                }
            }
        }

        assert!(has_load);
        assert!(has_shift);
    }

    #[test]
    fn x86_64_lifter_lifts_cdq_into_select_mask() {
        // cdq; ret
        let bytes = [0x99, 0xC3];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();

        let mut saw_trunc = false;
        let mut saw_select = false;
        for block in &lifted.basic_blocks {
            for instruction in &block.instructions {
                match &instruction.kind {
                    AsmInstructionKind::Trunc(_, _) => saw_trunc = true,
                    AsmInstructionKind::Select { .. } => saw_select = true,
                    _ => {}
                }
            }
        }

        assert!(saw_trunc);
        assert!(saw_select);
    }

    #[test]
    fn x86_64_lifter_lifts_vptest_with_memory_operand() {
        // vptest xmm0, xmmword ptr [rbx + 0x18]
        // ret
        let bytes = [0xC4, 0xE2, 0x79, 0x17, 0x43, 0x18, 0xC3];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();

        let mut has_compare = false;
        for block in &lifted.basic_blocks {
            for instruction in &block.instructions {
                if matches!(instruction.kind, AsmInstructionKind::Eq(_, _)) {
                    has_compare = true;
                }
            }
        }

        assert!(has_compare);
    }

    #[test]
    fn x86_64_lifter_lifts_mul_rm_and_writes_wide_result() {
        // mul qword ptr [rsp + 0x10]
        // ret
        let bytes = [0x48, 0xF7, 0x64, 0x24, 0x10, 0xC3];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();

        let mut has_mul = false;
        let mut has_shift = false;
        for block in &lifted.basic_blocks {
            for instruction in &block.instructions {
                match &instruction.kind {
                    AsmInstructionKind::Mul(_, _) => has_mul = true,
                    AsmInstructionKind::Shr(_, _) | AsmInstructionKind::Shl(_, _) => has_shift = true,
                    _ => {}
                }
            }
        }

        assert!(has_mul);
        assert!(has_shift);
    }
}
