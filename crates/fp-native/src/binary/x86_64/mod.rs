use crate::binary::cfg::wire_block_edges;
use crate::binary::{DataRegion, LiftedFunction, RipSymbol, RipSymbolKind, TextRelocation};
use fp_core::asmir::AsmLocal;
use fp_core::asmir::{
    AsmAnnotation, AsmConstant, AsmInstruction, AsmInstructionKind, AsmOpcode,
    AsmSyscallConvention, AsmType, AsmValue,
};
use fp_core::error::{Error, Result};
use fp_core::lir::{CallingConvention, Name};
use std::collections::HashMap;

mod decoder;
use decoder::*;
mod function_lifting;
pub use function_lifting::*;
mod instruction_lifting;
use instruction_lifting::*;
mod registers;
use registers::*;
mod lift_values;
use lift_values::*;
mod control_flow;
use control_flow::*;
mod jump_tables;
use jump_tables::*;
mod capstone;
use capstone::*;

mod simd;
mod simd_instructions;
use simd::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DecodedInstruction {
    offset: u64,
    len: usize,
    kind: Decoded,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LastCompare {
    id: u32,
    index: usize,
    is_float: bool,
}

pub(super) fn find_elf_sysv_main_offset(
    text_bytes: &[u8],
    text_address: u64,
    entry_address: u64,
    rip_symbols: &HashMap<u64, crate::binary::RipSymbol>,
) -> Option<usize> {
    use ::capstone::Syntax;
    use ::capstone::prelude::*;

    let entry_offset = entry_address.checked_sub(text_address)?;
    let entry_offset = usize::try_from(entry_offset).ok()?;
    if entry_offset >= text_bytes.len() {
        return None;
    }

    // Heuristic: glibc-style `_start` loads `main` into `rdi` before calling
    // `__libc_start_main`. We disassemble a small window from the entrypoint,
    // remember the last `lea rdi, [rip + disp]`, and confirm the subsequent
    // `call` targets `__libc_start_main` via its GOT relocation.
    let window_len = (text_bytes.len() - entry_offset).min(512);
    let window = &text_bytes[entry_offset..entry_offset + window_len];

    let mut capstone = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .build()
        .ok()?;
    capstone.set_syntax(Syntax::Intel).ok()?;
    let instructions = capstone.disasm_all(window, entry_address).ok()?;

    let trace = std::env::var_os("FP_LIFT_MAIN_TRACE").is_some();

    fn is_rip_relative(mem: &X86Memory) -> bool {
        mem.base == Some(16) && mem.index.is_none() && mem.displacement_offset.is_some()
    }

    let mut candidate_main: Option<u64> = None;
    for inst in instructions.iter() {
        let mnemonic = inst.mnemonic().unwrap_or("");
        match mnemonic {
            "lea" => {
                let op_str = inst.op_str().unwrap_or("");
                let mut parts = op_str.splitn(2, ',');
                let dst = parts.next().unwrap_or("").trim().to_ascii_lowercase();
                let src = parts.next().unwrap_or("").trim();
                if dst != "rdi" {
                    continue;
                }
                let Ok(mem) = parse_capstone_memory_operand(src) else {
                    continue;
                };
                if !is_rip_relative(&mem) {
                    continue;
                }
                let next_addr = inst.address().checked_add(inst.bytes().len() as u64)?;
                let target = (next_addr as i64).checked_add(mem.displacement)? as u64;
                if target >= text_address {
                    candidate_main = Some(target);
                    if trace {
                        eprintln!(
                            "[fp-native] ELF main candidate: 0x{target:x} (text_base=0x{text_address:x})"
                        );
                    }
                }
            }
            "call" => {
                let Some(candidate) = candidate_main else {
                    continue;
                };
                let op_str = inst.op_str().unwrap_or("");
                let Ok(mem) = parse_capstone_memory_operand(op_str) else {
                    continue;
                };
                if !is_rip_relative(&mem) {
                    continue;
                }
                let next_addr = inst.address().checked_add(inst.bytes().len() as u64)?;
                let got_target = (next_addr as i64).checked_add(mem.displacement)? as u64;
                let symbol = rip_symbols.get(&got_target)?;
                if trace {
                    eprintln!(
                        "[fp-native] ELF start call GOT 0x{got_target:x} -> {} ({:?})",
                        symbol.name, symbol.kind
                    );
                }

                let symbol_name = symbol.import.as_deref().unwrap_or(symbol.name.as_str());
                if symbol_name != "__libc_start_main" {
                    continue;
                }
                let offset = usize::try_from(candidate.checked_sub(text_address)?).ok()?;
                if offset < text_bytes.len() {
                    if trace {
                        eprintln!("[fp-native] ELF selected main offset: 0x{offset:x}");
                    }
                    return Some(offset);
                }
            }
            _ => {}
        }
    }

    None
}
