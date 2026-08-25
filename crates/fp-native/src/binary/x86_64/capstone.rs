use super::*;

fn parse_capstone_two_operands(op_str: &str) -> Result<(&str, &str)> {
    let (lhs, rhs) = op_str
        .split_once(',')
        .ok_or_else(|| Error::from("expected two capstone operands"))?;
    Ok((lhs.trim(), rhs.trim()))
}

fn parse_capstone_immediate(token: &str) -> Result<i64> {
    let token = token.trim();
    let (sign, rest) = if let Some(rest) = token.strip_prefix('-') {
        (-1i64, rest.trim())
    } else {
        (1i64, token)
    };

    let value = if let Some(rest) = rest.strip_prefix("0x") {
        i64::from_str_radix(rest, 16)
            .map_err(|e| Error::from(format!("invalid immediate: {token}: {e}")))?
    } else {
        rest.parse::<i64>()
            .map_err(|e| Error::from(format!("invalid immediate: {token}: {e}")))?
    };
    Ok(sign * value)
}

fn capstone_operand_width_bits(token: &str) -> Option<u16> {
    let lower = token.to_ascii_lowercase();
    if lower.contains("byte ptr") {
        return Some(8);
    }
    if lower.contains("tbyte ptr") || lower.contains("tword ptr") || lower.contains("xword ptr") {
        return Some(80);
    }
    if lower.contains("qword ptr") {
        return Some(64);
    }
    if lower.contains("dword ptr") {
        return Some(32);
    }
    if lower.contains("word ptr") {
        return Some(16);
    }

    let token = token.trim();
    let lower = token.to_ascii_lowercase();
    if matches!(
        lower.as_str(),
        "al" | "cl" | "dl" | "bl" | "spl" | "bpl" | "sil" | "dil"
    ) {
        return Some(8);
    }
    if matches!(
        lower.as_str(),
        "ax" | "cx" | "dx" | "bx" | "sp" | "bp" | "si" | "di"
    ) {
        return Some(16);
    }
    if matches!(
        lower.as_str(),
        "eax" | "ecx" | "edx" | "ebx" | "esp" | "ebp" | "esi" | "edi"
    ) {
        return Some(32);
    }
    if matches!(
        lower.as_str(),
        "rax" | "rcx" | "rdx" | "rbx" | "rsp" | "rbp" | "rsi" | "rdi"
    ) {
        return Some(64);
    }
    if let Some(rest) = lower.strip_prefix('r') {
        if rest.ends_with('b') {
            return Some(8);
        }
        if rest.ends_with('w') {
            return Some(16);
        }
        if rest.ends_with('d') {
            return Some(32);
        }
        if rest.parse::<u8>().is_ok() {
            return Some(64);
        }
    }
    // JUSTIFY: x86 operand strings from capstone do not always encode an
    // explicit width; callers apply context-appropriate defaults.
    eprintln!(
        "[fp-native] capstone_operand_width_bits: unable to determine width from token: {token:?}"
    );
    None
}

pub(super) fn parse_xmm_register(token: &str) -> Result<u8> {
    let id = token
        .trim()
        .strip_prefix("xmm")
        .ok_or_else(|| Error::from(format!("expected xmm register, got: {token}")))?
        .parse::<u8>()
        .map_err(|e| Error::from(format!("invalid xmm register: {token}: {e}")))?;
    Ok(id)
}

pub(super) fn parse_gpr_register(token: &str) -> Result<u8> {
    let token = token.trim();
    let normalized = token.trim_end_matches(['d', 'w', 'b']);
    let mapped = match normalized {
        "rax" | "eax" | "ax" | "al" => 0,
        "rcx" | "ecx" | "cx" | "cl" => 1,
        "rdx" | "edx" | "dx" | "dl" => 2,
        "rbx" | "ebx" | "bx" | "bl" => 3,
        "rsp" | "esp" | "sp" | "spl" => 4,
        "rbp" | "ebp" | "bp" | "bpl" => 5,
        "rsi" | "esi" | "si" | "sil" => 6,
        "rdi" | "edi" | "di" | "dil" => 7,
        _ => {
            if let Some(rest) = normalized.strip_prefix('r') {
                let id = rest
                    .parse::<u8>()
                    .map_err(|e| Error::from(format!("invalid gpr register: {token}: {e}")))?;
                if id >= 8 {
                    return Ok(id);
                }
            }
            return Err(Error::from(format!("unsupported gpr register: {token}")));
        }
    };
    Ok(mapped)
}

pub(super) fn decode_stream(bytes: &[u8]) -> Result<Vec<DecodedInstruction>> {
    use ::capstone::Syntax;
    use ::capstone::prelude::*;

    let mut capstone = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .build()
        .map_err(|err| Error::from(format!("failed to initialize capstone: {err}")))?;

    capstone
        .set_syntax(Syntax::Intel)
        .map_err(|err| Error::from(format!("failed to set capstone intel syntax: {err}")))?;

    let instructions = capstone
        .disasm_all(bytes, 0)
        .map_err(|err| Error::from(format!("failed to disassemble x86_64: {err}")))?;

    let mut decoded = Vec::with_capacity(instructions.len());
    for inst in instructions.iter() {
        let offset = inst.address();
        let len = inst.bytes().len();
        let offset_usize = usize::try_from(offset).map_err(|_| {
            Error::from(format!(
                "x86_64 instruction offset overflow: offset={offset}"
            ))
        })?;
        let end = offset_usize
            .checked_add(len)
            .ok_or_else(|| Error::from("x86_64 instruction length overflow"))?;
        let slice = bytes
            .get(offset_usize..end)
            .ok_or_else(|| Error::from("x86_64 instruction slice out of bounds"))?;

        let decode_error = match decode_instruction(slice, offset) {
            Ok(Some((kind, consumed))) => {
                decoded.push(DecodedInstruction { offset, len, kind });
                if consumed != len {
                    return Err(Error::from(format!(
                        "x86_64 decode length mismatch at 0x{offset:x}: capstone={len} custom={consumed}"
                    )));
                }
                continue;
            }
            Ok(None) => None,
            Err(err) => Some(err),
        };

        let (kind, consumed) = {
            let mnemonic = inst.mnemonic().unwrap_or("<unknown>");
            let op_str = inst.op_str().unwrap_or("");
            if (op_str.contains("zmm") || op_str.contains("ymm")) && mnemonic.starts_with('v') {
                // Many real-world x86_64 binaries ship multiple SIMD-optimized variants
                // of helper routines (often behind CPUID dispatch). We currently lift a
                // scalar subset, so treat unsupported wide-vector instructions as NOP to
                // keep exploring the executable.
                (Decoded::Nop, len)
            } else if mnemonic.starts_with('k') {
                // AVX-512 mask register operations. Treat as NOP for now.
                (Decoded::Nop, len)
            } else if op_str
                .split(|c: char| !c.is_ascii_alphanumeric())
                .any(|token| matches!(token, "ah" | "bh" | "ch" | "dh"))
            {
                // High 8-bit registers require subregister modeling; skip for now.
                (Decoded::Nop, len)
            } else if mnemonic == "movbe" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 2 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 movbe operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }

                let (kind, width_bits) = if parts[0].contains('[') {
                    let memory = parse_capstone_memory_operand(parts[0])?;
                    let src = parts[1];
                    let width_bits = capstone_operand_width_bits(src).ok_or_else(|| {
                        Error::from(format!(
                            "unsupported movbe register width at 0x{offset:x}: {src}"
                        ))
                    })?;
                    let src = parse_gpr_register(src)?;
                    (
                        Decoded::MovbeMemFromReg {
                            dst: memory,
                            src,
                            width_bits,
                        },
                        width_bits,
                    )
                } else if parts[1].contains('[') {
                    let dst = parts[0];
                    let memory = parse_capstone_memory_operand(parts[1])?;
                    let width_bits = capstone_operand_width_bits(dst).ok_or_else(|| {
                        Error::from(format!(
                            "unsupported movbe register width at 0x{offset:x}: {dst}"
                        ))
                    })?;
                    let dst = parse_gpr_register(dst)?;
                    (
                        Decoded::MovbeRegFromMem {
                            dst,
                            src: memory,
                            width_bits,
                        },
                        width_bits,
                    )
                } else {
                    return Err(Error::from(format!(
                        "unsupported x86_64 movbe form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                };

                if !matches!(width_bits, 16 | 32 | 64) {
                    return Err(Error::from(format!(
                        "unsupported x86_64 movbe width at 0x{offset:x}: {width_bits}"
                    )));
                }

                (kind, len)
            } else if mnemonic == "bswap" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 1 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 bswap operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst_token = parts[0];
                let width_bits = capstone_operand_width_bits(dst_token).unwrap_or(64);
                if !matches!(width_bits, 32 | 64) {
                    return Err(Error::from(format!(
                        "unsupported x86_64 bswap width at 0x{offset:x}: {width_bits}"
                    )));
                }
                let dst = parse_gpr_register(dst_token)?;
                (Decoded::Bswap { dst, width_bits }, len)
            } else if mnemonic == "vpbroadcastq" {
                let (dst, src) = parse_capstone_two_operands(op_str)?;
                let dst = parse_xmm_register(dst)?;
                let src = parse_gpr_register(src)?;
                (Decoded::Vpbroadcastq { dst, src }, len)
            } else if mnemonic == "vpxorq" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpxorq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                if parts[2].contains('[') {
                    let rhs = parse_capstone_memory_operand(parts[2])?;
                    (Decoded::VpxorqXmmMem { dst, lhs, rhs }, len)
                } else {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpxorq form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
            } else if mnemonic == "vptest" {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                let lhs = parse_xmm_register(lhs)?;
                if rhs.contains('[') {
                    let rhs = parse_capstone_memory_operand(rhs)?;
                    (Decoded::VptestMem { lhs, rhs }, len)
                } else {
                    let rhs = parse_xmm_register(rhs)?;
                    (Decoded::Vptest { lhs, rhs }, len)
                }
            } else if matches!(mnemonic, "vpcmpeqd" | "pcmpeqd") {
                let parts = parse_capstone_operands(op_str);
                let (dst, lhs, rhs) = if parts.len() == 3 {
                    (
                        parse_xmm_register(parts[0])?,
                        parse_xmm_register(parts[1])?,
                        parse_xmm_register(parts[2])?,
                    )
                } else if parts.len() == 2 {
                    let dst = parse_xmm_register(parts[0])?;
                    (dst, dst, parse_xmm_register(parts[1])?)
                } else {
                    return Err(Error::from(format!(
                        "unsupported x86_64 pcmpeqd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                };

                if lhs == rhs {
                    (Decoded::OnesXmm { dst }, len)
                } else {
                    return Err(Error::from(format!(
                        "unsupported x86_64 pcmpeqd form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
            } else if mnemonic == "vpalignr" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 4 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpalignr operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                let rhs = if parts[2].contains('[') {
                    VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    VecOperand::Reg(parse_xmm_register(parts[2])?)
                };
                let imm = parts[3]
                    .parse::<u8>()
                    .map_err(|e| Error::from(format!("invalid vpalignr immediate: {e}")))?;
                (Decoded::Vpalignr { dst, lhs, rhs, imm }, len)
            } else if matches!(mnemonic, "vpmaxsq" | "vpmaxuq") {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpmaxsq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                let rhs = if parts[2].contains('[') {
                    VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    VecOperand::Reg(parse_xmm_register(parts[2])?)
                };
                if mnemonic == "vpmaxsq" {
                    (Decoded::Vpmaxsq { dst, lhs, rhs }, len)
                } else {
                    (Decoded::Vpmaxuq { dst, lhs, rhs }, len)
                }
            } else if mnemonic == "vpmaxud" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpmaxud operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                let rhs = if parts[2].contains('[') {
                    VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    VecOperand::Reg(parse_xmm_register(parts[2])?)
                };
                (Decoded::Vpmaxud { dst, lhs, rhs }, len)
            } else if mnemonic == "vpminuq" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpminuq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                let rhs = if parts[2].contains('[') {
                    VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    VecOperand::Reg(parse_xmm_register(parts[2])?)
                };
                (Decoded::Vpminuq { dst, lhs, rhs }, len)
            } else if mnemonic == "vpsubq" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpsubq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                let rhs = if parts[2].contains('[') {
                    VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    VecOperand::Reg(parse_xmm_register(parts[2])?)
                };
                (Decoded::Vpsubq { dst, lhs, rhs }, len)
            } else if mnemonic == "vpaddd" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpaddd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                let rhs = if parts[2].contains('[') {
                    VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    VecOperand::Reg(parse_xmm_register(parts[2])?)
                };
                (Decoded::Vpaddd { dst, lhs, rhs }, len)
            } else if mnemonic == "vpaddq" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpaddq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                let rhs = if parts[2].contains('[') {
                    VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    VecOperand::Reg(parse_xmm_register(parts[2])?)
                };
                (Decoded::Vpaddq { dst, lhs, rhs }, len)
            } else if mnemonic == "vpsrldq" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpsrldq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let src = parse_xmm_register(parts[1])?;
                let imm = parts[2]
                    .parse::<u8>()
                    .map_err(|e| Error::from(format!("invalid vpsrldq immediate: {e}")))?;
                (Decoded::Vpsrldq { dst, src, imm }, len)
            } else if matches!(mnemonic, "vpandq" | "vporq") {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 {mnemonic} operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                let rhs = if parts[2].contains('[') {
                    VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    VecOperand::Reg(parse_xmm_register(parts[2])?)
                };
                if mnemonic == "vpandq" {
                    (Decoded::Vpandq { dst, lhs, rhs }, len)
                } else {
                    (Decoded::Vporq { dst, lhs, rhs }, len)
                }
            } else if matches!(mnemonic, "vpunpcklwd" | "vpunpckldq" | "vpunpcklqdq") {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpunpcklwd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                let rhs = if parts[2].contains('[') {
                    VecOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    VecOperand::Reg(parse_xmm_register(parts[2])?)
                };
                if mnemonic == "vpunpcklwd" {
                    (Decoded::Vpunpcklwd { dst, lhs, rhs }, len)
                } else if mnemonic == "vpunpckldq" {
                    (Decoded::Vpunpckldq { dst, lhs, rhs }, len)
                } else {
                    (Decoded::Vpunpcklqdq { dst, lhs, rhs }, len)
                }
            } else if mnemonic == "vpinsrd" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 4 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpinsrd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let vector = parse_xmm_register(parts[1])?;
                let value = if parts[2].contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    RmOperand::Reg(parse_gpr_register(parts[2])?)
                };
                let lane = parts[3]
                    .parse::<u8>()
                    .map_err(|e| Error::from(format!("invalid vpinsrd lane immediate: {e}")))?;
                (
                    Decoded::Pinsrd {
                        dst,
                        vector,
                        value,
                        lane,
                    },
                    len,
                )
            } else if mnemonic == "vpinsrb" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 4 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vpinsrb operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let vector = parse_xmm_register(parts[1])?;
                let value = if parts[2].contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    RmOperand::Reg(parse_gpr_register(parts[2])?)
                };
                let lane = parts[3]
                    .parse::<u8>()
                    .map_err(|e| Error::from(format!("invalid vpinsrb lane immediate: {e}")))?;
                (
                    Decoded::Pinsrb {
                        dst,
                        vector,
                        value,
                        lane,
                    },
                    len,
                )
            } else if matches!(mnemonic, "vmovd" | "movd") {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                if lhs.starts_with("xmm") {
                    let dst = parse_xmm_register(lhs)?;
                    if rhs.contains('[') {
                        let src = parse_capstone_memory_operand(rhs)?;
                        (Decoded::MovdXmmFromMem32 { dst, src }, len)
                    } else {
                        let src = parse_gpr_register(rhs)?;
                        (Decoded::MovdXmmFromGpr32 { dst, src }, len)
                    }
                } else if rhs.starts_with("xmm") {
                    let src = parse_xmm_register(rhs)?;
                    if lhs.contains('[') {
                        let dst = parse_capstone_memory_operand(lhs)?;
                        (Decoded::MovdMem32FromXmm { dst, src }, len)
                    } else {
                        let dst = parse_gpr_register(lhs)?;
                        let width_bits = capstone_operand_width_bits(lhs).unwrap_or(32);
                        (
                            Decoded::MovdGpr32FromXmm {
                                dst,
                                src,
                                width_bits,
                            },
                            len,
                        )
                    }
                } else {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vmovd form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
            } else if matches!(mnemonic, "vpxor" | "vxorps" | "vxorpd") {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vxor operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                let rhs = parse_xmm_register(parts[2])?;
                if lhs == rhs {
                    (Decoded::ZeroXmm { dst }, len)
                } else {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vxor form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
            } else if mnemonic == "vcvtusi2sd" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vcvtusi2sd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let src_vec = parse_xmm_register(parts[1])?;
                let src_gpr = if parts[2].contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    RmOperand::Reg(parse_gpr_register(parts[2])?)
                };
                let width_bits = capstone_operand_width_bits(parts[2]).unwrap_or(64);
                (
                    Decoded::Vcvtusi2sd {
                        dst,
                        src_vec,
                        src_gpr,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "vcvtusi2ss" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vcvtusi2ss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let src_vec = parse_xmm_register(parts[1])?;
                let src_gpr = if parts[2].contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    RmOperand::Reg(parse_gpr_register(parts[2])?)
                };
                let width_bits = capstone_operand_width_bits(parts[2]).unwrap_or(64);
                (
                    Decoded::Vcvtusi2ss {
                        dst,
                        src_vec,
                        src_gpr,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "vmulsd" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vmulsd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                if parts[2].contains('[') {
                    let rhs = parse_capstone_memory_operand(parts[2])?;
                    (Decoded::VmulsdMem { dst, lhs, rhs }, len)
                } else {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vmulsd form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
            } else if mnemonic == "vdivsd" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vdivsd operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                if parts[2].contains('[') {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vdivsd memory form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let rhs = parse_xmm_register(parts[2])?;
                (Decoded::Vdivsd { dst, lhs, rhs }, len)
            } else if mnemonic == "vmovups" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 2 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vmovups operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                if parts[0].contains('[') {
                    let dst = parse_capstone_memory_operand(parts[0])?;
                    let src = parse_xmm_register(parts[1])?;
                    (Decoded::VmovupsStore { dst, src }, len)
                } else if parts[1].contains('[') {
                    let dst = parse_xmm_register(parts[0])?;
                    let src = parse_capstone_memory_operand(parts[1])?;
                    (Decoded::VmovupsLoad { dst, src }, len)
                } else {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vmovups form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
            } else if mnemonic == "vmovss" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 2 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vmovss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                if parts[0].contains('[') {
                    let dst = parse_capstone_memory_operand(parts[0])?;
                    let src = parse_xmm_register(parts[1])?;
                    (Decoded::VmovssStore { dst, src }, len)
                } else if parts[1].contains('[') {
                    let dst = parse_xmm_register(parts[0])?;
                    let src = parse_capstone_memory_operand(parts[1])?;
                    (Decoded::VmovssLoad { dst, src }, len)
                } else {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vmovss form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
            } else if mnemonic == "vcomiss" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 2 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vcomiss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let lhs = parse_xmm_register(parts[0])?;
                if parts[1].contains('[') {
                    let rhs = parse_capstone_memory_operand(parts[1])?;
                    (Decoded::VcomissMem { lhs, rhs }, len)
                } else {
                    let rhs = parse_xmm_register(parts[1])?;
                    (Decoded::VcomissReg { lhs, rhs }, len)
                }
            } else if mnemonic == "vaddss" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vaddss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                if parts[2].contains('[') {
                    let rhs = parse_capstone_memory_operand(parts[2])?;
                    (Decoded::VaddssMem { dst, lhs, rhs }, len)
                } else {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vaddss form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
            } else if mnemonic == "vdivss" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vdivss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                if parts[2].contains('[') {
                    let rhs = parse_capstone_memory_operand(parts[2])?;
                    (Decoded::VdivssMem { dst, lhs, rhs }, len)
                } else {
                    let rhs = parse_xmm_register(parts[2])?;
                    (Decoded::Vdivss { dst, lhs, rhs }, len)
                }
            } else if mnemonic == "vcvttss2usi" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 2 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vcvttss2usi operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_gpr_register(parts[0])?;
                let src = parse_xmm_register(parts[1])?;
                let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(64);
                (
                    Decoded::Vcvttss2usi {
                        dst,
                        src,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "vmulss" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 vmulss operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let lhs = parse_xmm_register(parts[1])?;
                if parts[2].contains('[') {
                    let rhs = parse_capstone_memory_operand(parts[2])?;
                    (Decoded::VmulssMem { dst, lhs, rhs }, len)
                } else {
                    let rhs = parse_xmm_register(parts[2])?;
                    (Decoded::Vmulss { dst, lhs, rhs }, len)
                }
            } else if matches!(mnemonic, "vpinsrq" | "pinsrq") {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 4 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 pinsrq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_xmm_register(parts[0])?;
                let vector = parse_xmm_register(parts[1])?;
                let value = if parts[2].contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    RmOperand::Reg(parse_gpr_register(parts[2])?)
                };
                let lane = parts[3]
                    .parse::<u8>()
                    .map_err(|e| Error::from(format!("invalid pinsrq lane immediate: {e}")))?;
                (
                    Decoded::Pinsrq {
                        dst,
                        vector,
                        value,
                        lane,
                    },
                    len,
                )
            } else if matches!(mnemonic, "vpextrq" | "pextrq") {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 pextrq operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_gpr_register(parts[0])?;
                let src = parse_xmm_register(parts[1])?;
                let lane = parse_capstone_immediate(parts[2])?;
                if !(0..=1).contains(&lane) {
                    return Err(Error::from("invalid pextrq lane immediate"));
                }
                (
                    Decoded::Pextrq {
                        dst,
                        src,
                        lane: lane as u8,
                    },
                    len,
                )
            } else if matches!(mnemonic, "movq" | "vmovq") {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                if lhs.starts_with("xmm") {
                    let dst = parse_xmm_register(lhs)?;
                    if rhs.contains('[') {
                        let src = parse_capstone_memory_operand(rhs)?;
                        (Decoded::MovqXmmFromMem { dst, src }, len)
                    } else {
                        let src = parse_gpr_register(rhs)?;
                        (Decoded::MovqXmmFromGpr { dst, src }, len)
                    }
                } else if rhs.starts_with("xmm") {
                    let src = parse_xmm_register(rhs)?;
                    if lhs.contains('[') {
                        let dst = parse_capstone_memory_operand(lhs)?;
                        (Decoded::MovqMemFromXmm { dst, src }, len)
                    } else {
                        let dst = parse_gpr_register(lhs)?;
                        (Decoded::MovqGprFromXmm { dst, src }, len)
                    }
                } else {
                    return Err(Error::from(format!(
                        "unsupported x86_64 movq form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
            } else if mnemonic == "vzeroupper"
                || mnemonic.starts_with("vmovaps")
                || mnemonic.starts_with("vmovdqa")
                || mnemonic.starts_with("vmovdqu")
            {
                // TODO: Lift SIMD moves/spills properly.
                (Decoded::Nop, len)
            } else if mnemonic == "cmp" {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                let width_bits = capstone_operand_width_bits(lhs)
                    .or_else(|| capstone_operand_width_bits(rhs))
                    .unwrap_or(64);

                let lhs_operand = if lhs.contains('[') {
                    Operand::Rm(RmOperand::Mem(parse_capstone_memory_operand(lhs)?))
                } else {
                    Operand::Rm(RmOperand::Reg(parse_gpr_register(lhs)?))
                };

                let rhs_operand = if rhs.contains('[') {
                    Operand::Rm(RmOperand::Mem(parse_capstone_memory_operand(rhs)?))
                } else if rhs.starts_with("0x")
                    || rhs.starts_with('-')
                    || rhs.chars().next().is_some_and(|c| c.is_ascii_digit())
                {
                    Operand::Imm(parse_capstone_immediate(rhs)?)
                } else {
                    Operand::Rm(RmOperand::Reg(parse_gpr_register(rhs)?))
                };

                (
                    Decoded::Cmp {
                        lhs: lhs_operand,
                        rhs: rhs_operand,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "test" {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                let width_bits = capstone_operand_width_bits(lhs)
                    .or_else(|| capstone_operand_width_bits(rhs))
                    .unwrap_or(64);

                let lhs_operand = if lhs.contains('[') {
                    Operand::Rm(RmOperand::Mem(parse_capstone_memory_operand(lhs)?))
                } else {
                    Operand::Rm(RmOperand::Reg(parse_gpr_register(lhs)?))
                };

                let rhs_operand = if rhs.contains('[') {
                    Operand::Rm(RmOperand::Mem(parse_capstone_memory_operand(rhs)?))
                } else if rhs.starts_with("0x")
                    || rhs.starts_with('-')
                    || rhs.chars().next().is_some_and(|c| c.is_ascii_digit())
                {
                    Operand::Imm(parse_capstone_immediate(rhs)?)
                } else {
                    Operand::Rm(RmOperand::Reg(parse_gpr_register(rhs)?))
                };

                (
                    Decoded::Test {
                        lhs: lhs_operand,
                        rhs: rhs_operand,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "bt" {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                if lhs.contains('[') || rhs.contains('[') {
                    return Err(Error::from(format!(
                        "unsupported x86_64 bt memory form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let value = parse_gpr_register(lhs)?;
                if rhs.starts_with("0x")
                    || rhs
                        .chars()
                        .next()
                        .map(|c| c.is_ascii_digit())
                        .unwrap_or(false)
                {
                    let imm = parse_capstone_immediate(rhs)?;
                    let imm = u8::try_from(imm)
                        .map_err(|_| Error::from(format!("unsupported bt immediate: {imm}")))?;
                    (Decoded::BtImm { value, imm }, len)
                } else {
                    let bit = parse_gpr_register(rhs)?;
                    (Decoded::BtReg { value, bit }, len)
                }
            } else if mnemonic == "btc" {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                let width_bits = capstone_operand_width_bits(lhs).unwrap_or(64);
                let dst = if lhs.contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(lhs)?)
                } else {
                    RmOperand::Reg(parse_gpr_register(lhs)?)
                };
                let imm = parse_capstone_immediate(rhs)?;
                let imm = u8::try_from(imm)
                    .map_err(|_| Error::from(format!("unsupported btc immediate: {imm}")))?;
                (
                    Decoded::BtcImm {
                        dst,
                        imm,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "cqo" {
                (Decoded::Cqo, len)
            } else if mnemonic == "cdq" {
                (Decoded::Cdq, len)
            } else if mnemonic == "cdqe" {
                (Decoded::Cdqe, len)
            } else if matches!(mnemonic, "shl" | "sal" | "shr" | "sar") {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                let width_bits = capstone_operand_width_bits(lhs)
                    .or_else(|| capstone_operand_width_bits(rhs))
                    .unwrap_or(64);
                let dst = if lhs.contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(lhs)?)
                } else {
                    RmOperand::Reg(parse_gpr_register(lhs)?)
                };
                let imm = parse_capstone_immediate(rhs)?;
                let imm = u8::try_from(imm)
                    .map_err(|e| Error::from(format!("unsupported x86_64 shift immediate: {e}")))?;
                if matches!(mnemonic, "shl" | "sal") {
                    (
                        Decoded::ShlImm {
                            dst,
                            imm,
                            width_bits,
                        },
                        len,
                    )
                } else if mnemonic == "sar" {
                    (
                        Decoded::SarImm {
                            dst,
                            imm,
                            width_bits,
                        },
                        len,
                    )
                } else {
                    (
                        Decoded::ShrImm {
                            dst,
                            imm,
                            width_bits,
                        },
                        len,
                    )
                }
            } else if mnemonic == "shrx" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 shrx form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let width_bits = capstone_operand_width_bits(parts[0])
                    .or_else(|| capstone_operand_width_bits(parts[1]))
                    .unwrap_or(64);
                let dst = parse_gpr_register(parts[0])?;
                let src = if parts[1].contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(parts[1])?)
                } else {
                    RmOperand::Reg(parse_gpr_register(parts[1])?)
                };
                let shift = if parts[2].contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    RmOperand::Reg(parse_gpr_register(parts[2])?)
                };
                (
                    Decoded::Shrx {
                        dst,
                        src,
                        shift,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "shlx" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 shlx form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let width_bits = capstone_operand_width_bits(parts[0])
                    .or_else(|| capstone_operand_width_bits(parts[1]))
                    .unwrap_or(64);
                let dst = parse_gpr_register(parts[0])?;
                let src = if parts[1].contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(parts[1])?)
                } else {
                    RmOperand::Reg(parse_gpr_register(parts[1])?)
                };
                let shift = if parts[2].contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(parts[2])?)
                } else {
                    RmOperand::Reg(parse_gpr_register(parts[2])?)
                };
                (
                    Decoded::Shlx {
                        dst,
                        src,
                        shift,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "rorx" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 3 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 rorx form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let width_bits = capstone_operand_width_bits(parts[0])
                    .or_else(|| capstone_operand_width_bits(parts[1]))
                    .unwrap_or(64);
                if !matches!(width_bits, 32 | 64) {
                    return Err(Error::from(format!(
                        "unsupported x86_64 rorx width at 0x{offset:x}: {width_bits}"
                    )));
                }
                let dst = parse_gpr_register(parts[0])?;
                let src = if parts[1].contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(parts[1])?)
                } else {
                    RmOperand::Reg(parse_gpr_register(parts[1])?)
                };
                let imm = parse_capstone_immediate(parts[2])?;
                let imm = u16::try_from(imm)
                    .map_err(|_| Error::from(format!("unsupported rorx immediate: {imm}")))?;
                (
                    Decoded::Rorx {
                        dst,
                        src,
                        imm,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "blsr" {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                let width_bits = capstone_operand_width_bits(lhs)
                    .or_else(|| capstone_operand_width_bits(rhs))
                    .unwrap_or(64);
                if lhs.contains('[') {
                    return Err(Error::from(format!(
                        "unsupported x86_64 blsr destination at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_gpr_register(lhs)?;
                let src = if rhs.contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(rhs)?)
                } else {
                    RmOperand::Reg(parse_gpr_register(rhs)?)
                };
                (
                    Decoded::Blsr {
                        dst,
                        src,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "not" {
                let operand = op_str.trim();
                if operand.is_empty() {
                    return Err(Error::from(format!(
                        "unsupported x86_64 not form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let width_bits = capstone_operand_width_bits(operand).unwrap_or(64);
                let dst = if operand.contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(operand)?)
                } else {
                    RmOperand::Reg(parse_gpr_register(operand)?)
                };
                (Decoded::NotRm { dst, width_bits }, len)
            } else if mnemonic == "neg" {
                let operand = op_str.trim();
                if operand.is_empty() {
                    return Err(Error::from(format!(
                        "unsupported x86_64 neg form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let width_bits = capstone_operand_width_bits(operand).unwrap_or(64);
                let dst = if operand.contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(operand)?)
                } else {
                    RmOperand::Reg(parse_gpr_register(operand)?)
                };
                (Decoded::NegRm { dst, width_bits }, len)
            } else if mnemonic == "sbb" {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                if lhs.contains('[') || rhs.contains('[') {
                    return Err(Error::from(format!(
                        "unsupported x86_64 sbb memory form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_gpr_register(lhs)?;
                let src = parse_gpr_register(rhs)?;
                if dst != src {
                    return Err(Error::from(format!(
                        "unsupported x86_64 sbb form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let width_bits = capstone_operand_width_bits(lhs)
                    .or_else(|| capstone_operand_width_bits(rhs))
                    .unwrap_or(64);
                (
                    Decoded::SbbSelf {
                        reg: dst,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "sub" {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                if lhs.contains('[') {
                    return Err(Error::from(format!(
                        "unsupported x86_64 sub memory-destination form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_gpr_register(lhs)?;
                let width_bits = capstone_operand_width_bits(lhs)
                    .or_else(|| capstone_operand_width_bits(rhs))
                    .unwrap_or(64);
                let src = if rhs.contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(rhs)?)
                } else {
                    RmOperand::Reg(parse_gpr_register(rhs)?)
                };
                (
                    Decoded::SubRegRmWidth {
                        dst,
                        src,
                        width_bits,
                    },
                    len,
                )
            } else if matches!(mnemonic, "or" | "and" | "xor") {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                let width_bits = capstone_operand_width_bits(lhs)
                    .or_else(|| capstone_operand_width_bits(rhs))
                    .unwrap_or(64);
                let dst = if lhs.contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(lhs)?)
                } else {
                    RmOperand::Reg(parse_gpr_register(lhs)?)
                };
                if rhs.starts_with("0x")
                    || rhs.starts_with('-')
                    || rhs.chars().next().is_some_and(|c| c.is_ascii_digit())
                {
                    let imm = parse_capstone_immediate(rhs)?;
                    if mnemonic == "or" {
                        (
                            Decoded::OrImmRm {
                                dst,
                                imm,
                                width_bits,
                            },
                            len,
                        )
                    } else if mnemonic == "and" {
                        (
                            Decoded::AndImmRm {
                                dst,
                                imm,
                                width_bits,
                            },
                            len,
                        )
                    } else {
                        (
                            Decoded::XorImm {
                                dst,
                                imm,
                                width_bits,
                            },
                            len,
                        )
                    }
                } else if rhs.contains('[') {
                    return Err(Error::from(format!(
                        "unsupported x86_64 {mnemonic} memory-source form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                } else {
                    let src = parse_gpr_register(rhs)?;
                    match (mnemonic, dst) {
                        ("or", RmOperand::Reg(dst)) => (
                            Decoded::OrReg {
                                dst,
                                src,
                                width_bits,
                            },
                            len,
                        ),
                        ("and", RmOperand::Reg(dst)) => (
                            Decoded::AndReg {
                                dst,
                                src,
                                width_bits,
                            },
                            len,
                        ),
                        ("xor", RmOperand::Reg(dst)) => (
                            Decoded::XorReg {
                                dst,
                                src,
                                width_bits,
                            },
                            len,
                        ),
                        ("or", dst) => (
                            Decoded::OrRmReg {
                                dst,
                                src,
                                width_bits,
                            },
                            len,
                        ),
                        ("and", dst) => (
                            Decoded::AndRmReg {
                                dst,
                                src,
                                width_bits,
                            },
                            len,
                        ),
                        _ => {
                            return Err(Error::from(format!(
                                "unsupported x86_64 xor destination at 0x{offset:x}: {mnemonic} {op_str}"
                            )));
                        }
                    }
                }
            } else if mnemonic == "imul" {
                let parts = parse_capstone_operands(op_str);
                match parts.len() {
                    1 => {
                        let src_text = parts[0];
                        let width_bits = capstone_operand_width_bits(src_text).unwrap_or(64);
                        let src = if src_text.contains('[') {
                            RmOperand::Mem(parse_capstone_memory_operand(src_text)?)
                        } else {
                            RmOperand::Reg(parse_gpr_register(src_text)?)
                        };
                        (Decoded::ImulRmWide { src, width_bits }, len)
                    }
                    2 => {
                        let lhs = parts[0];
                        let rhs = parts[1];
                        if lhs.contains('[') || rhs.contains('[') {
                            return Err(Error::from(format!(
                                "unsupported x86_64 imul memory form at 0x{offset:x}: {mnemonic} {op_str}"
                            )));
                        }
                        let dst = parse_gpr_register(lhs)?;
                        let src = parse_gpr_register(rhs)?;
                        let width_bits = capstone_operand_width_bits(lhs)
                            .or_else(|| capstone_operand_width_bits(rhs))
                            .unwrap_or(64);
                        (
                            Decoded::ImulReg {
                                dst,
                                src,
                                width_bits,
                            },
                            len,
                        )
                    }
                    3 => {
                        let dst_text = parts[0];
                        let src_text = parts[1];
                        let imm_text = parts[2];
                        if dst_text.contains('[') {
                            return Err(Error::from(format!(
                                "unsupported x86_64 imul memory form at 0x{offset:x}: {mnemonic} {op_str}"
                            )));
                        }
                        let dst = parse_gpr_register(dst_text)?;
                        let src = if src_text.contains('[') {
                            RmOperand::Mem(parse_capstone_memory_operand(src_text)?)
                        } else {
                            RmOperand::Reg(parse_gpr_register(src_text)?)
                        };
                        let imm = parse_capstone_immediate(imm_text)?;
                        let width_bits = capstone_operand_width_bits(dst_text)
                            .or_else(|| capstone_operand_width_bits(src_text))
                            .unwrap_or(64);
                        (
                            Decoded::ImulRegImm {
                                dst,
                                src,
                                imm,
                                width_bits,
                            },
                            len,
                        )
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported x86_64 imul operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                }
            } else if mnemonic == "mul" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 1 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 mul operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let src_text = parts[0];
                let width_bits = capstone_operand_width_bits(src_text).unwrap_or(64);
                let src = if src_text.contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(src_text)?)
                } else {
                    RmOperand::Reg(parse_gpr_register(src_text)?)
                };
                (Decoded::MulRm { src, width_bits }, len)
            } else if mnemonic == "fild" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 1 || !parts[0].contains('[') {
                    return Err(Error::from(format!(
                        "unsupported x86_64 fild form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let src = parse_capstone_memory_operand(parts[0])?;
                let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(64);
                (Decoded::Fild { src, width_bits }, len)
            } else if mnemonic == "fld" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 1 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 fld operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                if parts[0].contains('[') {
                    let src = parse_capstone_memory_operand(parts[0])?;
                    let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(80);
                    (Decoded::FldMem { src, width_bits }, len)
                } else {
                    let index = parse_st_register(parts[0])?;
                    (Decoded::FldSt { index }, len)
                }
            } else if mnemonic == "fxch" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 1 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 fxch operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let index = parse_st_register(parts[0])?;
                (Decoded::Fxch { index }, len)
            } else if mnemonic == "fdivrp" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 2 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 fdivrp operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let a = parse_st_register(parts[0])?;
                let b = parse_st_register(parts[1])?;
                let index = if a != 0 { a } else { b };
                (Decoded::Fdivrp { index }, len)
            } else if mnemonic == "fdivp" {
                let parts = parse_capstone_operands(op_str);
                let index = match parts.as_slice() {
                    [single] => parse_st_register(single)?,
                    [a, b] => {
                        let a = parse_st_register(a)?;
                        let b = parse_st_register(b)?;
                        if a != 0 { a } else { b }
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fdivp operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                };
                (Decoded::Fdivp { index }, len)
            } else if mnemonic == "fmulp" {
                let parts = parse_capstone_operands(op_str);
                let index = match parts.as_slice() {
                    [single] => parse_st_register(single)?,
                    [a, b] => {
                        let a = parse_st_register(a)?;
                        let b = parse_st_register(b)?;
                        if a != 0 { a } else { b }
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fmulp operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                };
                (Decoded::Fmulp { index }, len)
            } else if mnemonic == "fmul" {
                let parts = parse_capstone_operands(op_str);
                let index = match parts.as_slice() {
                    [single] => parse_st_register(single)?,
                    [a, b] => {
                        let a = parse_st_register(a)?;
                        let b = parse_st_register(b)?;
                        if a != 0 { a } else { b }
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fmul operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                };
                (Decoded::FmulSt0St { index }, len)
            } else if mnemonic == "fcomi" {
                let parts = parse_capstone_operands(op_str);
                let index = match parts.as_slice() {
                    [single] => parse_st_register(single)?,
                    [a, b] => {
                        let a = parse_st_register(a)?;
                        let b = parse_st_register(b)?;
                        if a != 0 { a } else { b }
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fcomi operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                };
                (Decoded::Fcomi { index }, len)
            } else if mnemonic == "fcomip"
                || mnemonic == "fcompi"
                || mnemonic == "fucomip"
                || mnemonic == "fucompi"
            {
                let parts = parse_capstone_operands(op_str);
                let index = match parts.as_slice() {
                    [single] => parse_st_register(single)?,
                    [a, b] => {
                        let a = parse_st_register(a)?;
                        let b = parse_st_register(b)?;
                        if a != 0 { a } else { b }
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fcomip operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                };
                (Decoded::Fcomip { index }, len)
            } else if mnemonic == "fstp" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 1 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 fstp operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                if parts[0].contains('[') {
                    let dst = parse_capstone_memory_operand(parts[0])?;
                    let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(64);
                    (Decoded::FstpMem { dst, width_bits }, len)
                } else {
                    let index = parse_st_register(parts[0])?;
                    (Decoded::FstpSt { index }, len)
                }
            } else if mnemonic == "fisttp" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 1 || !parts[0].contains('[') {
                    return Err(Error::from(format!(
                        "unsupported x86_64 fisttp form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_capstone_memory_operand(parts[0])?;
                let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(64);
                (Decoded::Fisttp { dst, width_bits }, len)
            } else if let Some(condition) = mnemonic.strip_prefix("fcmov").and_then(|suffix| {
                Some(match suffix {
                    "b" | "c" | "nae" => 0x2,
                    "ae" | "nb" | "nc" => 0x3,
                    "be" | "na" => 0x6,
                    "a" | "nbe" => 0x7,
                    "e" | "z" => 0x4,
                    "ne" | "nz" => 0x5,
                    _ => return None,
                })
            }) {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 2 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 fcmov operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let dst = parse_st_register(parts[0])?;
                if dst != 0 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 fcmov dst at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let src = parse_st_register(parts[1])?;
                (Decoded::Fcmovcc { condition, src }, len)
            } else if mnemonic == "fadd" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 1 || !parts[0].contains('[') {
                    return Err(Error::from(format!(
                        "unsupported x86_64 fadd form at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let src = parse_capstone_memory_operand(parts[0])?;
                let width_bits = capstone_operand_width_bits(parts[0]).unwrap_or(64);
                (Decoded::FaddMem { src, width_bits }, len)
            } else if mnemonic == "ffreep" {
                let parts = parse_capstone_operands(op_str);
                if parts.len() != 1 {
                    return Err(Error::from(format!(
                        "unsupported x86_64 ffreep operand count at 0x{offset:x}: {mnemonic} {op_str}"
                    )));
                }
                let index = parse_st_register(parts[0])?;
                (Decoded::Ffreep { index }, len)
            } else if mnemonic == "fsubr" {
                let parts = parse_capstone_operands(op_str);
                let index = match parts.as_slice() {
                    [single] => parse_st_register(single)?,
                    [a, b] => {
                        let a = parse_st_register(a)?;
                        let b = parse_st_register(b)?;
                        if a != 0 { a } else { b }
                    }
                    _ => {
                        return Err(Error::from(format!(
                            "unsupported x86_64 fsubr operand count at 0x{offset:x}: {mnemonic} {op_str}"
                        )));
                    }
                };
                (Decoded::FsubrSt0St { index }, len)
            } else if let Some(condition) = mnemonic.strip_prefix("cmov").and_then(|suffix| {
                Some(match suffix {
                    "e" | "z" => 0x4,
                    "ne" | "nz" => 0x5,
                    "b" | "c" | "nae" => 0x2,
                    "ae" | "nb" | "nc" => 0x3,
                    "be" | "na" => 0x6,
                    "a" | "nbe" => 0x7,
                    // `s/ns` are based on the sign flag. We approximate them as
                    // `lt/ge` (valid for common patterns where OF=0, e.g. `test`).
                    "s" => 0xC,
                    "ns" => 0xD,
                    "l" => 0xC,
                    "ge" => 0xD,
                    "le" => 0xE,
                    "g" => 0xF,
                    _ => return None,
                })
            }) {
                let (lhs, rhs) = parse_capstone_two_operands(op_str)?;
                let dst = parse_gpr_register(lhs)?;
                let src = if rhs.contains('[') {
                    RmOperand::Mem(parse_capstone_memory_operand(rhs)?)
                } else {
                    RmOperand::Reg(parse_gpr_register(rhs)?)
                };
                let width_bits = capstone_operand_width_bits(lhs)
                    .or_else(|| capstone_operand_width_bits(rhs))
                    .unwrap_or(64);
                (
                    Decoded::Cmovcc {
                        dst,
                        src,
                        condition,
                        width_bits,
                    },
                    len,
                )
            } else if mnemonic == "hlt" {
                (Decoded::Hlt, len)
            } else if mnemonic == "leave" {
                (Decoded::Leave, len)
            } else {
                if let Some(err) = decode_error {
                    return Err(Error::from(format!(
                        "unsupported x86_64 instruction at 0x{offset:x}: {mnemonic} {op_str} (custom decode failed: {err})"
                    )));
                }
                return Err(Error::from(format!(
                    "unsupported x86_64 instruction at 0x{offset:x}: {mnemonic} {op_str}"
                )));
            }
        };
        if consumed != len {
            return Err(Error::from(format!(
                "x86_64 decode length mismatch at 0x{offset:x}: capstone={len} custom={consumed}"
            )));
        }

        decoded.push(DecodedInstruction { offset, len, kind });
    }

    Ok(decoded)
}
