use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Decoded {
    Nop,
    Ret,
    Hlt,
    Leave,
    PushReg {
        src: u8,
    },
    PushImm {
        imm: i64,
    },
    PushRm {
        src: RmOperand,
    },
    PopReg {
        dst: u8,
    },
    XorReg {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    XorImm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    AndReg {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    AndRmToReg {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    OrReg {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    OrRmToReg {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    OrRmReg {
        dst: RmOperand,
        src: u8,
        width_bits: u16,
    },
    AndRmReg {
        dst: RmOperand,
        src: u8,
        width_bits: u16,
    },
    XorRmToReg {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    OrImm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    AndImm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    AdcImm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    SbbImm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    AddRegRm {
        dst: u8,
        src: RmOperand,
    },
    AddRmReg {
        dst: RmOperand,
        src: u8,
        width_bits: u16,
    },
    SubRegRm {
        dst: u8,
        src: RmOperand,
    },
    SubRegRmWidth {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    SubRmReg {
        dst: RmOperand,
        src: u8,
        width_bits: u16,
    },
    AddImm {
        dst: u8,
        imm: i64,
        width_bits: u16,
    },
    AddImmRm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    SubImm {
        dst: u8,
        imm: i64,
        width_bits: u16,
    },
    SubImmRm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    Cmp {
        lhs: Operand,
        rhs: Operand,
        width_bits: u16,
    },
    Test {
        lhs: Operand,
        rhs: Operand,
        width_bits: u16,
    },
    MovImm64 {
        dst: u8,
        imm_offset: usize,
        imm: i64,
    },
    MovImm32ToRm {
        dst: RmOperand,
        imm_offset: usize,
        imm: i32,
    },
    MovImm32ToMem64 {
        dst: X86Memory,
        imm_offset: usize,
        imm: i32,
    },
    MovImm8ToRm {
        dst: RmOperand,
        imm: i8,
    },
    MovImm16ToRm {
        dst: RmOperand,
        imm_offset: usize,
        imm: u16,
    },
    MovSxd {
        dst: u8,
        src: RmOperand,
    },
    MovSx {
        dst: u8,
        src: RmOperand,
        src_width_bits: u16,
        dst_width_bits: u16,
    },
    DivRm {
        src: RmOperand,
        signed: bool,
        width_bits: u16,
    },
    Lea {
        dst: u8,
        src: X86Memory,
        width_bits: u16,
    },
    MovRmToReg {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    MovRegToRm {
        dst: RmOperand,
        src: u8,
        width_bits: u16,
    },
    MovbeRegFromMem {
        dst: u8,
        src: X86Memory,
        width_bits: u16,
    },
    MovbeMemFromReg {
        dst: X86Memory,
        src: u8,
        width_bits: u16,
    },
    Bswap {
        dst: u8,
        width_bits: u16,
    },
    CallRel32 {
        imm_offset: usize,
        target: u64,
    },
    CallRm {
        target: RmOperand,
    },
    IncRm {
        target: RmOperand,
        width_bits: u16,
    },
    DecRm {
        target: RmOperand,
        width_bits: u16,
    },
    JmpRel {
        target: u64,
    },
    JmpRm {
        target: RmOperand,
    },
    JccRel {
        condition: u8,
        target: u64,
    },
    Syscall,
    Vpbroadcastq {
        dst: u8,
        src: u8,
    },
    ZeroXmm {
        dst: u8,
    },
    OnesXmm {
        dst: u8,
    },
    Vcvtusi2sd {
        dst: u8,
        src_vec: u8,
        src_gpr: RmOperand,
        width_bits: u16,
    },
    Vcvtusi2ss {
        dst: u8,
        src_vec: u8,
        src_gpr: RmOperand,
        width_bits: u16,
    },
    VmulsdMem {
        dst: u8,
        lhs: u8,
        rhs: X86Memory,
    },
    Vdivsd {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    VmovupsStore {
        dst: X86Memory,
        src: u8,
    },
    VmovupsLoad {
        dst: u8,
        src: X86Memory,
    },
    VmovssLoad {
        dst: u8,
        src: X86Memory,
    },
    VmovssStore {
        dst: X86Memory,
        src: u8,
    },
    VcomissMem {
        lhs: u8,
        rhs: X86Memory,
    },
    VcomissReg {
        lhs: u8,
        rhs: u8,
    },
    VaddssMem {
        dst: u8,
        lhs: u8,
        rhs: X86Memory,
    },
    Vdivss {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    VdivssMem {
        dst: u8,
        lhs: u8,
        rhs: X86Memory,
    },
    Vcvttss2usi {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    Vmulss {
        dst: u8,
        lhs: u8,
        rhs: u8,
    },
    VmulssMem {
        dst: u8,
        lhs: u8,
        rhs: X86Memory,
    },
    VpxorqXmmMem {
        dst: u8,
        lhs: u8,
        rhs: X86Memory,
    },
    Vptest {
        lhs: u8,
        rhs: u8,
    },
    VptestMem {
        lhs: u8,
        rhs: X86Memory,
    },
    Vpalignr {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
        imm: u8,
    },
    Vpmaxsq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpmaxuq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpmaxud {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpminuq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpsubq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpaddd {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpaddq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpsrldq {
        dst: u8,
        src: u8,
        imm: u8,
    },
    Vpandq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vporq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpunpcklwd {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpunpckldq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    Vpunpcklqdq {
        dst: u8,
        lhs: u8,
        rhs: VecOperand,
    },
    MovdXmmFromGpr32 {
        dst: u8,
        src: u8,
    },
    MovdXmmFromMem32 {
        dst: u8,
        src: X86Memory,
    },
    MovdMem32FromXmm {
        dst: X86Memory,
        src: u8,
    },
    MovdGpr32FromXmm {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    Pinsrd {
        dst: u8,
        vector: u8,
        value: RmOperand,
        lane: u8,
    },
    Pinsrb {
        dst: u8,
        vector: u8,
        value: RmOperand,
        lane: u8,
    },
    MovqXmmFromMem {
        dst: u8,
        src: X86Memory,
    },
    MovqXmmFromGpr {
        dst: u8,
        src: u8,
    },
    MovqMemFromXmm {
        dst: X86Memory,
        src: u8,
    },
    MovqGprFromXmm {
        dst: u8,
        src: u8,
    },
    Pinsrq {
        dst: u8,
        vector: u8,
        value: RmOperand,
        lane: u8,
    },
    Pextrq {
        dst: u8,
        src: u8,
        lane: u8,
    },
    BtReg {
        value: u8,
        bit: u8,
    },
    BtImm {
        value: u8,
        imm: u8,
    },
    BtcImm {
        dst: RmOperand,
        imm: u8,
        width_bits: u16,
    },
    Cqo,
    Cdq,
    Cdqe,
    ShlImm {
        dst: RmOperand,
        imm: u8,
        width_bits: u16,
    },
    ShrImm {
        dst: RmOperand,
        imm: u8,
        width_bits: u16,
    },
    Shrx {
        dst: u8,
        src: RmOperand,
        shift: RmOperand,
        width_bits: u16,
    },
    Shlx {
        dst: u8,
        src: RmOperand,
        shift: RmOperand,
        width_bits: u16,
    },
    Rorx {
        dst: u8,
        src: RmOperand,
        imm: u16,
        width_bits: u16,
    },
    Blsr {
        dst: u8,
        src: RmOperand,
        width_bits: u16,
    },
    SarImm {
        dst: RmOperand,
        imm: u8,
        width_bits: u16,
    },
    NotRm {
        dst: RmOperand,
        width_bits: u16,
    },
    NegRm {
        dst: RmOperand,
        width_bits: u16,
    },
    SbbSelf {
        reg: u8,
        width_bits: u16,
    },
    OrImmRm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    AndImmRm {
        dst: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    ImulReg {
        dst: u8,
        src: u8,
        width_bits: u16,
    },
    ImulRegImm {
        dst: u8,
        src: RmOperand,
        imm: i64,
        width_bits: u16,
    },
    ImulRmWide {
        src: RmOperand,
        width_bits: u16,
    },
    MulRm {
        src: RmOperand,
        width_bits: u16,
    },
    Fild {
        src: X86Memory,
        width_bits: u16,
    },
    FldSt {
        index: u8,
    },
    FldMem {
        src: X86Memory,
        width_bits: u16,
    },
    Fxch {
        index: u8,
    },
    Fdivrp {
        index: u8,
    },
    Fdivp {
        index: u8,
    },
    Fmulp {
        index: u8,
    },
    FmulSt0St {
        index: u8,
    },
    Fcomi {
        index: u8,
    },
    Fcomip {
        index: u8,
    },
    FstpSt {
        index: u8,
    },
    FstpMem {
        dst: X86Memory,
        width_bits: u16,
    },
    Fisttp {
        dst: X86Memory,
        width_bits: u16,
    },
    FaddMem {
        src: X86Memory,
        width_bits: u16,
    },
    Ffreep {
        index: u8,
    },
    FsubrSt0St {
        index: u8,
    },
    Fcmovcc {
        condition: u8,
        src: u8,
    },
    Cmovcc {
        dst: u8,
        src: RmOperand,
        condition: u8,
        width_bits: u16,
    },
    Setcc {
        dst: RmOperand,
        condition: u8,
    },
    MovZx {
        dst: u8,
        src: RmOperand,
        src_width_bits: u16,
        dst_width_bits: u16,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Operand {
    Rm(RmOperand),
    Imm(i64),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum RmOperand {
    Reg(u8),
    Mem(X86Memory),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum VecOperand {
    Reg(u8),
    Mem(X86Memory),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct X86Memory {
    pub(super) base: Option<u8>,
    pub(super) index: Option<u8>,
    pub(super) scale: u8,
    pub(super) displacement: i64,
    pub(super) displacement_offset: Option<usize>,
    pub(super) segment: Option<X86Segment>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum X86Segment {
    Fs,
    Gs,
}

pub(super) fn decode_instruction(bytes: &[u8], offset: u64) -> Result<Option<(Decoded, usize)>> {
    if bytes.is_empty() {
        return Ok(None);
    }

    // Intel CET indirect branch tracking marker.
    // Treat as a NOP for lifting purposes.
    if bytes.starts_with(&[0xF3, 0x0F, 0x1E, 0xFA]) {
        return Ok(Some((Decoded::Nop, 4)));
    }

    let mut segment = None;
    let mut operand_size_override = false;
    let mut opcode_index = 0usize;
    while let Some(prefix) = bytes.get(opcode_index).copied() {
        match prefix {
            // Branch hints / segment overrides that show up in CET-enabled code.
            0x2E | 0x3E | 0x26 | 0x36 => {
                opcode_index += 1;
            }
            0x64 => {
                segment = Some(X86Segment::Fs);
                opcode_index += 1;
            }
            0x65 => {
                segment = Some(X86Segment::Gs);
                opcode_index += 1;
            }
            0x66 => {
                operand_size_override = true;
                opcode_index += 1;
            }
            0xF0 | 0xF2 | 0xF3 | 0x67 => {
                opcode_index += 1;
            }
            _ => break,
        }
    }

    // Minimal prefix handling: accept a REX prefix (0x40..0x4F).
    let (rex_w, rex_r, rex_x, rex_b, opcode_index) = match bytes.get(opcode_index).copied() {
        Some(rex @ 0x40..=0x4F) => (
            ((rex >> 3) & 1) != 0,
            ((rex >> 2) & 1) != 0,
            ((rex >> 1) & 1) != 0,
            (rex & 1) != 0,
            opcode_index + 1,
        ),
        _ => (false, false, false, false, opcode_index),
    };
    let Some(opcode) = bytes.get(opcode_index).copied() else {
        return Ok(None);
    };

    if opcode == 0x90 {
        return Ok(Some((Decoded::Nop, opcode_index + 1)));
    }
    if opcode == 0xC3 {
        return Ok(Some((Decoded::Ret, opcode_index + 1)));
    }

    // PUSH imm8/imm32.
    if opcode == 0x6A {
        let imm = *bytes
            .get(opcode_index + 1)
            .ok_or_else(|| Error::from("truncated push imm8"))? as i8;
        return Ok(Some((
            Decoded::PushImm { imm: imm as i64 },
            opcode_index + 2,
        )));
    }
    if opcode == 0x68 {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::PushImm { imm: imm as i64 },
            opcode_index + 5,
        )));
    }

    // Unsigned/signed division: F7 /6 (div) and F7 /7 (idiv).
    if opcode == 0xF7 {
        let (ext, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        if ext == 6 || ext == 7 {
            return Ok(Some((
                Decoded::DivRm {
                    src: rm,
                    signed: ext == 7,
                    width_bits: if rex_w { 64 } else { 32 },
                },
                opcode_index + 1 + consumed,
            )));
        }
    }

    // PUSH/POP r64.
    if (0x50..=0x57).contains(&opcode) {
        let mut src = opcode - 0x50;
        if rex_b {
            src = src.saturating_add(8);
        }
        return Ok(Some((Decoded::PushReg { src }, opcode_index + 1)));
    }
    if (0x58..=0x5F).contains(&opcode) {
        let mut dst = opcode - 0x58;
        if rex_b {
            dst = dst.saturating_add(8);
        }
        return Ok(Some((Decoded::PopReg { dst }, opcode_index + 1)));
    }

    if (0x70..=0x7F).contains(&opcode) {
        // Jcc rel8.
        let imm = *bytes
            .get(opcode_index + 1)
            .ok_or_else(|| Error::from("truncated rel8"))? as i8;
        let len = opcode_index + 2;
        let target = (offset as i64)
            .saturating_add(len as i64)
            .saturating_add(imm as i64);
        if target < 0 {
            return Err(Error::from("x86_64 jcc target underflow"));
        }
        return Ok(Some((
            Decoded::JccRel {
                condition: opcode & 0x0F,
                target: target as u64,
            },
            len,
        )));
    }

    // MOV r/m8, imm8: C6 /0 imm8.
    if opcode == 0xC6 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        if reg != 0 {
            return Err(Error::from("unsupported x86_64 C6 opcode extension"));
        }
        let imm_offset = opcode_index + 1 + consumed;
        let imm = *bytes
            .get(imm_offset)
            .ok_or_else(|| Error::from("truncated imm8"))? as i8;
        return Ok(Some((
            Decoded::MovImm8ToRm { dst: rm, imm },
            imm_offset + 1,
        )));
    }

    if opcode == 0x0F {
        let ext = *bytes
            .get(opcode_index + 1)
            .ok_or_else(|| Error::from("truncated 0f opcode"))?;
        if ext == 0x1F {
            // Multi-byte NOP: 0F 1F /0.
            let (_ext_reg, _rm, consumed) =
                decode_modrm(bytes, opcode_index + 2, rex_r, rex_x, rex_b, segment)?;
            return Ok(Some((Decoded::Nop, opcode_index + 2 + consumed)));
        }
        if ext == 0x05 {
            return Ok(Some((Decoded::Syscall, opcode_index + 2)));
        }
        if (0x80..=0x8F).contains(&ext) {
            // Jcc rel32.
            let imm = read_i32(bytes, opcode_index + 2)? as i64;
            let len = opcode_index + 2 + 4;
            let target = (offset as i64)
                .saturating_add(len as i64)
                .saturating_add(imm);
            if target < 0 {
                return Err(Error::from("x86_64 jcc target underflow"));
            }
            return Ok(Some((
                Decoded::JccRel {
                    condition: ext & 0x0F,
                    target: target as u64,
                },
                len,
            )));
        }
        if (0x90..=0x9F).contains(&ext) {
            // SETcc r/m8.
            let (ext_reg, rm, consumed) =
                decode_modrm(bytes, opcode_index + 2, rex_r, rex_x, rex_b, segment)?;
            // ext_reg is the ModRM.reg field; for SETcc it is part of the encoding
            // but should be 0.
            let _ = ext_reg;
            return Ok(Some((
                Decoded::Setcc {
                    dst: rm,
                    condition: ext & 0x0F,
                },
                opcode_index + 2 + consumed,
            )));
        }
        if ext == 0xB6 || ext == 0xB7 {
            // MOVZX r, r/m8|r/m16.
            let (reg, rm, consumed) =
                decode_modrm(bytes, opcode_index + 2, rex_r, rex_x, rex_b, segment)?;
            let src_width_bits = if ext == 0xB6 { 8 } else { 16 };
            let dst_width_bits = if rex_w { 64 } else { 32 };
            return Ok(Some((
                Decoded::MovZx {
                    dst: reg,
                    src: rm,
                    src_width_bits,
                    dst_width_bits,
                },
                opcode_index + 2 + consumed,
            )));
        }
        if ext == 0xBE || ext == 0xBF {
            // MOVSX r, r/m8|r/m16.
            let (reg, rm, consumed) =
                decode_modrm(bytes, opcode_index + 2, rex_r, rex_x, rex_b, segment)?;
            let src_width_bits = if ext == 0xBE { 8 } else { 16 };
            let dst_width_bits = if rex_w { 64 } else { 32 };
            return Ok(Some((
                Decoded::MovSx {
                    dst: reg,
                    src: rm,
                    src_width_bits,
                    dst_width_bits,
                },
                opcode_index + 2 + consumed,
            )));
        }
    }

    if opcode == 0xEB {
        // JMP rel8.
        let imm = *bytes
            .get(opcode_index + 1)
            .ok_or_else(|| Error::from("truncated rel8"))? as i8;
        let len = opcode_index + 2;
        let target = (offset as i64)
            .saturating_add(len as i64)
            .saturating_add(imm as i64);
        if target < 0 {
            return Err(Error::from("x86_64 jmp target underflow"));
        }
        return Ok(Some((
            Decoded::JmpRel {
                target: target as u64,
            },
            len,
        )));
    }

    if opcode == 0xE9 {
        // JMP rel32.
        let imm = read_i32(bytes, opcode_index + 1)? as i64;
        let len = opcode_index + 1 + 4;
        let target = (offset as i64)
            .saturating_add(len as i64)
            .saturating_add(imm);
        if target < 0 {
            return Err(Error::from("x86_64 jmp target underflow"));
        }
        return Ok(Some((
            Decoded::JmpRel {
                target: target as u64,
            },
            len,
        )));
    }

    if opcode == 0xE8 {
        // CALL rel32.
        let imm = read_i32(bytes, opcode_index + 1)? as i64;
        let len = opcode_index + 1 + 4;
        let target = (offset as i64)
            .saturating_add(len as i64)
            .saturating_add(imm);
        if target < 0 {
            return Err(Error::from("x86_64 call target underflow"));
        }
        return Ok(Some((
            Decoded::CallRel32 {
                imm_offset: opcode_index + 1,
                target: target as u64,
            },
            len,
        )));
    }

    if opcode == 0xFF {
        // Group 5.
        let (ext, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        match ext {
            0 => {
                // INC r/m.
                let width_bits = if rex_w { 64 } else { 32 };
                return Ok(Some((
                    Decoded::IncRm {
                        target: rm,
                        width_bits,
                    },
                    opcode_index + 1 + consumed,
                )));
            }
            1 => {
                // DEC r/m.
                let width_bits = if rex_w { 64 } else { 32 };
                return Ok(Some((
                    Decoded::DecRm {
                        target: rm,
                        width_bits,
                    },
                    opcode_index + 1 + consumed,
                )));
            }
            2 => {
                // CALL r/m64.
                return Ok(Some((
                    Decoded::CallRm { target: rm },
                    opcode_index + 1 + consumed,
                )));
            }
            4 => {
                // JMP r/m64.
                return Ok(Some((
                    Decoded::JmpRm { target: rm },
                    opcode_index + 1 + consumed,
                )));
            }
            6 => {
                // PUSH r/m.
                return Ok(Some((
                    Decoded::PushRm { src: rm },
                    opcode_index + 1 + consumed,
                )));
            }
            _ => {}
        }
    }

    // MOV r64, imm64: REX.W B8+rd imm64.
    if rex_w && (0xB8..=0xBF).contains(&opcode) {
        let mut dst = opcode - 0xB8;
        if rex_b {
            dst = dst.saturating_add(8);
        }
        let imm = read_i64(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::MovImm64 {
                dst,
                imm_offset: opcode_index + 1,
                imm,
            },
            opcode_index + 1 + 8,
        )));
    }

    // MOV r32, imm32: B8+rd imm32 (zero-extended).
    if !rex_w && (0xB8..=0xBF).contains(&opcode) {
        let mut dst = opcode - 0xB8;
        if rex_b {
            dst = dst.saturating_add(8);
        }
        let imm = read_i32(bytes, opcode_index + 1)? as u32 as i64;
        return Ok(Some((
            Decoded::MovImm64 {
                dst,
                imm_offset: opcode_index + 1,
                imm,
            },
            opcode_index + 1 + 4,
        )));
    }

    // MOV r/m, imm32: C7 /0 imm32 (sign-extended for 64-bit destinations).
    if opcode == 0xC7 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        if reg != 0 {
            return Err(Error::from("unsupported x86_64 C7 opcode extension"));
        }
        let imm_offset = opcode_index + 1 + consumed;
        if !rex_w && operand_size_override {
            let imm = read_u16(bytes, imm_offset)?;
            return Ok(Some((
                Decoded::MovImm16ToRm {
                    dst: rm,
                    imm_offset,
                    imm,
                },
                imm_offset + 2,
            )));
        }
        let imm = read_i32(bytes, imm_offset)?;
        if rex_w {
            // Treat this as a sign-extending write to the full 64-bit register.
            match rm {
                RmOperand::Reg(dst) => {
                    return Ok(Some((
                        Decoded::MovImm64 {
                            dst,
                            imm_offset,
                            imm: imm as i64,
                        },
                        imm_offset + 4,
                    )));
                }
                RmOperand::Mem(memory) => {
                    return Ok(Some((
                        Decoded::MovImm32ToMem64 {
                            dst: memory,
                            imm_offset,
                            imm,
                        },
                        imm_offset + 4,
                    )));
                }
            }
        }
        return Ok(Some((
            Decoded::MovImm32ToRm {
                dst: rm,
                imm_offset,
                imm,
            },
            imm_offset + 4,
        )));
    }

    // LEA r, m: 8D /r.
    if opcode == 0x8D {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let RmOperand::Mem(memory) = rm else {
            return Err(Error::from("unsupported x86_64 lea with register operand"));
        };
        let width_bits = if rex_w { 64 } else { 32 };
        return Ok(Some((
            Decoded::Lea {
                dst: reg,
                src: memory,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // ADD rax, imm32: REX.W 05 imm32.
    if rex_w && opcode == 0x05 {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::AddImm {
                dst: 0,
                imm: imm as i64,
                width_bits: 64,
            },
            opcode_index + 1 + 4,
        )));
    }

    // ADD eax, imm32: 05 imm32.
    if !rex_w && opcode == 0x05 {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::AddImm {
                dst: 0,
                imm: imm as i64,
                width_bits: 32,
            },
            opcode_index + 1 + 4,
        )));
    }

    // SUB rax, imm32: REX.W 2D imm32.
    if rex_w && opcode == 0x2D {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::SubImm {
                dst: 0,
                imm: imm as i64,
                width_bits: 64,
            },
            opcode_index + 1 + 4,
        )));
    }

    // SUB eax, imm32: 2D imm32.
    if !rex_w && opcode == 0x2D {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::SubImm {
                dst: 0,
                imm: imm as i64,
                width_bits: 32,
            },
            opcode_index + 1 + 4,
        )));
    }

    // ADD r64, r/m64: REX.W 03 /r.
    if rex_w && opcode == 0x03 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::AddRegRm { dst: reg, src: rm },
            opcode_index + 1 + consumed,
        )));
    }

    // SUB r64, r/m64: REX.W 2B /r.
    if rex_w && opcode == 0x2B {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::SubRegRm { dst: reg, src: rm },
            opcode_index + 1 + consumed,
        )));
    }

    // MOVSXD r64, r/m32: 63 /r.
    if opcode == 0x63 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::MovSxd { dst: reg, src: rm },
            opcode_index + 1 + consumed,
        )));
    }

    // ADD r/m, r: 01 /r.
    if opcode == 0x01 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::AddRmReg {
                dst: rm,
                src: reg,
                width_bits: if rex_w { 64 } else { 32 },
            },
            opcode_index + 1 + consumed,
        )));
    }

    // SUB r/m, r: 29 /r.
    if opcode == 0x29 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::SubRmReg {
                dst: rm,
                src: reg,
                width_bits: if rex_w { 64 } else { 32 },
            },
            opcode_index + 1 + consumed,
        )));
    }

    // CMP rax, imm32: REX.W 3D imm32.
    if rex_w && opcode == 0x3D {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::Cmp {
                lhs: Operand::Rm(RmOperand::Reg(0)),
                rhs: Operand::Imm(imm as i64),
                width_bits: 64,
            },
            opcode_index + 1 + 4,
        )));
    }

    // CMP eax, imm32: 3D imm32.
    if !rex_w && opcode == 0x3D {
        let imm = read_i32(bytes, opcode_index + 1)?;
        return Ok(Some((
            Decoded::Cmp {
                lhs: Operand::Rm(RmOperand::Reg(0)),
                rhs: Operand::Imm(imm as i64),
                width_bits: 32,
            },
            opcode_index + 1 + 4,
        )));
    }

    // CMP r/m, r: 39 /r.
    if opcode == 0x39 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::Cmp {
                lhs: Operand::Rm(rm),
                rhs: Operand::Rm(RmOperand::Reg(reg)),
                width_bits: if rex_w { 64 } else { 32 },
            },
            opcode_index + 1 + consumed,
        )));
    }

    // CMP r, r/m: 3B /r.
    if opcode == 0x3B {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::Cmp {
                lhs: Operand::Rm(RmOperand::Reg(reg)),
                rhs: Operand::Rm(rm),
                width_bits: if rex_w { 64 } else { 32 },
            },
            opcode_index + 1 + consumed,
        )));
    }

    // TEST r/m, r: 85 /r.
    if opcode == 0x85 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::Test {
                lhs: Operand::Rm(rm),
                rhs: Operand::Rm(RmOperand::Reg(reg)),
                width_bits: if rex_w { 64 } else { 32 },
            },
            opcode_index + 1 + consumed,
        )));
    }

    // TEST r/m8, r8: 84 /r.
    if opcode == 0x84 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::Test {
                lhs: Operand::Rm(rm),
                rhs: Operand::Rm(RmOperand::Reg(reg)),
                width_bits: 8,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // XOR r/m, r.
    if opcode == 0x30 || opcode == 0x31 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x30 {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        let RmOperand::Reg(dst) = rm else {
            return Err(Error::from("unsupported x86_64 xor to memory"));
        };
        return Ok(Some((
            Decoded::XorReg {
                dst,
                src: reg,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // XOR r, r/m.
    if opcode == 0x32 || opcode == 0x33 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x32 {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        return Ok(Some((
            Decoded::XorRmToReg {
                dst: reg,
                src: rm,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // AND r/m, r.
    if opcode == 0x20 || opcode == 0x21 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x20 {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        let RmOperand::Reg(dst) = rm else {
            return Err(Error::from("unsupported x86_64 and to memory"));
        };
        return Ok(Some((
            Decoded::AndReg {
                dst,
                src: reg,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // AND r, r/m.
    if opcode == 0x22 || opcode == 0x23 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x22 {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        return Ok(Some((
            Decoded::AndRmToReg {
                dst: reg,
                src: rm,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // OR r/m, r.
    if opcode == 0x08 || opcode == 0x09 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x08 {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        let RmOperand::Reg(dst) = rm else {
            return Err(Error::from("unsupported x86_64 or to memory"));
        };
        return Ok(Some((
            Decoded::OrReg {
                dst,
                src: reg,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // OR r, r/m.
    if opcode == 0x0A || opcode == 0x0B {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if opcode == 0x0A {
            8
        } else if rex_w {
            64
        } else {
            32
        };
        return Ok(Some((
            Decoded::OrRmToReg {
                dst: reg,
                src: rm,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // ADD/SUB/CMP r/m, imm8|imm32: 83/81 /0, /5, /7.
    if opcode == 0x83 || opcode == 0x81 {
        let (ext, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let imm_offset = opcode_index + 1 + consumed;
        let (imm, imm_len) = if opcode == 0x83 {
            let imm8 = *bytes
                .get(imm_offset)
                .ok_or_else(|| Error::from("truncated imm8"))? as i8;
            (imm8 as i64, 1usize)
        } else {
            (read_i32(bytes, imm_offset)? as i64, 4usize)
        };
        let len = imm_offset + imm_len;
        let width_bits = if rex_w { 64 } else { 32 };
        match (ext, rm) {
            (0, RmOperand::Reg(dst)) => {
                return Ok(Some((
                    Decoded::AddImm {
                        dst,
                        imm,
                        width_bits,
                    },
                    len,
                )));
            }
            (0, rm) => {
                return Ok(Some((
                    Decoded::AddImmRm {
                        dst: rm,
                        imm,
                        width_bits,
                    },
                    len,
                )));
            }
            (5, RmOperand::Reg(dst)) => {
                return Ok(Some((
                    Decoded::SubImm {
                        dst,
                        imm,
                        width_bits,
                    },
                    len,
                )));
            }
            (5, rm) => {
                return Ok(Some((
                    Decoded::SubImmRm {
                        dst: rm,
                        imm,
                        width_bits,
                    },
                    len,
                )));
            }
            (2, rm) => {
                return Ok(Some((
                    Decoded::AdcImm {
                        dst: rm,
                        imm,
                        width_bits,
                    },
                    len,
                )));
            }
            (3, rm) => {
                return Ok(Some((
                    Decoded::SbbImm {
                        dst: rm,
                        imm,
                        width_bits,
                    },
                    len,
                )));
            }
            (1, rm) => {
                return Ok(Some((
                    Decoded::OrImm {
                        dst: rm,
                        imm,
                        width_bits,
                    },
                    len,
                )));
            }
            (4, rm) => {
                return Ok(Some((
                    Decoded::AndImm {
                        dst: rm,
                        imm,
                        width_bits,
                    },
                    len,
                )));
            }
            (6, rm) => {
                return Ok(Some((
                    Decoded::XorImm {
                        dst: rm,
                        imm,
                        width_bits,
                    },
                    len,
                )));
            }
            (7, rm) => {
                return Ok(Some((
                    Decoded::Cmp {
                        lhs: Operand::Rm(rm),
                        rhs: Operand::Imm(imm),
                        width_bits,
                    },
                    len,
                )));
            }
            _ => {}
        }
    }

    // Group1 r/m8, imm8: 80 /ext imm8.
    if opcode == 0x80 {
        let (ext, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let imm_offset = opcode_index + 1 + consumed;
        let imm = *bytes
            .get(imm_offset)
            .ok_or_else(|| Error::from("truncated imm8"))? as i8;
        let len = imm_offset + 1;
        match ext {
            0 => {
                return Ok(Some((
                    Decoded::AddImmRm {
                        dst: rm,
                        imm: imm as i64,
                        width_bits: 8,
                    },
                    len,
                )));
            }
            1 => {
                return Ok(Some((
                    Decoded::OrImm {
                        dst: rm,
                        imm: imm as i64,
                        width_bits: 8,
                    },
                    len,
                )));
            }
            4 => {
                return Ok(Some((
                    Decoded::AndImm {
                        dst: rm,
                        imm: imm as i64,
                        width_bits: 8,
                    },
                    len,
                )));
            }
            5 => {
                return Ok(Some((
                    Decoded::SubImmRm {
                        dst: rm,
                        imm: imm as i64,
                        width_bits: 8,
                    },
                    len,
                )));
            }
            6 => {
                return Ok(Some((
                    Decoded::XorImm {
                        dst: rm,
                        imm: imm as i64,
                        width_bits: 8,
                    },
                    len,
                )));
            }
            7 => {
                return Ok(Some((
                    Decoded::Cmp {
                        lhs: Operand::Rm(rm),
                        rhs: Operand::Imm(imm as i64),
                        width_bits: 8,
                    },
                    len,
                )));
            }
            _ => {}
        }
    }

    // MOV r, r/m: 8B /r (32-bit by default, 64-bit with REX.W).
    if opcode == 0x8B {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if rex_w { 64 } else { 32 };
        return Ok(Some((
            Decoded::MovRmToReg {
                dst: reg,
                src: rm,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // MOV r8, r/m8: 8A /r.
    if opcode == 0x8A {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::MovRmToReg {
                dst: reg,
                src: rm,
                width_bits: 8,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // MOV r/m, r: 89 /r (32-bit by default, 64-bit with REX.W).
    if opcode == 0x89 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        let width_bits = if rex_w { 64 } else { 32 };
        return Ok(Some((
            Decoded::MovRegToRm {
                dst: rm,
                src: reg,
                width_bits,
            },
            opcode_index + 1 + consumed,
        )));
    }

    // MOV r/m8, r8: 88 /r.
    if opcode == 0x88 {
        let (reg, rm, consumed) =
            decode_modrm(bytes, opcode_index + 1, rex_r, rex_x, rex_b, segment)?;
        return Ok(Some((
            Decoded::MovRegToRm {
                dst: rm,
                src: reg,
                width_bits: 8,
            },
            opcode_index + 1 + consumed,
        )));
    }

    Ok(None)
}

pub(super) fn build_binop(id: u32, kind: AsmInstructionKind, opcode: AsmOpcode) -> AsmInstruction {
    AsmInstruction {
        id,
        opcode,
        kind,
        ty: AsmType::I64,
        operands: Vec::new(),
        implicit_uses: Vec::new(),
        implicit_defs: Vec::new(),
        encoding: None,
        debug_info: None,
        annotations: Vec::new(),
    }
}

pub(super) fn lift_rm_imm_binop(
    ctx: &mut RegisterLiftContext,
    dst: RmOperand,
    imm: i64,
    width_bits: u16,
    inst: DecodedInstruction,
    bytes: &[u8],
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
    opcode: fp_core::asmir::AsmGenericOpcode,
) -> Result<()> {
    let preserve_dst_reg = match dst {
        RmOperand::Reg(reg) => Some(reg),
        _ => None,
    };
    let lhs = value_from_rm_with_width(ctx, dst, width_bits, inst, relocs, instructions, next_id)?;
    let rhs = AsmValue::Constant(AsmConstant::Int(imm, AsmType::I64));

    let id = *next_id;
    let kind = match opcode {
        fp_core::asmir::AsmGenericOpcode::Add => AsmInstructionKind::Add(lhs, rhs),
        fp_core::asmir::AsmGenericOpcode::Sub => AsmInstructionKind::Sub(lhs, rhs),
        fp_core::asmir::AsmGenericOpcode::And => AsmInstructionKind::And(lhs, rhs),
        fp_core::asmir::AsmGenericOpcode::Or => AsmInstructionKind::Or(lhs, rhs),
        fp_core::asmir::AsmGenericOpcode::Xor => AsmInstructionKind::Xor(lhs, rhs),
        _ => return Err(Error::from("unsupported rm+imm binop opcode")),
    };
    let opcode_copy = opcode.clone();
    let mut binop_inst = build_binop(id, kind, AsmOpcode::Generic(opcode_copy));
    if let (
        Some(dst_reg),
        fp_core::asmir::AsmGenericOpcode::Add | fp_core::asmir::AsmGenericOpcode::Sub,
    ) = (preserve_dst_reg, opcode)
    {
        let raw_opcode = x86_opcode_after_prefixes(bytes, &inst)?;
        let imm_width_bits = match raw_opcode {
            0x83 => 8u16,
            0x81 => 32u16,
            _ => width_bits,
        };
        binop_inst.annotations.extend([
            AsmAnnotation {
                key: "fp.preserve.x86_64.dst_gpr".to_string(),
                value: dst_reg.to_string(),
            },
            AsmAnnotation {
                key: "fp.preserve.x86_64.imm_width_bits".to_string(),
                value: imm_width_bits.to_string(),
            },
        ]);
    }
    instructions.push(binop_inst);
    *next_id += 1;

    match dst {
        RmOperand::Reg(dst_reg) => write_gpr_with_width(
            ctx,
            dst_reg,
            AsmValue::Register(id),
            width_bits,
            instructions,
            next_id,
        ),
        RmOperand::Mem(memory) => {
            if memory.segment.is_some() {
                return Ok(());
            }
            let stored =
                value_for_store(width_bits, AsmValue::Register(id), instructions, next_id)?;
            let addr = compute_address(
                ctx,
                memory,
                inst.offset,
                inst.len,
                relocs,
                instructions,
                next_id,
            )?;
            let store_id = *next_id;
            instructions.push(AsmInstruction {
                id: store_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Store),
                kind: AsmInstructionKind::Store {
                    value: stored,
                    address: addr,
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::Void,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            Ok(())
        }
    }
}

pub(super) fn read_i32(bytes: &[u8], index: usize) -> Result<i32> {
    let imm = bytes
        .get(index..index + 4)
        .ok_or_else(|| Error::from("truncated immediate"))?;
    Ok(i32::from_le_bytes(imm.try_into().unwrap()))
}

pub(super) fn read_i64(bytes: &[u8], index: usize) -> Result<i64> {
    let imm = bytes
        .get(index..index + 8)
        .ok_or_else(|| Error::from("truncated immediate"))?;
    Ok(i64::from_le_bytes(imm.try_into().unwrap()))
}

pub(super) fn decode_modrm(
    bytes: &[u8],
    index: usize,
    rex_r: bool,
    rex_x: bool,
    rex_b: bool,
    segment: Option<X86Segment>,
) -> Result<(u8, RmOperand, usize)> {
    let modrm = *bytes
        .get(index)
        .ok_or_else(|| Error::from("missing modrm"))?;
    let mode = (modrm >> 6) & 0b11;
    let reg3 = (modrm >> 3) & 0b111;
    let rm3 = modrm & 0b111;
    let mut reg = reg3;
    if rex_r {
        reg = reg.saturating_add(8);
    }
    let mut consumed = 1usize;

    if mode == 0b11 {
        let mut rm = rm3;
        if rex_b {
            rm = rm.saturating_add(8);
        }
        return Ok((reg, RmOperand::Reg(rm), consumed));
    }

    let mut base = if mode == 0b00 && rm3 == 0b101 {
        None
    } else {
        let mut rm = rm3;
        if rex_b {
            rm = rm.saturating_add(8);
        }
        Some(rm)
    };
    let mut index_reg = None;
    let mut scale = 1u8;
    let mut displacement = 0i64;
    let mut displacement_offset = None;

    if rm3 == 0b100 {
        let sib = *bytes
            .get(index + consumed)
            .ok_or_else(|| Error::from("missing sib"))?;
        consumed += 1;
        let scale_bits = (sib >> 6) & 0b11;
        scale = 1u8 << scale_bits;
        let mut index_bits = (sib >> 3) & 0b111;
        let base_bits = sib & 0b111;
        if rex_x {
            index_bits = index_bits.saturating_add(8);
        }
        if index_bits != 0b100 {
            index_reg = Some(index_bits);
        }
        base = if mode == 0b00 && base_bits == 0b101 {
            None
        } else {
            let mut base_reg = base_bits;
            if rex_b {
                base_reg = base_reg.saturating_add(8);
            }
            Some(base_reg)
        };
    }

    match mode {
        0b00 => {
            if base.is_none() {
                displacement_offset = Some(index + consumed);
                displacement = read_i32(bytes, index + consumed)? as i64;
                consumed += 4;
            }
        }
        0b01 => {
            let disp8 = *bytes
                .get(index + consumed)
                .ok_or_else(|| Error::from("missing disp8"))? as i8;
            displacement = disp8 as i64;
            consumed += 1;
        }
        0b10 => {
            displacement = read_i32(bytes, index + consumed)? as i64;
            consumed += 4;
        }
        _ => {}
    }

    Ok((
        reg,
        RmOperand::Mem(X86Memory {
            base,
            index: index_reg,
            scale,
            displacement,
            displacement_offset,
            segment,
        }),
        consumed,
    ))
}

pub(super) fn vec_operand_value(
    ctx: &mut RegisterLiftContext,
    operand: VecOperand,
    instruction_offset: u64,
    instruction_len: usize,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    match operand {
        VecOperand::Reg(reg) => ctx.read_vec(reg),
        VecOperand::Mem(memory) => {
            if memory.segment.is_some() {
                let id = *next_id;
                instructions.push(AsmInstruction {
                    id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                    kind: AsmInstructionKind::BuildVector {
                        elements: vec![
                            AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                            AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
                        ],
                    },
                    ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                return Ok(AsmValue::Register(id));
            }

            let addr = compute_address(
                ctx,
                memory,
                instruction_offset,
                instruction_len,
                relocs,
                instructions,
                next_id,
            )?;

            let load0_id = *next_id;
            instructions.push(AsmInstruction {
                id: load0_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: addr.clone(),
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let addr1_id = *next_id;
            instructions.push(build_binop(
                addr1_id,
                AsmInstructionKind::Add(
                    addr,
                    AsmValue::Constant(AsmConstant::Int(8, AsmType::I64)),
                ),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
            ));
            *next_id += 1;

            let load1_id = *next_id;
            instructions.push(AsmInstruction {
                id: load1_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Load),
                kind: AsmInstructionKind::Load {
                    address: AsmValue::Register(addr1_id),
                    alignment: None,
                    volatile: false,
                },
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let id = *next_id;
            instructions.push(AsmInstruction {
                id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::BuildVector),
                kind: AsmInstructionKind::BuildVector {
                    elements: vec![AsmValue::Register(load0_id), AsmValue::Register(load1_id)],
                },
                ty: AsmType::Vector(Box::new(AsmType::I64), 2),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            Ok(AsmValue::Register(id))
        }
    }
}

pub(super) fn compute_address(
    ctx: &mut RegisterLiftContext,
    memory: X86Memory,
    instruction_offset: u64,
    instruction_len: usize,
    relocs: &[TextRelocation],
    instructions: &mut Vec<AsmInstruction>,
    next_id: &mut u32,
) -> Result<AsmValue> {
    if let Some(displacement_offset) = memory.displacement_offset {
        let relocation_offset = instruction_offset
            .checked_add(displacement_offset as u64)
            .ok_or_else(|| Error::from("x86_64 relocation offset overflow"))?;
        if let Some(reloc) = relocation_at(relocs, relocation_offset) {
            if reloc.addend == 0 && memory.displacement == 0 {
                if let Some(text) = ctx.rodata_cstrings.get(&reloc.symbol) {
                    return Ok(AsmValue::Constant(AsmConstant::String(text.clone())));
                }
            }
            if reloc.kind != object::RelocationKind::Relative
                && reloc.kind != object::RelocationKind::Absolute
            {
                return Err(Error::from(
                    "unsupported x86_64 relocation kind for address",
                ));
            }
            if reloc.encoding != object::RelocationEncoding::X86RipRelative
                && reloc.encoding != object::RelocationEncoding::X86RipRelativeMovq
                && reloc.encoding != object::RelocationEncoding::Generic
                && reloc.encoding != object::RelocationEncoding::Unknown
            {
                return Err(Error::from(
                    "unsupported x86_64 relocation encoding for address",
                ));
            }
            let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                Name::new(reloc.symbol.clone()),
                AsmType::Ptr(Box::new(AsmType::I8)),
                vec![0],
            ));
            let symbol_id = *next_id;
            instructions.push(AsmInstruction {
                id: symbol_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                kind: AsmInstructionKind::Freeze(symbol_const),
                ty: AsmType::Ptr(Box::new(AsmType::I8)),
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;

            let mut addr = AsmValue::Register(symbol_id);
            let mut addend = reloc.addend.saturating_add(memory.displacement);
            if reloc.kind == object::RelocationKind::Relative
                || reloc.kind == object::RelocationKind::PltRelative
            {
                // x86_64 RIP-relative address computations are based on the
                // next-instruction address, but relocation addends are defined
                // relative to the relocation field itself. Adjust by the
                // delta between the instruction end and the relocation field.
                let correction = instruction_len as i64 - displacement_offset as i64;
                addend = addend.saturating_add(correction);
            }
            if addend != 0 {
                let rhs = AsmValue::Constant(AsmConstant::Int(addend, AsmType::I64));
                let id = *next_id;
                instructions.push(build_binop(
                    id,
                    AsmInstructionKind::Add(addr, rhs),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                ));
                *next_id += 1;
                addr = AsmValue::Register(id);
            }

            return Ok(addr);
        }

        if memory.base == Some(16) && memory.index.is_none() {
            if let Some(symbol) =
                ctx.resolve_rip_symbol(&memory, instruction_offset, instruction_len)
            {
                let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new(symbol.name.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let symbol_id = *next_id;
                instructions.push(AsmInstruction {
                    id: symbol_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(symbol_const),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                return Ok(AsmValue::Register(symbol_id));
            }

            let next_ip = (ctx.code_base_address as i64)
                .saturating_add(instruction_offset as i64)
                .saturating_add(instruction_len as i64);
            let absolute = next_ip.saturating_add(memory.displacement);
            if absolute < 0 {
                return Err(Error::from("x86_64 RIP-relative address underflow"));
            }

            if let Some((region, offset)) = ctx.resolve_data_region(absolute as u64) {
                let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new(region.symbol.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let symbol_id = *next_id;
                instructions.push(AsmInstruction {
                    id: symbol_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(symbol_const),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;

                if offset == 0 {
                    return Ok(AsmValue::Register(symbol_id));
                }

                let rhs = AsmValue::Constant(AsmConstant::Int(offset as i64, AsmType::I64));
                let id = *next_id;
                instructions.push(build_binop(
                    id,
                    AsmInstructionKind::Add(AsmValue::Register(symbol_id), rhs),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                ));
                *next_id += 1;
                return Ok(AsmValue::Register(id));
            }

            let addr_const = AsmValue::Constant(AsmConstant::UInt(absolute as u64, AsmType::I64));
            let addr_id = *next_id;
            instructions.push(AsmInstruction {
                id: addr_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                kind: AsmInstructionKind::Freeze(addr_const),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            return Ok(AsmValue::Register(addr_id));
        }

        // `mod=00 rm=101 disp32` is RIP-relative addressing on x86_64.
        // ELF executables frequently use it without a relocation (e.g., for
        // local data). For now, treat the computed absolute address as an
        // immediate pointer value. This avoids inventing a fake symbol that
        // later stages cannot resolve when producing fully-linked executables.
        if memory.base.is_none() && memory.index.is_none() {
            let next_ip = (ctx.code_base_address as i64)
                .saturating_add(instruction_offset as i64)
                .saturating_add(instruction_len as i64);
            let absolute = next_ip.saturating_add(memory.displacement);

            if let Some(symbol) =
                ctx.resolve_disp32_symbol(&memory, instruction_offset, instruction_len)
            {
                let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new(symbol.name.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let symbol_id = *next_id;
                instructions.push(AsmInstruction {
                    id: symbol_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(symbol_const),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;
                return Ok(AsmValue::Register(symbol_id));
            }

            if absolute < 0 {
                return Err(Error::from("x86_64 RIP-relative address underflow"));
            }

            if let Some((region, offset)) = ctx.resolve_data_region(absolute as u64) {
                let symbol_const = AsmValue::Constant(AsmConstant::GlobalRef(
                    Name::new(region.symbol.clone()),
                    AsmType::Ptr(Box::new(AsmType::I8)),
                    vec![0],
                ));
                let symbol_id = *next_id;
                instructions.push(AsmInstruction {
                    id: symbol_id,
                    opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                    kind: AsmInstructionKind::Freeze(symbol_const),
                    ty: AsmType::Ptr(Box::new(AsmType::I8)),
                    operands: Vec::new(),
                    implicit_uses: Vec::new(),
                    implicit_defs: Vec::new(),
                    encoding: None,
                    debug_info: None,
                    annotations: Vec::new(),
                });
                *next_id += 1;

                if offset == 0 {
                    return Ok(AsmValue::Register(symbol_id));
                }

                let rhs = AsmValue::Constant(AsmConstant::Int(offset as i64, AsmType::I64));
                let id = *next_id;
                instructions.push(build_binop(
                    id,
                    AsmInstructionKind::Add(AsmValue::Register(symbol_id), rhs),
                    AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
                ));
                *next_id += 1;
                return Ok(AsmValue::Register(id));
            }
            let addr_const = AsmValue::Constant(AsmConstant::UInt(absolute as u64, AsmType::I64));
            let addr_id = *next_id;
            instructions.push(AsmInstruction {
                id: addr_id,
                opcode: AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Freeze),
                kind: AsmInstructionKind::Freeze(addr_const),
                ty: AsmType::I64,
                operands: Vec::new(),
                implicit_uses: Vec::new(),
                implicit_defs: Vec::new(),
                encoding: None,
                debug_info: None,
                annotations: Vec::new(),
            });
            *next_id += 1;
            return Ok(AsmValue::Register(addr_id));
        }
    }

    let mut addr = match memory.base {
        Some(base) => ctx.read_gpr(base)?,
        None => AsmValue::Constant(AsmConstant::Int(0, AsmType::I64)),
    };

    if let Some(index_reg) = memory.index {
        let mut index_value = ctx.read_gpr(index_reg)?;
        if memory.scale != 1 {
            let rhs = AsmValue::Constant(AsmConstant::Int(memory.scale as i64, AsmType::I64));
            let id = *next_id;
            instructions.push(build_binop(
                id,
                AsmInstructionKind::Mul(index_value.clone(), rhs.clone()),
                AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Mul),
            ));
            *next_id += 1;
            index_value = AsmValue::Register(id);
        }

        let id = *next_id;
        instructions.push(build_binop(
            id,
            AsmInstructionKind::Add(addr, index_value),
            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
        ));
        *next_id += 1;
        addr = AsmValue::Register(id);
    }

    if memory.displacement != 0 {
        let rhs = AsmValue::Constant(AsmConstant::Int(memory.displacement, AsmType::I64));
        let id = *next_id;
        instructions.push(build_binop(
            id,
            AsmInstructionKind::Add(addr, rhs),
            AsmOpcode::Generic(fp_core::asmir::AsmGenericOpcode::Add),
        ));
        *next_id += 1;
        addr = AsmValue::Register(id);
    }

    Ok(addr)
}
