pub mod aarch64;
mod cfg;
mod object_lift;
pub mod x86_64;

use fp_core::asmir::AsmProgram;
use fp_core::error::Result;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextRelocation {
    pub offset: u64,
    pub symbol: String,
    pub addend: i64,
    pub kind: object::RelocationKind,
    pub encoding: object::RelocationEncoding,
    pub size: u8,
    pub flags: object::RelocationFlags,
}

pub struct LiftedFunction {
    pub basic_blocks: Vec<fp_core::asmir::AsmBlock>,
    pub locals: Vec<fp_core::asmir::AsmLocal>,
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
    use fp_core::asmir::AsmSyscallConvention;

    #[test]
    fn x86_64_lifter_splits_blocks_for_unconditional_jump() {
        let bytes = [0xEB, 0x01, 0xC3, 0xC3];
        let lifted =
            x86_64::lift_function_bytes(&bytes, &[], Some(AsmSyscallConvention::LinuxX86_64))
                .unwrap();
        assert_eq!(lifted.basic_blocks.len(), 3);
        assert_eq!(lifted.basic_blocks[0].id, 0);
        assert!(matches!(
            lifted.basic_blocks[0].terminator,
            fp_core::asmir::AsmTerminator::Br(2)
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
}
