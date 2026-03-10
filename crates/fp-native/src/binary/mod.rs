mod object_lift;
pub mod aarch64;
pub mod x86_64;

use fp_core::asmir::AsmProgram;
use fp_core::error::Result;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextRelocation {
    pub offset: u64,
    pub symbol: String,
    pub addend: i64,
}

pub struct LiftedFunction {
    pub instructions: Vec<fp_core::asmir::AsmInstruction>,
    pub terminator: Option<fp_core::asmir::AsmTerminator>,
    pub locals: Vec<fp_core::asmir::AsmLocal>,
}

/// Lift an object file's machine code into generic AsmIR.
///
/// Current scope: x86_64 + aarch64 object files with a single text symbol.
pub fn lift_object_to_asmir(bytes: &[u8]) -> Result<AsmProgram> {
    object_lift::lift_object_to_asmir(bytes)
}

