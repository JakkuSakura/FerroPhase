mod object_lift;
pub mod aarch64;

use fp_core::asmir::AsmProgram;
use fp_core::error::Result;

/// Lift an object file's machine code into generic AsmIR.
///
/// Current scope: AArch64 object files with a single text symbol.
pub fn lift_object_to_asmir(bytes: &[u8]) -> Result<AsmProgram> {
    object_lift::lift_object_to_asmir(bytes)
}

