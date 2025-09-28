use std::fmt::{self, Formatter};

use fp_core::pretty::{PrettyCtx, PrettyPrintable};

use crate::context::LlvmContext;

impl PrettyPrintable for LlvmContext {
    fn fmt_pretty(&self, f: &mut Formatter<'_>, ctx: &mut PrettyCtx<'_>) -> fmt::Result {
        ctx.writeln(f, "llvm::Module {")?;
        let ir = self.print_to_string();
        ctx.with_indent(|ctx| {
            for line in ir.trim_end().lines() {
                ctx.writeln(f, line)?;
            }
            Ok(())
        })?;
        ctx.writeln(f, "}")
    }
}
