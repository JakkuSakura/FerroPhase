/// LLVM backend-specific THIR materialization

use super::super::format::build_printf_format;
use fp_core::hir::typed::{self as thir, ExprKind, ItemRef};
use fp_core::intrinsics::{IntrinsicCallKind, IntrinsicMaterializer, MaterializedPrint};
use fp_core::span::Span;

/// LLVM backend's intrinsic materializer
pub struct LlvmIntrinsicMaterializer;

impl IntrinsicMaterializer for LlvmIntrinsicMaterializer {
    fn prepare_print(
        &self,
        kind: IntrinsicCallKind,
        template: &thir::FormatString,
    ) -> Option<MaterializedPrint> {
        let newline = matches!(kind, IntrinsicCallKind::Println);

        // Build printf format string
        let format_literal = build_printf_format(template, &template.args, newline);

        Some(MaterializedPrint {
            format_literal,
            printf_function_name: "printf".to_string(),
        })
    }

    fn can_materialize(&self, kind: IntrinsicCallKind) -> bool {
        matches!(kind, IntrinsicCallKind::Print | IntrinsicCallKind::Println)
    }
}

/// Get the LLVM intrinsic materializer
pub fn get_materializer() -> &'static LlvmIntrinsicMaterializer {
    &LlvmIntrinsicMaterializer
}