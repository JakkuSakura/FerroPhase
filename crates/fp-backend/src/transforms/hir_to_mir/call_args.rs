use fp_core::hir;

pub(crate) fn values(args: &[hir::CallArg]) -> Vec<&hir::Expr> {
    args.iter().map(|arg| &arg.value).collect()
}
