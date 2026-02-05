use fp_core::hir;
use fp_core::mir;

pub fn is_c_abi_hir(abi: &hir::Abi) -> bool {
    matches!(abi, hir::Abi::C { .. } | hir::Abi::System { .. })
}

pub fn is_c_abi_mir(abi: &mir::ty::Abi) -> bool {
    matches!(abi, mir::ty::Abi::C { .. } | mir::ty::Abi::System { .. })
}

pub fn extern_symbol_name(full: &str) -> String {
    full.rsplit("::").next().unwrap_or(full).to_string()
}
