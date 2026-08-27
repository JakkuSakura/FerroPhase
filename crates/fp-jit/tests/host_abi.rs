use fp_core::ast::{Abi, FunctionSignature};
use fp_jit::{JitKey, signature_hash};

#[test]
fn host_static_access_abi_is_part_of_jit_identity() {
    let mut rust_sig = FunctionSignature::unit();
    let mut host_sig = FunctionSignature::unit();
    host_sig.abi = Abi::Named("host".into());

    assert_ne!(signature_hash(&rust_sig), signature_hash(&host_sig));
    assert_ne!(
        JitKey::new("HOST_VALUE", &rust_sig, 1),
        JitKey::new("HOST_VALUE", &host_sig, 1)
    );
    rust_sig.abi = Abi::Named("host".into());
    assert_eq!(signature_hash(&rust_sig), signature_hash(&host_sig));
}
