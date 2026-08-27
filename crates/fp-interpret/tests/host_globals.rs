use fp_core::lir::{Linkage, LirGlobal, LirType, Name, Visibility};

fn global(name: &str, is_constant: bool) -> LirGlobal {
    LirGlobal {
        name: Name::new(name),
        ty: LirType::I64,
        initializer: None,
        relocations: vec![],
        linkage: Linkage::External,
        visibility: Visibility::Default,
        is_constant,
        alignment: None,
        section: None,
    }
}

#[test]
fn host_static_global_metadata_preserves_mutability() {
    let host_static = global("HOST_VALUE", true);
    let host_static_mut = global("HOST_VALUE_MUT", false);

    assert!(host_static.is_constant);
    assert!(!host_static_mut.is_constant);
    assert_ne!(host_static, host_static_mut);
}
