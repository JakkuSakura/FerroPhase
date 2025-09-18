use fp_llvm::{TargetInfo, TargetConfiguration};

#[test]
fn test_target_configuration_interface_exists() {
    // Test that TargetConfiguration trait is implemented
    // This will fail until T028 is implemented
    let default_target = TargetConfiguration::default_target();
    assert!(target_info_is_valid(&default_target));
}

#[test]
fn test_target_triple_parsing() {
    // Test that target triples are parsed correctly
    let test_triples = [
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu", 
        "x86_64-pc-windows-msvc",
        "x86_64-apple-darwin",
    ];
    
    for triple in test_triples {
        let target_result = TargetConfiguration::from_triple(triple);
        assert!(target_result.is_ok(), "Failed to parse triple: {}", triple);
        
        let target = target_result.unwrap();
        assert_eq!(target.triple.as_str(), triple);
        assert!(target_info_is_complete(&target));
    }
}

#[test]
fn test_supported_targets() {
    // Test that supported targets are correctly identified
    let supported_targets = [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc", 
        "x86_64-apple-darwin",
        "aarch64-unknown-linux-gnu",
        "aarch64-apple-darwin",
    ];
    
    for triple in supported_targets {
        let target = TargetConfiguration::from_triple(triple).unwrap();
        assert!(TargetConfiguration::is_supported(&target), 
                "Target should be supported: {}", triple);
    }
}

#[test]
fn test_unsupported_targets() {
    // Test that unsupported targets are rejected
    let unsupported_targets = [
        "invalid-target-triple",
        "mips-unknown-linux-gnu", // Not yet supported
        "wasm32-unknown-unknown", // Future target
    ];
    
    for triple in unsupported_targets {
        let target_result = TargetConfiguration::from_triple(triple);
        if let Ok(target) = target_result {
            assert!(!TargetConfiguration::is_supported(&target),
                    "Target should not be supported: {}", triple);
        }
        // Or parsing itself might fail for invalid triples
    }
}

#[test]
fn test_target_machine_creation() {
    // Test that LLVM target machines can be created
    let target = TargetConfiguration::from_triple("x86_64-unknown-linux-gnu").unwrap();
    let target_machine_result = TargetConfiguration::create_target_machine(&target);
    
    assert!(target_machine_result.is_ok());
    let target_machine = target_machine_result.unwrap();
    assert!(target_machine_is_valid(&target_machine));
}

#[test]
fn test_calling_convention_mapping() {
    // Test that calling conventions are correctly mapped
    let linux_target = TargetConfiguration::from_triple("x86_64-unknown-linux-gnu").unwrap();
    assert_eq!(linux_target.calling_convention, CallingConvention::SystemV);
    
    let windows_target = TargetConfiguration::from_triple("x86_64-pc-windows-msvc").unwrap();
    assert_eq!(windows_target.calling_convention, CallingConvention::MicrosoftX64);
    
    let arm_target = TargetConfiguration::from_triple("aarch64-unknown-linux-gnu").unwrap();
    assert_eq!(arm_target.calling_convention, CallingConvention::Aapcs);
}

#[test]
fn test_data_layout_consistency() {
    // Test that data layouts are consistent with target
    let targets = [
        "x86_64-unknown-linux-gnu",
        "aarch64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
    ];
    
    for triple in targets {
        let target = TargetConfiguration::from_triple(triple).unwrap();
        assert!(!target.data_layout.is_empty());
        assert!(data_layout_matches_target(&target.data_layout, triple));
        assert_eq!(target.pointer_size, expected_pointer_size_for_target(triple));
    }
}

#[test]
fn test_cpu_features_handling() {
    // Test that CPU features are properly handled
    let target = TargetConfiguration::from_triple("x86_64-unknown-linux-gnu").unwrap();
    
    // Should have basic CPU features
    assert!(!target.cpu_features.is_empty());
    assert!(target.cpu_features.contains(&"sse2".to_string()));
    
    // Features should be valid for the target architecture
    for feature in &target.cpu_features {
        assert!(cpu_feature_is_valid_for_arch(feature, "x86_64"));
    }
}

#[test]
fn test_alignment_requirements() {
    // Test that alignment requirements are correct
    let target = TargetConfiguration::from_triple("x86_64-unknown-linux-gnu").unwrap();
    
    assert!(target.alignment_requirements.is_valid());
    assert_eq!(target.alignment_requirements.pointer_alignment(), 8); // 64-bit pointers
    assert!(target.alignment_requirements.i32_alignment() >= 4);
    assert!(target.alignment_requirements.i64_alignment() >= 8);
}

// Helper functions - will be implemented when types are available

fn target_info_is_valid(target: &TargetInfo) -> bool {
    todo!("Validate target info structure")
}

fn target_info_is_complete(target: &TargetInfo) -> bool {
    todo!("Check target info completeness")
}

fn target_machine_is_valid(target_machine: &fp_llvm::TargetMachine) -> bool {
    todo!("Validate LLVM target machine")
}

fn data_layout_matches_target(data_layout: &str, triple: &str) -> bool {
    todo!("Check data layout consistency")
}

fn expected_pointer_size_for_target(triple: &str) -> usize {
    if triple.starts_with("x86_64") || triple.starts_with("aarch64") {
        8
    } else if triple.starts_with("i386") || triple.starts_with("arm") {
        4
    } else {
        todo!("Determine pointer size for target")
    }
}

fn cpu_feature_is_valid_for_arch(feature: &str, arch: &str) -> bool {
    todo!("Validate CPU feature for architecture")
}

// Type definitions that will be implemented later
use fp_llvm::{CallingConvention, AlignmentInfo};

// Trait extension for alignment requirements
trait AlignmentRequirements {
    fn is_valid(&self) -> bool;
    fn pointer_alignment(&self) -> usize;
    fn i32_alignment(&self) -> usize;
    fn i64_alignment(&self) -> usize;
}

impl AlignmentRequirements for AlignmentInfo {
    fn is_valid(&self) -> bool {
        todo!("Validate alignment info")
    }
    
    fn pointer_alignment(&self) -> usize {
        todo!("Get pointer alignment")
    }
    
    fn i32_alignment(&self) -> usize {
        todo!("Get i32 alignment")
    }
    
    fn i64_alignment(&self) -> usize {
        todo!("Get i64 alignment")
    }
}