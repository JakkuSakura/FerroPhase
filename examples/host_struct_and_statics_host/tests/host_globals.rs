use std::process::Command;

use host_struct_and_statics_host::{HOST_POINT, host_globals};

#[test]
fn host_global_example_compiles_natively_without_recursing() {
    let output = Command::new(env!("CARGO_BIN_EXE_host-struct-and-statics-host"))
        .output()
        .expect("run host struct and statics example");

    assert!(
        output.status.success(),
        "host-global native compilation failed: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("phase: compiled"),
        "example did not reach the native compilation phase",
    );
}

#[test]
fn host_global_is_read_into_cross_language_output() {
    let output = Command::new(env!("CARGO_BIN_EXE_host-struct-and-statics-host"))
        .output()
        .expect("run host struct and statics example");

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).expect("example output is UTF-8"),
        "host point = (3, 4)\n",
    );
}

#[test]
fn mutable_host_global_registration_points_at_host_storage() {
    let globals = host_globals().expect("register host globals");
    let registered = globals
        .get("HOST_POINT")
        .expect("HOST_POINT is registered");

    assert!(registered.descriptor.mutable);

    let address = registered.address().cast::<i64>();
    unsafe {
        let point = std::ptr::addr_of_mut!(HOST_POINT);
        let original = (*point).x;
        *address = original + 1;
        assert_eq!((*point).x, original + 1);
        *address = original;
    }
}
