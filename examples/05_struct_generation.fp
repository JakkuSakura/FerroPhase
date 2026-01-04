#!/usr/bin/env fp run
//! Struct generation with compile-time conditionals

fn main() {
    println!("📘 Tutorial: 05_struct_generation.fp");
    println!("🧭 Focus: Struct generation with compile-time conditionals");
    println!("🧪 What to look for: labeled outputs below");
    println!("✅ Expectation: outputs match labels");
    println!("");
    // Const flags affect struct generation
    const FLAG_A: bool = true;
    const FLAG_B: bool = false;

    type Base = const {
        let mut t = create_struct!("Base");
        addfield!(t, "id", i64);
        addfield!(t, "name", &'static str);
        t
    };

    type Config = const {
        let mut t = create_struct!("Config");
        addfield!(t, "id", i64);
        addfield!(t, "name", &'static str);
        if FLAG_A {
            addfield!(t, "mode", &'static str);
        }
        if FLAG_B {
            addfield!(t, "max_retries", i64);
        }
        t
    };

    type ConfigClone = const { clone_struct!(Config) };

    let base = Base { id: 1, name: "core" };
    println!("base: id={} name={}", base.id, base.name);

    let config = Config {
        id: 2,
        name: "primary",
        mode: "strict",
    };
    println!("config: id={} name={} mode={}", config.id, config.name, config.mode);

    let clone = ConfigClone {
        id: 3,
        name: "shadow",
        mode: "relaxed",
    };
    println!(
        "config clone: id={} name={} mode={}",
        clone.id, clone.name, clone.mode
    );

    println!("config fields: {}", field_count!(Config));
    println!("config has mode: {}", hasfield!(Config, "mode"));
    println!("config has max_retries: {}", hasfield!(Config, "max_retries"));
    println!("config size: {}", struct_size!(Config));
    println!("config type: {}", type_name!(Config));
}
