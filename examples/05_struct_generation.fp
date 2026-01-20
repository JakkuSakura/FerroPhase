#!/usr/bin/env fp run
//! Struct generation with compile-time conditionals

use std::meta::TypeBuilder;

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
        TypeBuilder::new("Base")
            .with_field("id", i64)
            .with_field("name", &'static str)
            .build()
    };

    type Config = const {
        let mut builder = TypeBuilder::new("Config");
        builder = builder.with_field("id", i64);
        builder = builder.with_field("name", &'static str);
        if FLAG_A {
            builder = builder.with_field("mode", &'static str);
        }
        if FLAG_B {
            builder = builder.with_field("max_retries", i64);
        }
        builder.build()
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

    println!("config fields: {}", type(Config).fields.len());
    println!(
        "config has mode: {}",
        type(Config).fields.contains("mode")
    );
    println!(
        "config has max_retries: {}",
        type(Config).fields.contains("max_retries")
    );
    println!("config size: {}", type(Config).size);
    println!("config type: {}", type(Config).name);
}
