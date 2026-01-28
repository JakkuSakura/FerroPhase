use std::env;

use magnet::commands;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() <= 1 {
        commands::print_usage();
        std::process::exit(1);
    }

    let command = args[1].as_str();
    let rest = &args[2..];

    let result = match command {
        "init" => commands::init::run(rest),
        "generate" => commands::generate::run(rest),
        "check" => commands::check::run(rest),
        "tree" => commands::tree::run(rest),
        "graph" => commands::graph::run(rest),
        "lock" => commands::lock::run(rest),
        "update" => commands::update::run(rest),
        "build" => commands::build::run(rest),
        "run" => commands::run::run(rest),
        "test" => commands::test::run(rest),
        "bench" => commands::bench::run(rest),
        "export" => commands::export::run(rest),
        "submodule" => commands::submodule::run(rest),
        "help" | "--help" | "-h" => {
            commands::print_usage();
            Ok(())
        }
        _ => {
            commands::print_usage();
            Err(format!("unknown command '{command}'").into())
        }
    };

    if let Err(err) = result {
        eprintln!("magnet: {err}");
        std::process::exit(1);
    }
}
