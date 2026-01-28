pub mod bench;
pub mod build;
pub mod check;
pub mod export;
pub mod generate;
pub mod graph;
pub mod init;
pub mod lock;
pub mod run;
pub mod submodule;
pub mod test;
pub mod tree;
pub mod update;
pub mod utils;

pub fn print_usage() {
    eprintln!("magnet (bootstrap)");
    eprintln!("Usage:");
    eprintln!("  magnet init [path] [--from-cargo]");
    eprintln!("  magnet generate [--config <path>]");
    eprintln!("  magnet check [--config <path>]");
    eprintln!("  magnet tree [--config <path>]");
    eprintln!("  magnet graph [path] [--output <file>]");
    eprintln!("  magnet lock [path] [--output <file>]");
    eprintln!("  magnet update [path] [--output <file>]");
    eprintln!("  magnet build [path] [--package <name>] [--entry <file>] [--example <name>] [--release] [--profile <name>] [--build-option <k=v>] [--jobs <n>] [--fp <path>]");
    eprintln!("  magnet run [path] [--package <name>] [--entry <file>] [--example <name>] [--release] [--profile <name>] [-- <args>]");
    eprintln!("  magnet test [path] [--package <name>] [--release] [--profile <name>] [-- <args>]");
    eprintln!("  magnet bench [path] [--package <name>] [--release] [--profile <name>] [-- <args>]");
    eprintln!("  magnet export [path] [--output <dir>] [--crates <dir>]");
    eprintln!("  magnet submodule <list|init|update|deinit|switch> [path] [--remote <url>] [--rev <rev>]");
}
