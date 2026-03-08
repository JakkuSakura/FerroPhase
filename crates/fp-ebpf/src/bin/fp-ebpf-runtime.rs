use std::path::PathBuf;

fn main() {
    if let Err(err) = run() {
        eprintln!("error: {}", err);
        std::process::exit(1);
    }
}

fn run() -> fp_core::error::Result<()> {
    let mut args = std::env::args_os();
    let _program = args.next();
    let input = args.next().ok_or_else(|| {
        fp_core::error::Error::from(
            "usage: fp-ebpf-runtime <input.o> [--function <name>]".to_string(),
        )
    })?;

    let mut function = String::from("main");
    while let Some(arg) = args.next() {
        if arg == "--function" {
            let value = args.next().ok_or_else(|| {
                fp_core::error::Error::from("missing value for --function".to_string())
            })?;
            function = value.to_string_lossy().into_owned();
        } else {
            return Err(fp_core::error::Error::from(format!(
                "unknown argument {:?}",
                arg
            )));
        }
    }

    let input = PathBuf::from(input);
    let mut stdout = std::io::stdout().lock();
    let status = fp_ebpf::runtime::run_file(&input, &function, &mut stdout)?;
    std::process::exit(status);
}
