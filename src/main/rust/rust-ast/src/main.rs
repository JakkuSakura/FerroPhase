use std::io::{Read, stdin, stdout, Write};
use std::process::{Command, exit, Stdio};
use std::str::FromStr;
use eyre::*;

use clap::Parser;
use regex::Regex;


#[derive(Copy, Clone, Debug)]
enum Flavor {
    Rustc,
    RustcOld,
    Syn,
}

impl FromStr for Flavor {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let v = match s.to_ascii_lowercase().as_str() {
            "rustc" => Flavor::Rustc,
            "rustc-old" => Flavor::RustcOld,
            "syn" => Flavor::Syn,
            _ => bail!("cannot recognize {}", s)
        };
        Ok(v)
    }
}

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct CliArguments {
    #[clap(long, default_value = "rustc-old")]
    flavor: Flavor,
}

fn remove_trailing_comma_in_some(data: &[u8]) -> String {
    let mut some_layers = Vec::new();
    let mut layer = 0;
    let mut output = Vec::new();
    for (i, &c) in data.iter().enumerate() {
        match c {
            b'(' => {
                layer += 1;
                if i >= 4 && data[..i].ends_with(b"Some") {
                    some_layers.push(layer)
                }
            }
            b')' => {
                if some_layers.last().cloned() == Some(layer) {
                    some_layers.pop();
                }
                if layer > 0 {
                    layer -= 1;
                }
            }
            _ => {}
        }
        if !(some_layers.last().cloned() == Some(layer) && c == b',') {
            output.push(c);
        }
    }
    String::from_utf8_lossy(&output).to_string()
}

fn process_rustc(data: &str) -> Result<serde_json::Value> {
    let mut process = Command::new("rustc")
        .args(["-Z", "unpretty=ast-tree", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    let mut stdin = process.stdin.take().unwrap();
    stdin.write_all(data.as_bytes())?;
    stdin.flush()?;
    drop(stdin);
    let output = process.wait_with_output()?;
    if !output.status.success() {
        let err = String::from_utf8_lossy(&output.stderr);
        bail!("rustc run error: {:?} {}", output.status, err)
    }
    let mut data = String::from_utf8_lossy(&output.stdout).to_string();
    let replacements = [
        ("\\{", "("),
        ("\\}", ")"),
        ("No", "false"),
        ("Yes", "true"),
        (r"<anon>:(\d+):(\d+): (\d+):(\d+) \(#(\d+)\)", "Anon($1,$2,$3,$4,$5)"),
        (r"no-location \(#(\d+)\)", "NoLocation($1)"),
        ("ident:\\s*(.+?),", "ident: \"$1\","),
    ];

    for (re, rep) in replacements {
        data = Regex::new(re)?.replace_all(&data, rep).to_string();
    }
    data = remove_trailing_comma_in_some(data.as_bytes());

    // eprintln!("parsing value {}", data);
    let value: ron::Value = ron::from_str(&data)?;
    // eprintln!("parsed value {}", ron::to_string(&value)?);
    let j = serde_json::to_value(&value)?;
    Ok(j)
}
fn process_rustc_old(data: &str) -> Result<serde_json::Value> {
    let mut process = Command::new("rustc")
        .args(["-Z", "ast-json", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    let mut stdin = process.stdin.take().unwrap();
    stdin.write_all(data.as_bytes())?;
    stdin.flush()?;
    drop(stdin);
    let output = process.wait_with_output()?;
    if !output.status.success() {
        let err = String::from_utf8_lossy(&output.stderr);
        bail!("rustc run error: {:?} {}", output.status, err)
    }
    let data = String::from_utf8_lossy(&output.stdout).to_string();
    let j = serde_json::from_str(&data)?;
    Ok(j)
}

fn process_syn(data: &str) -> Result<serde_json::Value> {
    let syntax: syn::File = syn::parse_str(data)?;
    let j = syn_serde::json::to_string(&syntax);
    let j = serde_json::from_str(&j)?;
    Ok(j)
}

fn process(flavor: Flavor) -> Result<()> {
    let mut data = String::new();
    stdin().read_to_string(&mut data)?;

    let value = match flavor {
        Flavor::Rustc => process_rustc(&data),
        Flavor::RustcOld => process_rustc_old(&data),
        Flavor::Syn => process_syn(&data)
    }?;
    serde_json::to_writer_pretty(stdout(), &value)?;
    Ok(())
}

fn main() {
    let args: CliArguments = CliArguments::parse();

    if let Err(err) = process(args.flavor) {
        eprintln!("Error: {:?}", err);
        exit(1)
    }
}
