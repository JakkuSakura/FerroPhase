include!("defs/mod.rs");
use shell_lang::*;

fn main() {
    let src = SourceProcess::spawn("src", 1);
    let add = AddProcess::spawn("add");
    let dest = SinkProcess::spawn("dest");
    let _ = pipe!(src | add | dest).start();
    let _ = pipe!(src | add | dest).start();

}