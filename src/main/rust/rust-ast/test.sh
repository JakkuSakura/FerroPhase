#!/bin/sh

cargo build

echo "fn main(){}" | cargo run -- --flavor rustc
echo "fn main(){}" | cargo run -- --flavor rustc-old
echo "fn main(){}" | cargo run -- --flavor syn
