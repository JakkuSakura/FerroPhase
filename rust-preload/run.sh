#!/bin/bash
export PRELOAD_DIR=`cargo run -- --path examples/src`
echo PRELOAD_DIR=$PRELOAD_DIR
cargo run --example specialize
