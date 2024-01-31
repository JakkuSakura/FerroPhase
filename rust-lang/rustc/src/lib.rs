#![feature(rustc_private)]
#![feature(cell_update)]
#![feature(float_gamma)]
#![feature(map_try_insert)]
#![feature(never_type)]
#![feature(try_blocks)]
#![feature(io_error_more)]
#![feature(variant_count)]
#![feature(yeet_expr)]
#![feature(nonzero_ops)]
#![feature(let_chains)]
#![feature(lint_reasons)]
#![allow(unused_imports)]
extern crate either; // the one from rustc

extern crate rustc_apfloat;
extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_index;
#[macro_use]
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

// Necessary to pull in object code as the rest of the rustc crates are shipped only as rmeta
// files.
#[allow(unused_extern_crates)]
extern crate rustc_driver;
