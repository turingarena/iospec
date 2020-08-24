//! Parses a spec file and compiles it to High-level Intermediate Representation (HIR)

mod ast;
mod ast_parse;
mod ast_span;

mod diagnostic;
mod hir_compile;
mod hir_env;
mod hir_err;

mod main;

pub use main::*;
