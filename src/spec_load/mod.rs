//! Parses a spec file and compiles it to High-level Intermediate Representation (HIR)

pub mod ast;
pub mod ast_parse;
pub mod diagnostic;
pub mod hir_compile;
pub mod hir_env;
pub mod hir_err;

pub mod sess;
