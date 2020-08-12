//! Intermediate representation (IR)
//!
//! Compiled AST is expanded into IR, adding variable declarations,
//! construction and destruction, array allocations and de-allocations,
//! and expanding reads and writes.

use crate::compile::*;

type CompileResult<T> = Result<T, String>;

#[derive(Debug, Clone)]
pub enum IrInst<'a> {
    Decl(IrInstDecl<'a>),
    Read(IrInstRead<'a>),
    Write(IrInstWrite<'a>),
    Call(IrInstCall<'a>),
    // TODO: Alloc, Free, control structures
}

#[derive(Debug, Clone)]
pub struct IrInstDecl<'a> {
    pub inner: &'a CompiledDecl<'a>,
}

#[derive(Debug, Clone)]
pub struct IrInstRead<'a> {
    pub decl: &'a CompiledDecl<'a>,
}

#[derive(Debug, Clone)]
pub struct IrInstWrite<'a> {
    pub expr: &'a CompiledExpr<'a>,
}

#[derive(Debug, Clone)]
pub struct IrInstCall<'a> {
    pub inner: &'a CompiledStmtCall<'a>,
}

pub type IrBlock<'a> = Vec<IrInst<'a>>;

pub struct IrSpec<'a> {
    pub main: IrBlock<'a>,
}

impl<'a> CompiledStmt<'a> {
    fn build_ir(self: &'a Self) -> Vec<IrInst<'a>> {
        match self {
            CompiledStmt::Read(stmt) => stmt
                .args
                .iter()
                .flat_map(|decl| {
                    vec![
                        IrInst::Decl(IrInstDecl { inner: decl }),
                        IrInst::Read(IrInstRead { decl }),
                    ]
                })
                .collect(),
            CompiledStmt::Write(stmt) => stmt
                .args
                .iter()
                .map(|expr| IrInst::Write(IrInstWrite { expr }))
                .collect(),
            CompiledStmt::Call(stmt) => vec![
                stmt.return_value
                    .iter()
                    .map(|decl| IrInst::Decl(IrInstDecl { inner: decl }))
                    .collect(),
                vec![IrInst::Call(IrInstCall { inner: stmt })],
            ]
            .into_iter()
            .flatten()
            .collect(),
        }
    }
}

impl<'a> CompiledBlock<'a> {
    pub fn build_ir(self: &'a Self) -> Vec<IrInst<'a>> {
        self.stmts.iter().flat_map(|stmt| stmt.build_ir()).collect()
    }
}

impl<'a> CompiledSpec<'a> {
    pub fn build_ir(self: &'a Self) -> Vec<IrInst<'a>> {
        self.main.build_ir()
    }
}
