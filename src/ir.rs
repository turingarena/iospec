//! Intermediate representation (IR)
//!
//! Compiled AST is expanded into IR, adding variable declarations,
//! construction and destruction, array allocations and de-allocations,
//! and expanding reads and writes.

use crate::compile::*;

#[derive(Debug, Clone)]
pub enum IrInst<'a> {
    Decl { inner: &'a CompiledDecl<'a> },
    Read { decl: &'a CompiledDecl<'a> },
    Write { expr: &'a CompiledExpr<'a> },
    Call { inner: &'a CompiledStmt<'a> },
    For { inner: &'a CompiledStmt<'a> },
    // TODO: Alloc, Free, control structures
}

pub type IrBlock<'a> = Vec<IrInst<'a>>;

pub struct IrSpec<'a> {
    pub main: IrBlock<'a>,
}

impl<'a> CompiledStmt<'a> {
    fn build_ir(self: &'a Self) -> Vec<IrInst<'a>> {
        let stmt = self;
        match self {
            CompiledStmt::Read { args, .. } => args
                .iter()
                .flat_map(|decl| {
                    vec![
                        IrInst::Decl { inner: decl },
                        IrInst::Read { decl },
                    ]
                })
                .collect(),
            CompiledStmt::Write { args, .. } => args
                .iter()
                .map(|expr| IrInst::Write { expr })
                .collect(),
            CompiledStmt::Call { return_value, .. } => vec![
                return_value
                    .iter()
                    .map(|decl| IrInst::Decl { inner: decl })
                    .collect(),
                vec![IrInst::Call { inner: stmt }],
            ]
                .into_iter()
                .flatten()
                .collect(),
            CompiledStmt::For { .. } => vec![
                IrInst::For { inner: stmt },
            ],
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
