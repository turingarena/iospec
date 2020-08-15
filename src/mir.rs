use std::rc::Rc;

use crate::hir::*;

#[derive(Debug, Clone)]
pub struct MirSpec {
    pub main: MirBlock,
}

pub type MirBlock = Vec<MirInst>;

#[derive(Debug, Clone)]
pub enum MirInst {
    Decl {
        name: String,
        ty: MirConsTy,
    },
    Read {
        arg: MirExpr,
        ty: MirDefTy,
    },
    Write {
        arg: MirExpr,
        ty: MirDefTy,
    },
    Call {
        name: String,
        args: Vec<MirExpr>,
        ret: Option<MirExpr>,
    },
    For {
        index_name: String,
        bound: MirExpr,
        body: Box<MirBlock>,
    },
    // TODO: Alloc, Free, control structures
}

#[derive(Debug, Clone)]
pub enum MirExpr {
    Var {
        name: String,
    },
    Subscript {
        array: Box<MirExpr>,
        index: Box<MirExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum MirConsTy {
    Scalar {
        def: MirDefTy,
    },
    Array {
        item: Box<MirConsTy>,
    },
}

#[derive(Debug, Clone)]
pub enum MirDefTy {
    N32,
    I32,
    N64,
    I64,
}