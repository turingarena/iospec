//! Low-level Intermediate Representation (LIR), used for code generation.
//!
//! LIR is meant to be directly translated into parser code.
//! It is a tree without references to higher level nodes (HIR).
//! Thus, it can be easily traversed structurally and translated into code.

use crate::ty::*;

#[derive(Debug, Clone)]
pub struct LSpec {
    pub funs: Vec<LFun>,
    pub main: LBlock,
}

#[derive(Debug, Clone)]
pub struct LFun {
    pub name: String,
    pub params: Vec<LParam>,
    pub ret: Option<AtomTy>,
}

#[derive(Debug, Clone)]
pub struct LParam {
    pub name: String,
    pub ty: LTy,
}

#[derive(Debug, Clone)]
pub struct LBlock {
    pub stmts: Vec<LStmt>,
}

#[derive(Debug, Clone)]
pub enum LStmt {
    Decl {
        name: String,
        ty: LTy,
    },
    Alloc {
        array: LExpr,
        item_ty: LTy,
        size: LExpr,
    },
    Read {
        arg: LExpr,
        ty: AtomTy,
    },
    Write {
        arg: LExpr,
        ty: AtomTy,
    },
    Call {
        name: String,
        args: Vec<LExpr>,
        ret: Option<LExpr>,
    },
    For {
        index_name: String,
        bound: LExpr,
        body: LBlock,
    },
}

#[derive(Debug, Clone)]
pub enum LExpr {
    Var {
        name: String,
    },
    Subscript {
        array: Box<LExpr>,
        index: Box<LExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum LTy {
    Atom { atom: AtomTy },
    Array { item: Box<LTy> },
}
