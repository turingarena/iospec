//! Low-level Intermediate Representation (LIR), used for code generation.
//!
//! LIR is meant to be directly translated into parser code.
//! It is a tree without references to higher level nodes (HIR).
//! Thus, it can be easily traversed structurally and translated into code.

use crate::atom::*;

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
    Read {
        args: Vec<LReadArg>,
    },
    Write {
        args: Vec<LWriteArg>,
    },
    Call {
        decl: Option<LDecl>,
        name: String,
        args: Vec<LExpr>,
        ret: Option<LExpr>,
    },
    For {
        allocs: Vec<LAlloc>,
        index: LDecl,
        index_ty: AtomTy,
        bound: LExpr,
        body: LBlock,
    },
}

#[derive(Debug, Clone)]
pub struct LDecl {
    pub ty: LTy,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct LReadArg {
    pub decl: Option<LDecl>,
    pub expr: LExpr,
    pub ty: AtomTy,
}

#[derive(Debug, Clone)]
pub struct LWriteArg {
    pub expr: LExpr,
    pub ty: AtomTy,
}

#[derive(Debug, Clone)]
pub struct LAlloc {
    pub decl: Option<LDecl>,
    pub array: LExpr,
    pub item_ty: LTy,
    pub size: LExpr,
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
    Lit {
        value: i64,
    },
    Paren {
        inner: Box<LExpr>,
    },
    Mul {
        factors: Vec<LExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum LTy {
    Atom { atom: AtomTy },
    Array { item: Box<LTy> },
}
