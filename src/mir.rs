//! Middle-level Intermediate Representation (MIR), used for interpretation and code generation.
//!
//! The MIR is a tree of nodes, but its topology can be different from the AST.
//! E.g., the MIR contains extra nodes for the declaration of variables,
//! allocation and de-allocation of arrays, and so on.
//!
//! MIR is meant to be directly interpreted or translated into parser code.
//! Being a tree, it can be easily traversed structurally.
//! (TODO: the following is not implemented yet)
//! However, MIR nodes also contains references to HIR nodes from which they were generated.
//! This is useful, e.g., for reporting detailed errors if they occur during interpretation.

#[derive(Debug, Clone)]
pub struct MSpec {
    pub funs: Vec<MFun>,
    pub main: MBlock,
}

#[derive(Debug, Clone)]
pub struct MFun {
    pub name: String,
    pub params: Vec<MParam>,
    pub ret: Option<MAtomTy>,
}

#[derive(Debug, Clone)]
pub struct MParam {
    pub name: String,
    pub ty: MConsTy,
}

pub type MBlock = Vec<MInst>;

#[derive(Debug, Clone)]
pub enum MInst {
    Decl {
        name: String,
        ty: MConsTy,
    },
    Alloc {
        array: MExpr,
        ty: MConsTy,
        size: MExpr,
    },
    Read {
        arg: MExpr,
        ty: MAtomTy,
    },
    Write {
        arg: MExpr,
        ty: MAtomTy,
    },
    Call {
        name: String,
        args: Vec<MExpr>,
        ret: Option<MExpr>,
    },
    For {
        index_name: String,
        bound: MExpr,
        body: Box<MBlock>,
    },
    // TODO: Alloc, Free, control structures
}

#[derive(Debug, Clone)]
pub enum MExpr {
    Var {
        name: String,
    },
    Subscript {
        array: Box<MExpr>,
        index: Box<MExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum MConsTy {
    Atom { atom: MAtomTy },
    Array { item: Box<MConsTy> },
}

#[derive(Debug, Clone)]
pub enum MAtomTy {
    N32,
    I32,
    N64,
    I64,
}
