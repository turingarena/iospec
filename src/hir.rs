//! High-level Intermediate Representation (HIR), used for semantic analysis.
//!
//! The HIR has a topology similar to the AST, but it also has links from each node
//! to any other nodes it refers to.
//! E.g., names are resolved by introducing a link to the node where the name is defined.
//!
//! HIR nodes only link to nodes which occur *before* in a post-order traversal of the AST.
//! Hence, HIR nodes result in a directed acyclic graph (DAG).
//! To represent links, nodes are wrapped in `std::rc:Rc` pointers.
//! Since there are no cycles, no `std::rc:Weak` reference is needed.
//!
//! The HIR contains references to all the *tokens* in the original AST, and all the information
//! needed to reconstruct the AST tree, but does not keep any reference to the tree itself.

extern crate proc_macro2;
extern crate syn;

pub use std::rc::Rc;

use crate::kw;
use crate::mir::MFun;

#[derive(Debug)]
pub struct HSpec {
    pub funs: Rc<Vec<Rc<HFun>>>,
    pub main: Rc<HBlock>,
}

#[derive(Debug)]
pub struct HBlock {
    pub funs: Rc<Vec<Rc<HFun>>>,
    pub vars: Rc<Vec<Rc<HVar>>>,
    pub defs: Rc<Vec<Rc<HDefExpr>>>,
    pub stmts: Rc<Vec<Rc<HStmt>>>,
}

#[derive(Debug)]
pub struct HStmt {
    pub funs: Rc<Vec<Rc<HFun>>>,
    pub vars: Rc<Vec<Rc<HVar>>>,
    pub defs: Rc<Vec<Rc<HDefExpr>>>,
    pub kind: HStmtKind,
}

#[derive(Debug)]
pub enum HStmtKind {
    Write {
        kw: kw::write,
        args: Rc<Vec<Rc<HValExpr>>>,
        arg_commas: Vec<syn::Token![,]>,
        semi: syn::Token![;],
    },
    Read {
        kw: kw::read,
        args: Vec<Rc<HDef>>,
        arg_commas: Vec<syn::Token![,]>,
        semi: syn::Token![;],
    },
    Call {
        kw: kw::call,
        fun: Rc<HFun>,
        args: Rc<Vec<Rc<HValExpr>>>,
        args_paren: syn::token::Paren,
        arg_commas: Vec<syn::Token![,]>,
        ret_rarrow: Option<syn::Token![->]>,
        semi: syn::Token![;],
    },
    For {
        kw: syn::Token![for],
        range: Rc<HRange>,
        body_brace: syn::token::Brace,
        body: Rc<HBlock>,
    },
}

#[derive(Debug)]
pub struct HFun {
    pub name: Rc<HIdent>,
    pub params: Rc<Vec<Rc<HParam>>>,
    pub ret: Option<Rc<HDef>>,
}

#[derive(Debug)]
pub struct HParam {
    pub name: Rc<HIdent>,
    pub ty: Rc<HExprTy>,
}

#[derive(Debug)]
pub struct HDef {
    pub expr: Rc<HDefExpr>,
    pub colon: syn::Token![:],
    pub atom_ty: Rc<HAtomTy>,
    pub var_ty: Rc<HExprTy>,
    pub ident: Rc<HIdent>,
}

#[derive(Debug)]
pub struct HDefExpr {
    pub kind: HDefExprKind,
    pub ident: Rc<HIdent>,
    pub expr_ty: Rc<HExprTy>,
    pub var_ty: Rc<HExprTy>,
}

#[derive(Debug)]
pub enum HDefExprKind {
    Var {
        ident: Rc<HIdent>,
    },
    Subscript {
        array: Rc<HDefExpr>,
        bracket: syn::token::Bracket,
        index: Rc<HValExpr>,
    },
}

#[derive(Debug)]
pub struct HValExpr {
    pub ty: Rc<HExprTy>,
    pub kind: HValExprKind,
}

#[derive(Debug)]
pub enum HValExprKind {
    Var {
        var: Option<Rc<HVar>>,
        ident: Rc<HIdent>,
    },
    Subscript {
        array: Rc<HValExpr>,
        bracket: syn::token::Bracket,
        index: Rc<HValExpr>,
    },
}

#[derive(Debug)]
pub struct HRange {
    pub index: Rc<HIdent>,
    pub upto: kw::upto,
    pub bound: Rc<HValExpr>,
}

#[derive(Debug)]
pub enum HExprTy {
    Atom {
        atom: Rc<HAtomTy>,
    },
    Array {
        item: Rc<HExprTy>,
        range: Rc<HRange>,
    },
    Index {
        range: Rc<HRange>,
    },
}

#[derive(Debug)]
pub struct HAtomTy {
    pub ident: Rc<HIdent>,
}

#[derive(Debug)]
pub struct HIdent {
    pub token: proc_macro2::Ident,
}

#[derive(Debug)]
pub struct HVar {
    pub ident: Rc<HIdent>,
    pub kind: HVarKind,
}

#[derive(Debug)]
pub enum HVarKind {
    Data { def: Rc<HDef> },
    Index { range: Rc<HRange> },
}
