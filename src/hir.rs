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

#[derive(Debug)]
pub struct HSpec {
    pub main: Rc<HStmt>,
    pub funs: Vec<Rc<HFun>>,
}

/// An executable part of the spec, i.e., either a statement or block.
#[derive(Debug)]
pub enum HStmt {
    Block {
        stmts: Vec<Rc<HStmt>>,
    },
    Write {
        kw: kw::write,
        args: Vec<Rc<HVal>>,
        arg_commas: Vec<syn::Token![,]>,
        semi: syn::Token![;],
    },
    Read {
        kw: kw::read,
        args: Vec<Rc<HAtom>>,
        arg_commas: Vec<syn::Token![,]>,
        semi: syn::Token![;],
    },
    Call {
        kw: kw::call,
        fun: Rc<HFun>,
        semi: syn::Token![;],
    },
    For {
        kw: syn::Token![for],
        range: Rc<HRange>,
        body_brace: syn::token::Brace,
        body: Rc<HStmt>,
    },
}

#[derive(Debug)]
pub struct HFun {
    pub name: Rc<HIdent>,
    pub args: Vec<Rc<HArg>>,
    pub ret: Option<Rc<HAtom>>,

    pub args_paren: syn::token::Paren,
    pub arg_commas: Vec<syn::Token![,]>,
    pub ret_rarrow: Option<syn::Token![->]>,
}

#[derive(Debug)]
pub struct HArg {
    pub name: Rc<HIdent>,
    pub ty: Rc<HExprTy>,
    pub val: Rc<HVal>,
}

#[derive(Debug)]
pub struct HAtom {
    pub node: Rc<HNode>,
    pub colon: syn::Token![:],
    pub ty: Rc<HAtomTy>,
}

#[derive(Debug)]
pub struct HNode {
    pub expr: Rc<HNodeExpr>,
    pub root: Rc<HDataVar>,
    pub ty: Rc<HExprTy>,
}

#[derive(Debug)]
pub enum HNodeExpr {
    Var {
        var: Rc<HDataVar>,
    },
    Subscript {
        array: Rc<HNode>,
        bracket: syn::token::Bracket,
        index: Rc<HIndex>,
    },
}

#[derive(Debug)]
pub struct HIndex {
    pub name: Rc<HIdent>,
    pub range: Rc<HRange>,
}

#[derive(Debug)]
pub struct HDataVar {
    pub name: Rc<HIdent>,
    pub ty: Rc<HExprTy>,
}

#[derive(Debug)]
pub struct HVal {
    pub expr: Rc<HValExpr>,
    pub ty: Rc<HExprTy>,
}

#[derive(Debug)]
pub enum HValExpr {
    Var {
        var: Rc<HVar>,
        ident: Rc<HIdent>,
    },
    Subscript {
        array: Rc<HVal>,
        bracket: syn::token::Bracket,
        index: Rc<HVal>,
    },
}

#[derive(Debug)]
pub struct HRange {
    pub index: Rc<HIdent>,
    pub upto: kw::upto,
    pub bound: Rc<HVal>,
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
    pub name: Rc<HIdent>,
    pub ty: Rc<HExprTy>, // Cache
    pub kind: HVarKind,
}

#[derive(Debug)]
pub enum HVarKind {
    Data { var: Rc<HDataVar> },
    Index { range: Rc<HRange> },
}

#[derive(Debug, Clone)]
pub enum HNodeLoc {
    Main,
    For {
        range: Rc<HRange>,
        parent: Rc<HNodeLoc>,
    },
}
