extern crate proc_macro2;
extern crate syn;

use std::ops::Deref;
use std::rc::Rc;

use syn::punctuated::Punctuated;

use crate::kw::*;

pub type HN<T> = Rc<T>;

#[derive(Debug)]
pub struct HSpec {
    pub main: HN<HBlock>,
}

#[derive(Debug)]
pub struct HBlock {
    pub decls: HN<Vec<HN<HDecl>>>,
    pub defs: HN<Vec<HN<HDefExpr>>>,
    pub stmts: HN<Vec<HN<HStmt>>>,
}

#[derive(Debug)]
pub struct HStmt {
    pub decls: HN<Vec<HN<HDecl>>>,
    pub defs: HN<Vec<HN<HDefExpr>>>,
    pub kind: HStmtKind,
}

#[derive(Debug)]
pub enum HStmtKind {
    Write {
        inst: kw::write,
        args: HN<Vec<HN<HValExpr>>>,
        arg_commas: Vec<syn::Token![,]>,
        semi: syn::Token![;],
    },
    Read {
        inst: kw::read,
        args: Vec<HN<HDef>>,
        arg_commas: Vec<syn::Token![,]>,
        semi: syn::Token![;],
    },
    Call {
        inst: kw::call,
        name: HN<HIdent>,
        args_paren: syn::token::Paren,
        args: HN<Vec<HN<HValExpr>>>,
        arg_commas: Vec<syn::Token![,]>,
        ret_rarrow: Option<syn::Token![->]>,
        ret: Option<HN<HDef>>,
        semi: syn::Token![;],
    },
    For {
        for_token: syn::Token![for],
        range: HN<HRange>,
        body_brace: syn::token::Brace,
        body: HN<HBlock>,
    },
}

#[derive(Debug)]
pub struct HDef {
    pub expr: HN<HDefExpr>,
    pub colon: syn::Token![:],
    pub atom_ty: HN<HAtomTy>,
    pub var_ty: HN<HExprTy>,
    pub ident: HN<HIdent>,
}

#[derive(Debug)]
pub struct HDefExpr {
    pub kind: HDefExprKind,
    pub ident: HN<HIdent>,
    pub expr_ty: HN<HExprTy>,
    pub var_ty: HN<HExprTy>,
}

#[derive(Debug)]
pub enum HDefExprKind {
    Var {
        ident: HN<HIdent>,
    },
    Subscript {
        array: HN<HDefExpr>,
        bracket: syn::token::Bracket,
        index: HN<HValExpr>,
    },
}

#[derive(Debug)]
pub struct HValExpr {
    pub ty: HN<HExprTy>,
    pub kind: HValExprKind,
}

#[derive(Debug)]
pub enum HValExprKind {
    Ref {
        ident: HN<HIdent>,
        target: Option<HN<HDecl>>,
    },
    Subscript {
        array: HN<HValExpr>,
        bracket: syn::token::Bracket,
        index: HN<HValExpr>,
    },
}

#[derive(Debug)]
pub struct HRange {
    pub index_name: HN<HIdent>,
    pub upto: kw::upto,
    pub bound: HN<HValExpr>,
}

#[derive(Debug)]
pub enum HExprTy {
    Atom {
        atom: HN<HAtomTy>,
    },
    Array {
        item: HN<HExprTy>,
        range: HN<HRange>,
    },
    Index {
        range: HN<HRange>,
    },
}

#[derive(Debug)]
pub struct HAtomTy {
    pub ident: HN<HIdent>,
}

#[derive(Debug)]
pub struct HIdent {
    pub token: proc_macro2::Ident,
}

#[derive(Debug)]
pub struct HDecl {
    pub ident: HN<HIdent>,
    pub kind: HDeclKind,
}

#[derive(Debug)]
pub enum HDeclKind {
    Var { def: HN<HDef> },
    Index { range: HN<HRange> },
}
