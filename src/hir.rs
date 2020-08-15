extern crate proc_macro2;
extern crate syn;

use std::ops::Deref;
use std::rc::Rc;

use syn::punctuated::Punctuated;

use crate::kw::*;

#[derive(Debug, Clone)]
pub struct HN<T>(Rc<T>);

impl<T> HN<T> {
    pub fn new(data: T) -> Self {
        Self(Rc::new(data))
    }
}

impl<T> Deref for HN<T> {
    type Target = T;

    fn deref(self: &Self) -> &T {
        self.0.deref()
    }
}

#[derive(Debug, Clone)]
pub struct HSpec {
    pub main: HN<HBlock>,
}

#[derive(Debug, Clone)]
pub struct HBlock {
    pub conses: Vec<HN<HCons>>,
    pub stmts: Vec<HN<HStmt>>,
}

#[derive(Debug, Clone)]
pub struct HStmt {
    pub conses: Vec<HN<HCons>>,
    pub kind: HStmtKind,
}

#[derive(Debug, Clone)]
pub enum HStmtKind {
    Write {
        inst: kw::write,
        args: Vec<HN<HValExpr>>,
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
        args: Vec<HN<HValExpr>>,
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

#[derive(Debug, Clone)]
pub struct HDef {
    pub expr: HN<HDefExpr>,
    pub colon: syn::Token![:],
    pub ty: HN<HAtomTy>,
}

#[derive(Debug, Clone)]
pub struct HDefExpr {
    pub kind: HDefExprKind,
    pub ident: HN<HIdent>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct HValExpr {
    pub kind: HValExprKind,
}

#[derive(Debug, Clone)]
pub enum HValExprKind {
    Ref {
        ident: HN<HIdent>,
        target: Option<HN<HRef>>,
    },
    Subscript {
        array: HN<HValExpr>,
        bracket: syn::token::Bracket,
        index: HN<HValExpr>,
    },
}

#[derive(Debug, Clone)]
pub struct HRange {
    pub index_name: HN<HIdent>,
    pub upto: kw::upto,
    pub bound: HN<HValExpr>,
}

#[derive(Debug, Clone)]
pub struct HAtomTy {
    pub ident: HN<HIdent>,
}

#[derive(Debug, Clone)]
pub struct HIdent {
    pub token: proc_macro2::Ident,
}

#[derive(Debug, Clone)]
pub struct HRef {
    pub ident: HN<HIdent>,
    pub kind: HRefKind,
}

#[derive(Debug, Clone)]
pub enum HRefKind {
    Var { def: HN<HDef>, cons: HN<HCons> },
    Index { range: HN<HRange> },
}

#[derive(Debug, Clone)]
pub enum HCons {
    Scalar { def: HN<HDef> },
    Array { item: HN<HCons>, range: HN<HRange> },
}
