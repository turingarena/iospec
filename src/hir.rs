extern crate proc_macro2;
extern crate syn;

use std::ops::Deref;
use std::rc::Rc;

use syn::punctuated::Punctuated;

use crate::kw::*;

#[derive(Debug, Clone)]
pub struct HirNode<T>(Rc<T>);

impl<T> HirNode<T> {
    pub fn new(data: T) -> Self {
        Self(Rc::new(data))
    }
}

impl<T> Deref for HirNode<T> {
    type Target = T;

    fn deref(self: &Self) -> &T {
        self.0.deref()
    }
}

#[derive(Debug, Clone)]
pub struct HirBlock {
    pub conses: Vec<HirNode<HirCons>>,
    pub stmts: Vec<HirNode<HirStmt>>,
}

#[derive(Debug, Clone)]
pub struct HirDef {
    pub expr: HirNode<HirDefExpr>,
    pub colon: syn::Token![:],
    pub ty: HirNode<HirScalarTypeExpr>,
}

#[derive(Debug, Clone)]
pub struct HirDefExpr {
    pub kind: HirDefExprKind,
    pub ident: HirNode<HirIdent>,
}

#[derive(Debug, Clone)]
pub enum HirDefExprKind {
    Var {
        ident: HirNode<HirIdent>,
    },
    Subscript {
        array: HirNode<HirDefExpr>,
        bracket: syn::token::Bracket,
        index: HirNode<HirExpr>,
    },
}

#[derive(Debug, Clone)]
pub struct HirExpr {
    pub kind: HirExprKind,
}

#[derive(Debug, Clone)]
pub enum HirExprKind {
    Ref {
        ident: HirNode<HirIdent>,
        target: Option<HirNode<HirRef>>,
    },
    Subscript {
        array: HirNode<HirExpr>,
        bracket: syn::token::Bracket,
        index: HirNode<HirExpr>,
    },
}

#[derive(Debug, Clone)]
pub struct HirIdent {
    pub token: proc_macro2::Ident,
}

#[derive(Debug, Clone)]
pub struct HirSpec {
    pub main: HirNode<HirBlock>,
}

#[derive(Debug, Clone)]
pub struct HirStmt {
    pub conses: Vec<HirNode<HirCons>>,
    pub kind: HirStmtKind,
}

#[derive(Debug, Clone)]
pub enum HirStmtKind {
    Write {
        inst: kw::write,
        args: Vec<HirNode<HirExpr>>,
        arg_commas: Vec<syn::Token![,]>,
        semi: syn::Token![;],
    },
    Read {
        inst: kw::read,
        args: Vec<HirNode<HirDef>>,
        arg_commas: Vec<syn::Token![,]>,
        semi: syn::Token![;],
    },
    Call {
        inst: kw::call,
        name: HirNode<HirIdent>,
        args_paren: syn::token::Paren,
        args: Vec<HirNode<HirExpr>>,
        arg_commas: Vec<syn::Token![,]>,
        return_value_rarrow: Option<syn::Token![->]>,
        return_value: Option<HirNode<HirDef>>,
        semi: syn::Token![;],
    },
    For {
        for_token: syn::Token![for],
        range: HirNode<HirRange>,
        body_brace: syn::token::Brace,
        body: HirNode<HirBlock>,
    },
}

#[derive(Debug, Clone)]
pub struct HirRange {
    pub index_name: HirNode<HirIdent>,
    pub upto: kw::upto,
    pub bound: HirNode<HirExpr>,
}

#[derive(Debug, Clone)]
pub struct HirScalarTypeExpr {
    pub ident: HirNode<HirIdent>,
}

#[derive(Debug, Clone)]
pub struct HirRef {
    pub ident: HirNode<HirIdent>,
    pub kind: HirRefKind,
}

#[derive(Debug, Clone)]
pub enum HirRefKind {
    Var {
        cons: HirNode<HirCons>,
    },
    Index {
        range: HirNode<HirRange>,
    },
}

#[derive(Debug, Clone)]
pub enum HirCons {
    Scalar {
        def: HirNode<HirDef>,
    },
    Array {
        item: HirNode<HirCons>,
        range: HirNode<HirRange>,
    },
}
