extern crate syn;

use std::rc::Rc;

use syn::punctuated::Punctuated;

use crate::kw::*;

pub type HirNode<T> = Rc<T>;

#[derive(Debug, Clone)]
pub struct HirBlock {
    pub stmts: Vec<HirNode<HirStmt>>,
}

#[derive(Debug, Clone)]
pub struct HirDef {
    pub expr: HirNode<HirExpr>,
    pub colon: syn::Token![:],
    pub ty: HirNode<HirScalarTypeExpr>,
}

#[derive(Debug, Clone)]
pub struct HirExpr {
    pub kind: HirExprKind,
}

#[derive(Debug, Clone)]
pub enum HirExprKind {
    Ref {
        ident: HirNode<HirIdent>,
    },
    Subscript {
        array: HirNode<HirExpr>,
        bracket: syn::token::Bracket,
        index: HirNode<HirExpr>,
    },
}

#[derive(Debug, Clone)]
pub struct HirIdent {
    pub sym: String,
}

#[derive(Debug, Clone)]
pub struct HirSpec {
    pub main: HirNode<HirBlock>,
}

#[derive(Debug, Clone)]
pub struct HirStmt {
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
        return_value: Option<(syn::Token![->], HirNode<HirDef>)>,
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
