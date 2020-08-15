extern crate proc_macro2;
extern crate syn;

use syn::punctuated::Punctuated;

use crate::kw::*;

#[derive(Debug, Clone)]
pub struct ABlock {
    pub stmts: Vec<AStmt>,
}

#[derive(Debug, Clone)]
pub struct ADef {
    pub expr: AExpr,
    pub colon: syn::Token![:],
    pub ty: ATy,
}

#[derive(Debug, Clone)]
pub enum AExpr {
    Ref {
        ident: AIdent,
    },
    Subscript {
        array: Box<AExpr>,
        bracket: syn::token::Bracket,
        index: Box<AExpr>,
    },
}

#[derive(Debug, Clone)]
pub struct AIdent {
    pub token: proc_macro2::Ident,
}

#[derive(Debug, Clone)]
pub struct ASpec {
    pub main: ABlock,
}

#[derive(Debug, Clone)]
pub enum AStmt {
    Write {
        inst: kw::write,
        args: Punctuated<AExpr, syn::Token![,]>,
        semi: syn::Token![;],
    },
    Read {
        inst: kw::read,
        args: Punctuated<ADef, syn::Token![,]>,
        semi: syn::Token![;],
    },
    Call {
        inst: kw::call,
        name: AIdent,
        args_paren: syn::token::Paren,
        args: Punctuated<AExpr, syn::Token![,]>,
        ret: Option<(syn::Token![->], ADef)>,
        semi: syn::Token![;],
    },
    For {
        for_token: syn::Token![for],
        index_name: AIdent,
        upto: kw::upto,
        bound: AExpr,
        body_brace: syn::token::Brace,
        body: ABlock,
    },
}

#[derive(Debug, Clone)]
pub struct ATy {
    pub ident: AIdent,
}
