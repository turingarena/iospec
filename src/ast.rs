extern crate proc_macro2;
extern crate syn;

use syn::punctuated::Punctuated;

use crate::kw::*;

#[derive(Debug)]
pub struct ASpec {
    pub main: ABlock,
}

#[derive(Debug)]
pub struct ABlock {
    pub stmts: Vec<AStmt>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ADef {
    pub expr: AExpr,
    pub colon: syn::Token![:],
    pub ty: ATy,
}

#[derive(Debug)]
pub struct ATy {
    pub ident: AIdent,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct AIdent {
    pub token: proc_macro2::Ident,
}
