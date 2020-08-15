extern crate proc_macro2;
extern crate syn;

use syn::punctuated::Punctuated;

use crate::kw::*;

#[derive(Debug, Clone)]
pub struct ParsedBlock {
    pub stmts: Vec<ParsedStmt>,
}

#[derive(Debug, Clone)]
pub struct ParsedDef {
    pub expr: ParsedExpr,
    pub colon: syn::Token![:],
    pub ty: ParsedScalarTypeExpr,
}

#[derive(Debug, Clone)]
pub enum ParsedExpr {
    Ref {
        ident: ParsedIdent,
    },
    Subscript {
        array: Box<ParsedExpr>,
        bracket: syn::token::Bracket,
        index: Box<ParsedExpr>,
    },
}

#[derive(Debug, Clone)]
pub struct ParsedIdent {
    pub sym: String,
}

#[derive(Debug, Clone)]
pub struct ParsedSpec {
    pub main: ParsedBlock,
}

#[derive(Debug, Clone)]
pub enum ParsedStmt {
    Write {
        inst: kw::write,
        args: Punctuated<ParsedExpr, syn::Token![,]>,
        semi: syn::Token![;],
    },
    Read {
        inst: kw::read,
        args: Punctuated<ParsedDef, syn::Token![,]>,
        semi: syn::Token![;],
    },
    Call {
        inst: kw::call,
        name: ParsedIdent,
        args_paren: syn::token::Paren,
        args: Punctuated<ParsedExpr, syn::Token![,]>,
        return_value: Option<(syn::Token![->], ParsedDef)>,
        semi: syn::Token![;],
    },
    For {
        for_token: syn::Token![for],
        index_name: ParsedIdent,
        upto: kw::upto,
        bound: ParsedExpr,
        body_brace: syn::token::Brace,
        body: ParsedBlock,
    },
}

#[derive(Debug, Clone)]
pub struct ParsedScalarTypeExpr {
    pub ident: ParsedIdent,
}
