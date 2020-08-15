extern crate proc_macro2;
extern crate syn;

use syn::punctuated::Punctuated;

use crate::kw::*;

#[derive(Debug, Clone)]
pub struct AstBlock {
    pub stmts: Vec<AstStmt>,
}

#[derive(Debug, Clone)]
pub struct AstDef {
    pub expr: AstExpr,
    pub colon: syn::Token![:],
    pub ty: AstScalarTypeExpr,
}

#[derive(Debug, Clone)]
pub enum AstExpr {
    Ref {
        ident: AstIdent,
    },
    Subscript {
        array: Box<AstExpr>,
        bracket: syn::token::Bracket,
        index: Box<AstExpr>,
    },
}

#[derive(Debug, Clone)]
pub struct AstIdent {
    pub token: proc_macro2::Ident,
}

#[derive(Debug, Clone)]
pub struct AstSpec {
    pub main: AstBlock,
}

#[derive(Debug, Clone)]
pub enum AstStmt {
    Write {
        inst: kw::write,
        args: Punctuated<AstExpr, syn::Token![,]>,
        semi: syn::Token![;],
    },
    Read {
        inst: kw::read,
        args: Punctuated<AstDef, syn::Token![,]>,
        semi: syn::Token![;],
    },
    Call {
        inst: kw::call,
        name: AstIdent,
        args_paren: syn::token::Paren,
        args: Punctuated<AstExpr, syn::Token![,]>,
        return_value: Option<(syn::Token![->], AstDef)>,
        semi: syn::Token![;],
    },
    For {
        for_token: syn::Token![for],
        index_name: AstIdent,
        upto: kw::upto,
        bound: AstExpr,
        body_brace: syn::token::Brace,
        body: AstBlock,
    },
}

#[derive(Debug, Clone)]
pub struct AstScalarTypeExpr {
    pub ident: AstIdent,
}
