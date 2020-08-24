//! Abstract Syntax Tree (AST), obtained by parsing spec file syntax.

extern crate proc_macro2;
extern crate syn;

use syn::punctuated::Punctuated;

use crate::spec::kw;
use crate::spec::rel::*;

/// AST of a spec file as a whole. Wraps `ABlock`.
#[derive(Debug)]
pub struct ASpec {
    pub main: ABlock,
}

/// AST of a block (sequence of statements), e.g. `read A: n32; call f(A) -> S: i32;`.
#[derive(Debug)]
pub struct ABlock {
    pub stmts: Vec<AStmt>,
}

/// AST of a statement, e.g., `read A: n32;` or `for i upto N { [...] }`.
#[derive(Debug)]
pub enum AStmt {
    /// AST of, e.g., `write N, A[i];`.
    Write {
        kw: kw::write,
        args: Punctuated<AExpr, syn::Token![,]>,
        semi: syn::Token![;],
    },
    /// AST of, e.g., `read N: n32, A[i]: i64;`.
    Read {
        kw: kw::read,
        args: Punctuated<ADef, syn::Token![,]>,
        semi: syn::Token![;],
    },
    /// AST of, e.g., `call f(N, M, A, B) -> S: i64;`.
    Call {
        kw: kw::call,
        name: AIdent,
        args_paren: syn::token::Paren,
        args: Punctuated<AExpr, syn::Token![,]>,
        ret: Option<(syn::Token![->], ADef)>,
        semi: syn::Token![;],
    },
    /// AST of, e.g., `for i upto M[i] { [...] }`.
    For {
        kw: syn::Token![for],
        index: AIdent,
        upto: kw::upto,
        bound: AExpr,
        body_brace: syn::token::Brace,
        body: ABlock,
    },
}

/// AST of, e.g., `N: n32` or `A[i]: i32`.
#[derive(Debug)]
pub struct ADef {
    pub expr: AExpr,
    pub colon: syn::Token![:],
    pub ty: ATy,
}

/// AST of, e.g, `n32`.
#[derive(Debug)]
pub struct ATy {
    pub ident: AIdent,
}

/// AST of, e.g, `N[A[i]]` (or `A[i] * N + 1` when implemented).
#[derive(Debug)]
pub enum AExpr {
    IntLit {
        token: syn::LitInt,
    },
    Ref {
        ident: AIdent,
    },
    Subscript {
        array: Box<AExpr>,
        bracket: syn::token::Bracket,
        index: Box<AExpr>,
    },
    Paren {
        paren: syn::token::Paren,
        inner: Box<AExpr>,
    },
    Mul {
        factors: syn::punctuated::Punctuated<AExpr, syn::Token![*]>,
    },
    Sum {
        first_sign: Option<ASign>,
        terms: syn::punctuated::Punctuated<AExpr, ASign>,
    },
    RelChain {
        chain: syn::punctuated::Punctuated<AExpr, RelOp>,
    },
}

#[derive(Debug)]
pub enum ASign {
    Plus(syn::Token![+]),
    Minus(syn::Token![-]),
}

/// AST of an identifier, including variable names, function names, and types.
#[derive(Debug)]
pub struct AIdent {
    pub token: proc_macro2::Ident,
}
