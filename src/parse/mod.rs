extern crate proc_macro2;
extern crate syn;

use syn::parse::Parse;
use syn::parse::ParseBuffer;
use syn::punctuated::Punctuated;
use syn::Error;

mod ident;
pub use ident::*;

mod types;
pub use types::*;

mod expr;
pub use expr::*;

mod decl;
pub use decl::*;

mod kw {
    syn::custom_keyword!(read);
    syn::custom_keyword!(write);
    syn::custom_keyword!(call);
    syn::custom_keyword!(upto);
}

#[derive(Debug, Clone)]
pub enum ParsedStmt {
    Write(ParsedStmtWrite),
    Read(ParsedStmtRead),
    Call(ParsedStmtCall),
    For(ParsedStmtFor),
}

impl Parse for ParsedStmt {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        if input.peek(kw::read) {
            Ok(ParsedStmt::Read(input.parse()?))
        } else if input.peek(kw::write) {
            Ok(ParsedStmt::Write(input.parse()?))
        } else if input.peek(kw::call) {
            Ok(ParsedStmt::Call(input.parse()?))
        } else if input.peek(syn::Token![for]) {
            Ok(ParsedStmt::For(input.parse()?))
        } else {
            Err(Error::new(input.span(), "statement expected"))
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedStmtWrite {
    pub inst: kw::write,
    pub args: Punctuated<ParsedExpr, syn::Token![,]>,
    pub semi: syn::Token![;],
}

impl Parse for ParsedStmtWrite {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            inst: input.parse()?,
            args: Punctuated::parse_separated_nonempty(input)?,
            semi: input.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ParsedStmtRead {
    pub inst: kw::read,
    pub args: Punctuated<ParsedDecl, syn::Token![,]>,
    pub semi: syn::Token![;],
}

impl Parse for ParsedStmtRead {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            inst: input.parse()?,
            args: Punctuated::parse_separated_nonempty(input)?,
            semi: input.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ParsedStmtCall {
    pub inst: kw::call,
    pub name: ParsedIdent,
    pub args_paren: syn::token::Paren,
    pub args: Punctuated<ParsedExpr, syn::Token![,]>,
    pub return_value: Option<(syn::Token![->], ParsedDecl)>,
    pub semi: syn::Token![;],
}

impl Parse for ParsedStmtCall {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let args_input;
        Ok(Self {
            inst: input.parse()?,
            name: input.parse()?,
            args_paren: syn::parenthesized!(args_input in input),
            args: Punctuated::parse_terminated(&args_input)?,
            return_value: if input.peek(syn::Token![->]) {
                Some((input.parse()?, input.parse()?))
            } else {
                None
            },
            semi: input.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ParsedStmtFor {
    pub for_token: syn::Token![for],
    pub index_name: ParsedIdent,
    pub upto: kw::upto,
    pub range: ParsedExpr,
    pub body_brace: syn::token::Brace,
    pub body: ParsedBlock,
}

impl Parse for ParsedStmtFor {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let body_input;
        Ok(Self {
            for_token: input.parse()?,
            index_name: input.parse()?,
            upto: input.parse()?,
            range: input.parse()?,
            body_brace: syn::braced!(body_input in input),
            body: body_input.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ParsedBlock {
    pub stmts: Vec<ParsedStmt>,
}

impl Parse for ParsedBlock {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let mut stmts = vec![];
        while !input.is_empty() {
            stmts.push(input.parse()?);
        }
        Ok(ParsedBlock { stmts })
    }
}

#[derive(Debug, Clone)]
pub struct ParsedSpec {
    pub main: ParsedBlock,
}

impl Parse for ParsedSpec {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(ParsedSpec {
            main: input.parse()?,
        })
    }
}
