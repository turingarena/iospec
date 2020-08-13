extern crate proc_macro2;
extern crate syn;

use syn::Error;
use syn::parse::Parse;
use syn::parse::ParseBuffer;
use syn::punctuated::Punctuated;

pub use decl::*;
pub use expr::*;
pub use ident::*;
pub use stmt::*;
pub use types::*;

mod ident;
mod types;
mod expr;
mod decl;
mod stmt;

mod kw {
    syn::custom_keyword!(read);
    syn::custom_keyword!(write);
    syn::custom_keyword!(call);
    syn::custom_keyword!(upto);
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
