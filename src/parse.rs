extern crate proc_macro2;
extern crate syn;

use syn::parse::Parse;
use syn::parse::ParseBuffer;
use syn::punctuated::Punctuated;
use syn::Error;

#[derive(Debug, Clone)]
pub struct ParsedIdent {
    pub sym: String,
}

impl Parse for ParsedIdent {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        // Parsing TokenTree instead of Indent to ignore Rust keywords
        let token_tree: proc_macro2::TokenTree = input.parse()?;
        match token_tree {
            proc_macro2::TokenTree::Ident(x) => Ok(ParsedIdent { sym: x.to_string() }),
            _ => Err(Error::new(token_tree.span(), "expected identifier")),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedScalarTypeExpr {
    pub ident: ParsedIdent,
}

impl Parse for ParsedScalarTypeExpr {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            ident: input.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum ParsedExpr {
    Var(ParsedExprVar),
    Index(ParsedExprIndex),
}

impl Parse for ParsedExpr {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let mut current = ParsedExpr::Var(input.parse()?);
        while input.peek(syn::token::Bracket) {
            let index_input;
            current = ParsedExpr::Index(ParsedExprIndex {
                array: Box::new(current),
                bracket: syn::bracketed!(index_input in input),
                index: index_input.parse()?,
            });
        }
        Ok(current)
    }
}

#[derive(Debug, Clone)]
pub struct ParsedExprVar {
    pub ident: ParsedIdent,
}

impl Parse for ParsedExprVar {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            ident: input.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ParsedExprIndex {
    pub array: Box<ParsedExpr>,
    pub bracket: syn::token::Bracket,
    pub index: Box<ParsedExpr>,
}

impl Parse for ParsedExprIndex {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let index_input;
        Ok(Self {
            array: input.parse()?,
            bracket: syn::bracketed!(index_input in input),
            index: index_input.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ParsedDecl {
    pub expr: ParsedExpr,
    pub colon: syn::Token![:],
    pub ty: ParsedScalarTypeExpr,
}

impl Parse for ParsedDecl {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            expr: input.parse()?,
            colon: input.parse()?,
            ty: input.parse()?,
        })
    }
}

mod kw {
    syn::custom_keyword!(read);
    syn::custom_keyword!(write);
    syn::custom_keyword!(call);
}

#[derive(Debug, Clone)]
pub enum ParsedStmt {
    Write(ParsedStmtWrite),
    Read(ParsedStmtRead),
    Call(ParsedStmtCall),
}

impl Parse for ParsedStmt {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        if input.peek(kw::read) {
            Ok(ParsedStmt::Read(input.parse()?))
        } else if input.peek(kw::write) {
            Ok(ParsedStmt::Write(input.parse()?))
        } else if input.peek(kw::call) {
            Ok(ParsedStmt::Call(input.parse()?))
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
pub enum ParsedBlock {
    Empty(ParsedBlockEmpty),
    Cons(ParsedBlockCons),
}

#[derive(Debug, Clone)]
pub struct ParsedBlockCons {
    pub prev: Box<ParsedBlock>,
    pub stmt: ParsedStmt,
}

#[derive(Debug, Clone)]
pub struct ParsedBlockEmpty;

impl Parse for ParsedBlock {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let mut current: ParsedBlock = ParsedBlock::Empty(ParsedBlockEmpty);
        while !input.is_empty() {
            current = ParsedBlock::Cons(ParsedBlockCons {
                prev: Box::new(current),
                stmt: input.parse()?,
            });
        }
        Ok(current)
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
