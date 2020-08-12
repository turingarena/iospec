extern crate proc_macro2;
extern crate syn;

use syn::parse::Parse;
use syn::parse::ParseBuffer;
use syn::punctuated::Punctuated;
use syn::Error;

#[derive(Debug)]
pub struct Ident {
    pub sym: String,
}

impl Parse for Ident {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        // Parsing TokenTree instead of Indent to ignore Rust keywords
        let token_tree: proc_macro2::TokenTree = input.parse()?;
        match token_tree {
            proc_macro2::TokenTree::Ident(x) => Ok(Ident { sym: x.to_string() }),
            _ => Err(Error::new(token_tree.span(), "expected identifier")),
        }
    }
}

#[derive(Debug)]
pub struct ScalarType {
    pub ident: Ident,
}

impl Parse for ScalarType {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            ident: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub enum Expr {
    Var(ExprVar),
    Index(ExprIndex),
}

impl Parse for Expr {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let mut current = Expr::Var(input.parse()?);
        while input.peek(syn::token::Bracket) {
            let index_input;
            current = Expr::Index(ExprIndex {
                array: Box::new(current),
                bracket: syn::bracketed!(index_input in input),
                index: index_input.parse()?,
            });
        }
        Ok(current)
    }
}

#[derive(Debug)]
pub struct ExprVar {
    pub ident: Ident,
}

impl Parse for ExprVar {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            ident: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct ExprIndex {
    pub array: Box<Expr>,
    pub bracket: syn::token::Bracket,
    pub index: Box<Expr>,
}

impl Parse for ExprIndex {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let index_input;
        Ok(Self {
            array: input.parse()?,
            bracket: syn::bracketed!(index_input in input),
            index: index_input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct Decl {
    pub expr: Expr,
    pub colon: syn::Token![:],
    pub ty: ScalarType,
}

impl Parse for Decl {
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

#[derive(Debug)]
pub enum Stmt {
    Write(StmtWrite),
    Read(StmtRead),
    Call(StmtCall),
}

impl Parse for Stmt {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        if input.peek(kw::read) {
            Ok(Stmt::Read(input.parse()?))
        } else if input.peek(kw::write) {
            Ok(Stmt::Write(input.parse()?))
        } else if input.peek(kw::call) {
            Ok(Stmt::Call(input.parse()?))
        } else {
            Err(Error::new(input.span(), "statement expected"))
        }
    }
}

#[derive(Debug)]
pub struct StmtWrite {
    pub inst: kw::write,
    pub args: Punctuated<Expr, syn::Token![,]>,
    pub semi: syn::Token![;],
}

impl Parse for StmtWrite {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            inst: input.parse()?,
            args: Punctuated::parse_separated_nonempty(input)?,
            semi: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct StmtRead {
    pub inst: kw::read,
    pub args: Punctuated<Decl, syn::Token![,]>,
    pub semi: syn::Token![;],
}

impl Parse for StmtRead {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            inst: input.parse()?,
            args: Punctuated::parse_separated_nonempty(input)?,
            semi: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct StmtCall {
    pub inst: kw::call,
    pub name: Ident,
    pub args_paren: syn::token::Paren,
    pub args: Punctuated<Expr, syn::Token![,]>,
    pub return_value: Option<(syn::Token![->], Decl)>,
    pub semi: syn::Token![;],
}

impl Parse for StmtCall {
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

#[derive(Debug)]
pub enum Block {
    Empty(BlockEmpty),
    Cons(BlockCons),
}

#[derive(Debug)]
pub struct BlockCons {
    pub prev: Box<Block>,
    pub stmt: Stmt,
}

#[derive(Debug)]
pub struct BlockEmpty;

impl Parse for Block {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let mut current: Block = Block::Empty(BlockEmpty);
        while !input.is_empty() {
            current = Block::Cons(BlockCons {
                prev: Box::new(current),
                stmt: input.parse()?,
            });
        }
        Ok(current)
    }
}

#[derive(Debug)]
pub struct Spec {
    pub main: Block,
}

impl Parse for Spec {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Spec {
            main: input.parse()?,
        })
    }
}
