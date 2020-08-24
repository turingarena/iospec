//! Parse an AST from a `syn::parse::ParseBuffer`.

use syn::parse::Parse;
use syn::parse::ParseBuffer;
use syn::punctuated::Punctuated;
use syn::Error;

use crate::spec::kw;

use super::ast::*;
use super::diagnostic::*;

pub fn parse_spec(source: &str, dgns: &mut Vec<Diagnostic>) -> Result<ASpec, ()> {
    match syn::parse_str(source) {
        Ok(spec) => Ok(spec),
        Err(error) => {
            dgns.push(Diagnostic::ParseError { error });
            Err(())
        }
    }
}

impl Parse for ASpec {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(ASpec {
            main: input.parse()?,
        })
    }
}

impl Parse for ABlock {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let mut stmts = vec![];
        while !input.is_empty() {
            stmts.push(input.parse()?);
        }
        Ok(ABlock { stmts })
    }
}

impl Parse for AStmt {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::read) {
            Ok(AStmt::Read {
                kw: input.parse()?,
                args: Punctuated::parse_separated_nonempty(input)?,
                semi: input.parse()?,
            })
        } else if lookahead.peek(kw::write) {
            Ok(AStmt::Write {
                kw: input.parse()?,
                args: Punctuated::parse_separated_nonempty(input)?,
                semi: input.parse()?,
            })
        } else if lookahead.peek(kw::call) {
            let args_input;

            Ok(AStmt::Call {
                kw: input.parse()?,
                name: input.parse()?,
                args_paren: syn::parenthesized!(args_input in input),
                args: Punctuated::parse_terminated(&args_input)?,
                ret: if input.peek(syn::Token![->]) {
                    Some((input.parse()?, input.parse()?))
                } else {
                    None
                },
                semi: input.parse()?,
            })
        } else if lookahead.peek(syn::Token![for]) {
            let body_input;
            Ok(AStmt::For {
                kw: input.parse()?,
                index: input.parse()?,
                upto: input.parse()?,
                bound: input.parse()?,
                body_brace: syn::braced!(body_input in input),
                body: body_input.parse()?,
            })
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for AExpr {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let mut current = if input.peek(syn::token::Paren) {
            let inner_input;

            AExpr::Paren {
                paren: syn::parenthesized!(inner_input in input),
                inner: Box::new(inner_input.parse()?),
            }
        } else if input.peek(syn::Lit) {
            AExpr::IntLit {
                token: input.parse()?,
            }
        } else {
            AExpr::Ref {
                ident: input.parse()?,
            }
        };

        while input.peek(syn::token::Bracket) {
            let index_input;
            current = AExpr::Subscript {
                array: Box::new(current),
                bracket: syn::bracketed!(index_input in input),
                index: index_input.parse()?,
            };
        }
        Ok(current)
    }
}

impl Parse for ADef {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            expr: input.parse()?,
            colon: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl Parse for ATy {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            ident: input.parse()?,
        })
    }
}

impl Parse for AIdent {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        // Parsing TokenTree instead of Indent to ignore Rust keywords
        let token_tree: proc_macro2::TokenTree = input.parse()?;
        match token_tree {
            proc_macro2::TokenTree::Ident(token) => Ok(AIdent { token }),
            _ => Err(Error::new(token_tree.span(), "expected identifier")),
        }
    }
}
