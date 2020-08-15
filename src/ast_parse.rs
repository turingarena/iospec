use syn::Error;
use syn::parse::Parse;
use syn::parse::ParseBuffer;
use syn::punctuated::Punctuated;

use crate::kw::*;
use crate::ast::*;

impl Parse for ParsedBlock {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let mut stmts = vec![];
        while !input.is_empty() {
            stmts.push(input.parse()?);
        }
        Ok(ParsedBlock { stmts })
    }
}

impl Parse for ParsedDef {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            expr: input.parse()?,
            colon: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl Parse for ParsedExpr {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let mut current = ParsedExpr::Ref {
            ident: input.parse()?,
        };

        while input.peek(syn::token::Bracket) {
            let index_input;
            current = ParsedExpr::Subscript {
                array: Box::new(current),
                bracket: syn::bracketed!(index_input in input),
                index: index_input.parse()?,
            };
        }
        Ok(current)
    }
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

impl Parse for ParsedSpec {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(ParsedSpec {
            main: input.parse()?,
        })
    }
}

impl Parse for ParsedStmt {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        if input.peek(kw::read) {
            Ok(ParsedStmt::Read {
                inst: input.parse()?,
                args: Punctuated::parse_separated_nonempty(input)?,
                semi: input.parse()?,
            })
        } else if input.peek(kw::write) {
            Ok(ParsedStmt::Write {
                inst: input.parse()?,
                args: Punctuated::parse_separated_nonempty(input)?,
                semi: input.parse()?,
            })
        } else if input.peek(kw::call) {
            let args_input;

            Ok(ParsedStmt::Call {
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
        } else if input.peek(syn::Token![for]) {
            let body_input;
            Ok(ParsedStmt::For {
                for_token: input.parse()?,
                index_name: input.parse()?,
                upto: input.parse()?,
                bound: input.parse()?,
                body_brace: syn::braced!(body_input in input),
                body: body_input.parse()?,
            })
        } else {
            Err(Error::new(input.span(), "statement expected"))
        }
    }
}

impl Parse for ParsedScalarTypeExpr {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            ident: input.parse()?,
        })
    }
}
