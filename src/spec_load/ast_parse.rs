//! Parse an AST from a `syn::parse::ParseBuffer`.

use syn::parse::Parse;
use syn::parse::ParseBuffer;
use syn::punctuated::Punctuated;
use syn::Error;

use crate::spec::kw;
use crate::spec::rel::RelOp;

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
        } else if lookahead.peek(kw::assume) {
            Ok(AStmt::Assume {
                kw: input.parse()?,
                cond: input.parse()?,
                semi: input.parse()?,
            })
        } else {
            Err(lookahead.error())
        }
    }
}

impl AExpr {
    fn parse_atomic(input: &ParseBuffer) -> Result<Self, Error> {
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

    fn parse_mul(input: &ParseBuffer) -> Result<Self, Error> {
        let first: AExpr = Self::parse_atomic(input)?;
        Ok(if input.peek(syn::Token![*]) {
            let mut chain = syn::punctuated::Punctuated::<AExpr, syn::Token![*]>::new();
            chain.push_value(first);
            while input.peek(syn::Token![*]) {
                chain.push_punct(input.parse()?);
                chain.push_value(Self::parse_atomic(input)?);
            }
            AExpr::Mul { factors: chain }
        } else {
            first
        })
    }

    fn parse_sum(input: &ParseBuffer) -> Result<Self, Error> {
        let first_sign: Option<ASign> = if Self::peek_sign(input) {
            Some(input.parse()?)
        } else {
            None
        };

        let first: AExpr = Self::parse_mul(input)?;

        Ok(if first_sign.is_some() || Self::peek_sign(input) {
            let mut chain = syn::punctuated::Punctuated::<AExpr, ASign>::new();
            chain.push_value(first);
            while Self::peek_sign(input) {
                chain.push_punct(input.parse()?);
                chain.push_value(Self::parse_mul(input)?);
            }
            AExpr::Sum {
                first_sign,
                terms: chain,
            }
        } else {
            first
        })
    }

    fn parse_rel(input: &ParseBuffer) -> Result<Self, Error> {
        let first: AExpr = Self::parse_sum(input)?;

        Ok(if Self::peek_rel_op(input) {
            let mut chain = syn::punctuated::Punctuated::<AExpr, RelOp>::new();
            chain.push_value(first);
            while Self::peek_rel_op(input) {
                chain.push_punct(input.parse()?);
                chain.push_value(Self::parse_sum(input)?);
            }
            AExpr::RelChain { chain }
        } else {
            first
        })
    }

    fn peek_sign(input: &ParseBuffer) -> bool {
        input.peek(syn::Token![+]) || input.peek(syn::Token![-])
    }

    fn peek_rel_op(input: &ParseBuffer) -> bool {
        [
            input.peek(syn::Token![==]),
            input.peek(syn::Token![!=]),
            input.peek(syn::Token![<=]),
            input.peek(syn::Token![>=]),
            input.peek(syn::Token![<]),
            input.peek(syn::Token![>]),
        ]
        .iter()
        .any(|b| *b)
    }
}

impl Parse for ASign {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let la = input.lookahead1();
        Ok(if la.peek(syn::Token![+]) {
            ASign::Plus(input.parse()?)
        } else if la.peek(syn::Token![-]) {
            ASign::Minus(input.parse()?)
        } else {
            Err(la.error())?
        })
    }
}

impl Parse for RelOp {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let la = input.lookahead1();
        Ok(if la.peek(syn::Token![==]) {
            RelOp::Eq(input.parse()?)
        } else if la.peek(syn::Token![!=]) {
            RelOp::Ne(input.parse()?)
        } else if la.peek(syn::Token![<=]) {
            RelOp::Le(input.parse()?)
        } else if la.peek(syn::Token![>=]) {
            RelOp::Ge(input.parse()?)
        } else if la.peek(syn::Token![<]) {
            RelOp::Lt(input.parse()?)
        } else if la.peek(syn::Token![>]) {
            RelOp::Gt(input.parse()?)
        } else {
            Err(la.error())?
        })
    }
}

impl Parse for AExpr {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Self::parse_rel(input)
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
