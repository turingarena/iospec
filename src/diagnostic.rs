use std::ops::Deref;

use genco::prelude::*;

use crate::hir::*;
use crate::hir_span::*;
use crate::sess::Sess;

#[derive(Debug, Clone)]
pub enum Diagnostic {
    ParseError {
        error: syn::parse::Error,
    },
    InvalidAtomTy {
        ident: Rc<HName>,
    },
    UndefVar {
        ident: Rc<HName>,
    },
    AlreadyDefinedVar {
        old_var: Rc<HVar>,
        new_var: Rc<HVar>,
    },
    RangeBoundNotNatural {
        val: Rc<HVal>,
        atom_ty: Option<Rc<HAtomTy>>,
    },
    AtomNotScalar {
        val: Rc<HVal>,
    },
    SubscriptDefIndexNotMatched {
        bracket: syn::token::Bracket,
        range: Option<Rc<HRange>>,
        name: Option<Rc<HName>>,
    },
    SubscriptArrayNotArray {
        array: Rc<HVal>,
        index: Rc<HVal>,
        bracket: syn::token::Bracket,
    },
    SubscriptIndexWrongType {
        range: Rc<HRange>,
        array: Rc<HVal>,
        index: Rc<HVal>,
        bracket: syn::token::Bracket,
    },
    ArgumentNotVariable {
        val: Rc<HVal>,
    },
}

impl FormatInto<()> for &HAtomTy {
    fn format_into(self: Self, tokens: &mut Tokens) {
        match self.expr.deref() {
            HAtomTyExpr::Name { name, .. } => quote_in!(*tokens => #(name.deref())),
            HAtomTyExpr::Err => quote_in!(*tokens => <<invalid scalar type>>),
        }
    }
}

impl FormatInto<()> for &HValTy {
    fn format_into(self: Self, tokens: &mut Tokens) {
        match self {
            HValTy::Atom { atom_ty } => quote_in!(*tokens => #(atom_ty.deref())),
            HValTy::Array { item, .. } => quote_in!(*tokens => #(item.deref())[]),
            HValTy::Err => {}
        }
    }
}

impl FormatInto<()> for &HName {
    fn format_into(self: Self, tokens: &mut Tokens) {
        let ident = self.ident.to_string();
        quote_in!(*tokens => #ident)
    }
}

fn quote_string(tokens: Tokens) -> String {
    tokens.to_string().unwrap()
}

impl Diagnostic {
    pub fn is_critical(self: &Self) -> bool {
        true
    }

    pub fn diagnostic_message(self: &Self, sess: &Sess) -> String {
        match self {
            Diagnostic::ParseError { error } => sess.error(
                &error.to_string(),
                vec![sess.error_ann("here", error.span())],
            ),
            Diagnostic::InvalidAtomTy { ident } => sess.error(
                &format!("invalid scalar type `{}`", ident.to_string()),
                vec![], // TODO
            ),
            Diagnostic::UndefVar { ident } => sess.error(
                &format!(
                    "no variable named `{}` found in the current scope",
                    ident.to_string()
                ),
                vec![sess.error_ann("not found in this scope", ident.ident.span())],
            ),
            Diagnostic::AlreadyDefinedVar { old_var, new_var } => sess.error(
                &format!("variable `{}` already defined", new_var.name.to_string()),
                vec![
                    sess.error_ann("cannot re-define a variable in scope", new_var.name.span()),
                    sess.help_ann("was defined here", old_var.name.span()),
                ],
            ),
            Diagnostic::RangeBoundNotNatural { val, atom_ty } => sess.error(
                &format!(
                    "for cycle upper bound must be a natural, got `{}`",
                    quote_string(quote!(#(val.ty.deref()))),
                ),
                {
                    let mut anns = vec![sess.error_ann("must be a natural", val.span())];
                    if let Some(atom_ty) = atom_ty {
                        if let HAtomTyExpr::Name { name } = atom_ty.expr.deref() {
                            anns.push(sess.help_ann("type defined here", name.span()))
                        }
                    }
                    anns
                },
            ),
            Diagnostic::AtomNotScalar { val } => sess.error(
                &format!(
                    "input/output data must be scalars, got `{}`",
                    quote_string(quote!(#(val.ty.deref()))),
                ),
                vec![sess.error_ann("must be a scalar", val.span())],
            ),
            Diagnostic::SubscriptArrayNotArray { array, .. } => sess.error(
                &format!(
                    "cannot index into a value of non-array type `{}`",
                    quote_string(quote!(#(array.ty.deref()))),
                ),
                vec![sess.error_ann("must be an array", array.span())],
            ),
            Diagnostic::SubscriptIndexWrongType { range, index, .. } => sess.error(
                &format!(
                    "index must be `{}`, got `{}`",
                    quote_string(quote!(#(range.bound.ty.deref()))),
                    quote_string(quote!(#(index.ty.deref()))),
                ),
                vec![
                    sess.error_ann("invalid index type", index.span()),
                    sess.help_ann("array range defined here", range.span()),
                ],
            ),
            Diagnostic::SubscriptDefIndexNotMatched {
                bracket,
                range,
                name,
            } => sess.error(
                &match range {
                    Some(range) => match name {
                        Some(name) => format!(
                            "index must match enclosing for, expecting `{}`, got `{}`",
                            quote_string(quote!(#(range.index.deref()))),
                            quote_string(quote!(#(name.deref()))),
                        ),
                        None => format!(
                            "index must match enclosing for, expecting `{}`, got an expression",
                            quote_string(quote!(#(range.index.deref()))),
                        ),
                    },
                    None => format!("index must match an enclosing for, but no for was found"),
                },
                match range {
                    Some(range) => vec![
                        match name {
                            Some(name) => {
                                sess.error_ann("does not match enclosing for index", name.span())
                            }
                            None => {
                                sess.error_ann("complex expressions not allowed here", bracket.span)
                            }
                        },
                        sess.help_ann("must match this index", range.index.span()),
                    ],
                    None => {
                        vec![sess.error_ann("subscript without an enclosing `for`", bracket.span)]
                    }
                },
            ),
            Diagnostic::ArgumentNotVariable { val } => sess.error(
                &format!("function call arguments must be variables, got an expression",),
                vec![sess.error_ann("must be a variable, not an expression", val.span())],
            ),
        }
    }
}
