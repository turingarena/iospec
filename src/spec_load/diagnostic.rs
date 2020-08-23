use crate::atom::AtomTy;
use crate::spec::hir::*;
use crate::spec::hir_quote::quote_hir;
use crate::spec::hir_span::*;
use crate::spec::sess::*;

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
    DefInvalidExpression {
        span: proc_macro2::Span,
    },
    SubscriptDefIndexNotMatched {
        bracket: syn::token::Bracket,
        expected_range: Option<Rc<HRange>>,
        actual_range: Option<Rc<HRange>>,
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
    InvalidLiteral {
        token: syn::LitInt,
        ty: Option<AtomTy>,
        value_i64: Result<i64, std::num::ParseIntError>,
    },
}

impl Diagnostic {
    pub fn is_critical(self: &Self) -> bool {
        true
    }

    pub fn diagnostic_message(self: &Self, sess: &Sess) -> String {
        match self {
            Diagnostic::ParseError { error } => sess.error_snippet(
                &error.to_string(),
                vec![sess.error_ann("here", error.span())],
                vec![],
            ),
            Diagnostic::InvalidAtomTy { ident } => sess.error_snippet(
                &format!("invalid scalar type `{}`", ident.to_string()),
                vec![], // TODO
                vec![],
            ),
            Diagnostic::UndefVar { ident } => sess.error_snippet(
                &format!(
                    "no variable named `{}` found in the current scope",
                    ident.to_string()
                ),
                vec![sess.error_ann("not found in this scope", ident.ident.span())],
                vec![],
            ),
            Diagnostic::AlreadyDefinedVar { old_var, new_var } => sess.error_snippet(
                &format!("variable `{}` already defined", new_var.name.to_string()),
                vec![
                    sess.error_ann("cannot re-define a variable in scope", new_var.name.span()),
                    sess.info_ann("was defined here", old_var.name.span()),
                ],
                vec![],
            ),
            Diagnostic::RangeBoundNotNatural { val, atom_ty } => sess.error_snippet(
                &format!(
                    "for cycle upper bound must be a natural, got `{}`",
                    quote_hir(val.ty.as_ref()),
                ),
                {
                    let mut anns = vec![sess.error_ann("must be a natural", val.span())];
                    if let Some(atom_ty) = atom_ty {
                        if let HAtomTyExpr::Name { name } = &atom_ty.expr {
                            anns.push(sess.info_ann("type defined here", name.span()))
                        }
                    }
                    anns
                },
                vec![],
            ),
            Diagnostic::AtomNotScalar { val } => sess.error_snippet(
                &format!(
                    "input/output data must be scalars, got `{}`",
                    quote_hir(val.ty.as_ref()),
                ),
                vec![sess.error_ann("must be a scalar", val.span())],
                vec![],
            ),
            Diagnostic::SubscriptArrayNotArray { array, .. } => sess.error_snippet(
                &format!(
                    "cannot index into a value of non-array type `{}`",
                    quote_hir(array.ty.as_ref()),
                ),
                vec![sess.error_ann("must be an array", array.span())],
                vec![],
            ),
            Diagnostic::SubscriptIndexWrongType { range, index, .. } => sess.error_snippet(
                &format!(
                    "expected index of type `{}`, got `{}`",
                    quote_hir(range.bound.ty.as_ref()),
                    quote_hir(index.ty.as_ref()),
                ),
                vec![
                    sess.error_ann(
                        &format!("invalid index type `{}`", quote_hir(index.ty.as_ref())),
                        index.span(),
                    ),
                    sess.info_ann("array range", range.span()),
                    sess.info_ann("expected type", range.bound.ty.span()),
                ]
                .into_iter()
                .chain(match index.ty.as_ref() {
                    HValTy::Atom { atom_ty } => {
                        Some(sess.info_ann("got this type", atom_ty.span()))
                    }
                    _ => None,
                })
                .collect(),
                vec![],
            ),
            Diagnostic::SubscriptDefIndexNotMatched {
                bracket,
                expected_range,
                actual_range: _,
                name,
            } => sess.error_snippet(
                &match expected_range {
                    Some(expected_range) => match name {
                        Some(name) => format!(
                            "index must match enclosing for, expecting `{}`, got `{}`",
                            quote_hir(expected_range.index.as_ref()),
                            quote_hir(name.as_ref()),
                        ),
                        None => format!(
                            "index must match enclosing for, expecting `{}`, got an expression",
                            quote_hir(expected_range.index.as_ref()),
                        ),
                    },
                    None => format!("index must match an enclosing for, but no for was found"),
                },
                match expected_range {
                    Some(expected_range) => vec![
                        match name {
                            Some(name) => {
                                sess.error_ann("does not match enclosing for index", name.span())
                            }
                            None => {
                                sess.error_ann("complex expressions not allowed here", bracket.span)
                            }
                        },
                        sess.info_ann("must match this index", expected_range.index.span()),
                    ],
                    None => {
                        vec![sess.error_ann("subscript without an enclosing `for`", bracket.span)]
                    }
                },
                vec![],
            ),
            Diagnostic::ArgumentNotVariable { val } => sess.error_snippet(
                &format!("function call arguments must be variables, got an expression",),
                vec![sess.error_ann("must be a variable, not an expression", val.span())],
                vec![],
            ),
            Diagnostic::InvalidLiteral {
                token,
                ty,
                value_i64,
            } => sess.error_snippet(
                &format!("invalid literal",),
                std::iter::empty()
                    .chain(if ty.is_none() {
                        Some(if token.suffix().is_empty() {
                            sess.error_ann(
                                "must specify the type as suffix (e.g., `10n32`)",
                                token.span(),
                            )
                        } else {
                            sess.error_ann("invalid type as suffix", token.span())
                        })
                    } else {
                        None
                    })
                    .chain(match value_i64 {
                        // TODO: show reason
                        Err(_) => Some(sess.error_ann("cannot parse", token.span())),
                        Ok(_) => None,
                    })
                    .chain(if value_i64.is_ok() && ty.is_some() {
                        Some(sess.error_ann("value outside range", token.span()))
                    } else {
                        None
                    })
                    .collect(),
                vec![],
            ),
            Diagnostic::DefInvalidExpression { span } => sess.error_snippet(
                "invalid expression in definition",
                vec![sess.error_ann("invalid expression", *span)],
                vec![sess.footer_note("only variables and subscripts are allowed")],
            ),
        }
    }
}
