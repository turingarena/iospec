use std::ops::Deref;
use std::sync::Arc;

use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::*;
use codemap::File;
use genco::prelude::*;
use proc_macro2::{LineColumn, Span};

use crate::hir::*;
use crate::hir_span::*;

/// A compilation session, collecting diagnostics.
/// To be borrowed as mutable during compilation.
pub struct Sess {
    pub file: Arc<File>,
    pub diagnostics: Vec<Diagnostic>,
}

impl Sess {
    pub fn new(file: &Arc<File>) -> Sess {
        Sess {
            diagnostics: Vec::new(),
            file: file.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Diagnostic {
    ParseError {
        error: syn::parse::Error,
    },
    InvalidAtomTy {
        ident: Rc<HIdent>,
    },
    UndefVar {
        ident: Rc<HIdent>,
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
        name: Option<Rc<HIdent>>,
    },
    SubscriptIndexNotScalar {
        array: Rc<HVal>,
        index: Rc<HVal>,
        bracket: syn::token::Bracket,
    },
    SubscriptArrayNotArray {
        array: Rc<HVal>,
        index: Rc<HVal>,
        bracket: syn::token::Bracket,
    },
    SubscriptIndexWrongType {
        array: Rc<HVal>,
        index: Rc<HVal>,
        bracket: syn::token::Bracket,
    },
}

impl Sess {
    fn pos(self: &Self, lc: LineColumn) -> usize {
        let line_start = self.file.line_span(lc.line - 1).low() - self.file.span.low();
        line_start as usize + lc.column
    }

    fn annotation<'a>(
        self: &'a Self,
        annotation_type: AnnotationType,
        label: &'a str,
        span: Span,
    ) -> SourceAnnotation {
        SourceAnnotation {
            annotation_type,
            label,
            range: (self.pos(span.start()), self.pos(span.end())),
        }
    }

    fn diagnostic_message(
        self: &Self,
        annotation_type: AnnotationType,
        message: &str,
        annotations: Vec<SourceAnnotation>,
    ) -> String {
        let snippet = Snippet {
            title: Some(Annotation {
                id: None,
                label: Some(message),
                annotation_type,
            }),
            footer: vec![],
            slices: vec![Slice {
                source: self.file.source(),
                line_start: 1,
                origin: Some(self.file.name()),
                fold: false,
                annotations,
            }],
            opt: FormatOptions {
                color: true,
                ..Default::default()
            },
        };

        DisplayList::from(snippet).to_string()
    }
}

impl FormatInto<()> for &HAtomTy {
    fn format_into(self: Self, tokens: &mut Tokens) {
        match self.expr.deref() {
            HAtomTyExpr::Ident { ident, .. } => quote_in!(*tokens => #(ident.deref())),
            HAtomTyExpr::Err => quote_in!(*tokens => <<invalid scalar type>>),
        }
    }
}

impl FormatInto<()> for &HValTy {
    fn format_into(self: Self, tokens: &mut Tokens) {
        match self {
            HValTy::Atom { atom_ty } => quote_in!(*tokens => #(atom_ty.deref())),
            HValTy::Array { item, range } => quote_in!(*tokens => #(item.deref())[]),
            HValTy::Err => {}
        }
    }
}

impl FormatInto<()> for &HIdent {
    fn format_into(self: Self, tokens: &mut Tokens) {
        let ident = self.token.to_string();
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
            Diagnostic::ParseError { error } => sess.diagnostic_message(
                AnnotationType::Error,
                &error.to_string(),
                vec![sess.annotation(AnnotationType::Error, "here", error.span())],
            ),
            Diagnostic::InvalidAtomTy { ident } => sess.diagnostic_message(
                AnnotationType::Error,
                &format!("invalid scalar type `{}`", ident.to_string()),
                vec![], // TODO
            ),
            Diagnostic::UndefVar { ident } => sess.diagnostic_message(
                AnnotationType::Error,
                &format!(
                    "no variable named `{}` found in the current scope",
                    ident.to_string()
                ),
                vec![sess.annotation(
                    AnnotationType::Error,
                    "not found in this scope",
                    ident.token.span(),
                )],
            ),
            Diagnostic::RangeBoundNotNatural { val, atom_ty } => sess.diagnostic_message(
                AnnotationType::Error,
                &format!(
                    "for cycle upper bound must be a natural, got `{}`",
                    quote_string(quote!(#(val.ty.deref()))),
                ),
                {
                    let mut anns = vec![sess.annotation(
                        AnnotationType::Error,
                        "must be a natural",
                        val.span(),
                    )];
                    if let Some(atom_ty) = atom_ty {
                        if let HAtomTyExpr::Ident { ident } = atom_ty.expr.deref() {
                            anns.push(sess.annotation(
                                AnnotationType::Note,
                                "type defined here",
                                ident.span(),
                            ))
                        }
                    }
                    anns
                },
            ),
            Diagnostic::AtomNotScalar { val } => sess.diagnostic_message(
                AnnotationType::Error,
                &format!(
                    "input/output data must be scalars, got `{}`",
                    quote_string(quote!(#(val.ty.deref()))),
                ),
                vec![sess.annotation(AnnotationType::Error, "must be a scalar", val.span())],
            ),
            Diagnostic::SubscriptArrayNotArray { array, index, .. } => sess.diagnostic_message(
                AnnotationType::Error,
                &format!(
                    "cannot index into a value of non-array type `{}`",
                    quote_string(quote!(#(array.ty.deref()))),
                ),
                vec![sess.annotation(AnnotationType::Error, "must be an array", array.span())],
            ),
            Diagnostic::SubscriptIndexNotScalar { array, index, .. } => todo!(),
            Diagnostic::SubscriptIndexWrongType { array, index, .. } => todo!(),
            Diagnostic::SubscriptDefIndexNotMatched {
                bracket,
                range,
                name,
            } => sess.diagnostic_message(
                AnnotationType::Error,
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
                    None => format!("index must match an enclosing for, but for was found"),
                },
                match range {
                    Some(range) => vec![
                        match name {
                            Some(name) => sess.annotation(
                                AnnotationType::Error,
                                "does not match enclosing index `for` index",
                                name.span(),
                            ),
                            None => sess.annotation(
                                AnnotationType::Error,
                                "complex expressions not allowed here",
                                bracket.span,
                            ),
                        },
                        sess.annotation(
                            AnnotationType::Info,
                            "must match this index",
                            range.index.span(),
                        ),
                    ],
                    None => vec![sess.annotation(
                        AnnotationType::Error,
                        "subscript without an enclosing `for`",
                        bracket.span,
                    )],
                },
            ),
        }
    }
}
