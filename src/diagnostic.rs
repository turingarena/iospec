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
    },
    AtomNotScalar {
        val: Rc<HVal>,
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

    fn range(self: &Self, span: Span) -> (usize, usize) {
        (self.pos(span.start()), self.pos(span.end()))
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
                vec![SourceAnnotation {
                    range: sess.range(error.span()),
                    annotation_type: AnnotationType::Error,
                    label: "here",
                }],
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
                vec![SourceAnnotation {
                    range: sess.range(ident.token.span()),
                    annotation_type: AnnotationType::Error,
                    label: "not found in this scope",
                }],
            ),
            Diagnostic::RangeBoundNotNatural { val } => sess.diagnostic_message(
                AnnotationType::Error,
                &format!(
                    "for cycle upper bound must be a natural, got `{}`",
                    quote_string(quote!(#(val.ty.deref()))),
                ),
                vec![SourceAnnotation {
                    range: sess.range(val.span()),
                    annotation_type: AnnotationType::Error,
                    label: "must be a natural",
                }],
            ),
            Diagnostic::AtomNotScalar { val } => sess.diagnostic_message(
                AnnotationType::Error,
                &format!(
                    "input/output data must be scalars, got `{}`",
                    quote_string(quote!(#(val.ty.deref()))),
                ),
                vec![SourceAnnotation {
                    range: sess.range(val.span()),
                    annotation_type: AnnotationType::Error,
                    label: "must be a scalar",
                }],
            ),
            Diagnostic::SubscriptArrayNotArray { array, index, .. } => sess.diagnostic_message(
                AnnotationType::Error,
                &format!(
                    "cannot index into a value of non-array type `{}`",
                    quote_string(quote!(#(array.ty.deref()))),
                ),
                vec![SourceAnnotation {
                    range: sess.range(array.span()),
                    annotation_type: AnnotationType::Error,
                    label: "must be an array",
                }],
            ),
            Diagnostic::SubscriptIndexNotScalar { array, index, .. } => todo!(),
            Diagnostic::SubscriptIndexWrongType { array, index, .. } => todo!(),
        }
    }
}
