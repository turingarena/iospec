use std::sync::Arc;

use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::*;
use codemap::File;
use proc_macro2::{LineColumn, Span};

use crate::hir::*;
use genco::prelude::*;

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
    ParseError { error: syn::parse::Error },
    InvalidAtomTy { ident: Rc<HIdent> },
    UndefVar { ident: Rc<HIdent> },
    SubscriptIndexNotScalar { array: Rc<HVal>, index: Rc<HVal> },
    SubscriptArrayNotArray { array: Rc<HVal>, index: Rc<HVal> },
    SubscriptIndexWrongType { array: Rc<HVal>, index: Rc<HVal> },
}

impl Sess {
    fn pos(self: &Self, lc: LineColumn) -> usize {
        let line_start = self.file.line_span(lc.line - 1).low() - self.file.span.low();
        line_start as usize + lc.column
    }

    fn range(self: &Self, span: &Span) -> (usize, usize) {
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

impl FormatInto<()> for HAtomTy {
    fn format_into(self, tokens: &mut Tokens) {
        let ident = self.ident.token.to_string();
        quote_in!(*tokens => #ident)
    }
}

impl FormatInto<()> for HValTy {
    fn format_into(self, tokens: &mut Tokens) {
        match self {
            HValTy::Atom { atom_ty } => quote_in!(*tokens => #*atom_ty),
            HValTy::Array { item, range } => quote_in!(*tokens => #*item[]),
            HValTy::Err => {}
        }
    }
}

fn quote_string(tokens: Tokens) -> String {
    tokens.to_file_string().unwrap()
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
                    range: sess.range(&error.span()),
                    annotation_type: AnnotationType::Error,
                    label: "here",
                }],
            ),
            Diagnostic::InvalidAtomTy { ident } => sess.diagnostic_message(
                AnnotationType::Error,
                &format!("invalid scalar type `{}`", ident.token,),
                vec![], // TODO
            ),
            Diagnostic::UndefVar { ident } => sess.diagnostic_message(
                AnnotationType::Error,
                &format!(
                    "no variable named `{}` found in the current scope",
                    ident.token
                ),
                vec![SourceAnnotation {
                    range: sess.range(&ident.token.span()),
                    annotation_type: AnnotationType::Error,
                    label: "not found in this scope",
                }],
            ),
            Diagnostic::SubscriptArrayNotArray { array, index } => sess.diagnostic_message(
                AnnotationType::Error,
                &format!(
                    "cannot index into a value of non-array type `{}`",
                    quote_string(quote!(array.ty)),
                ),
                vec![], // TODO
            ),
            Diagnostic::SubscriptIndexNotScalar { array, index } => todo!(),
            Diagnostic::SubscriptIndexWrongType { array, index } => todo!(),
        }
    }
}
