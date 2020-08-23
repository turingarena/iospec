use crate::run::state::RState;
use crate::spec::hir::*;
use crate::spec::hir_quote::quote_hir;
use crate::spec::hir_span::HasSpan;
use crate::spec::sess::Sess;

use super::err::*;

impl AtomSourceError {
    pub fn message(self: &Self, ty: &HAtomTy) -> String {
        match self {
            AtomSourceError::Parse(e) => format!("{}", e),
            AtomSourceError::Value(AtomValueError { expected, actual }) => {
                format!("expected `{}`, got `{}`", expected, actual)
            }
            AtomSourceError::End => format!("expected a `{}`, got end-of-file", quote_hir(ty)),
            AtomSourceError::Type(AtomTypeError { ty, actual }) => format!(
                "expected a `{}`, got value outside range `{}`",
                ty.name(),
                actual
            ),
        }
    }
}

impl RError {
    pub fn diagnostic_message(self: &Self, state: &RState, sess: &Sess) -> String {
        let messages: Vec<_> = state
            .indexes
            .iter()
            .map(|(range, index_value)| {
                (
                    format!(
                        "at iteration `{}` = {}",
                        quote_hir(range.index.as_ref()),
                        index_value
                    ),
                    range.span(),
                )
            })
            .collect();

        let mut state_annotations = Vec::new();
        for (message, span) in messages.iter() {
            state_annotations.push(sess.info_ann(message, *span));
        }

        match self {
            RError::UnresolvedVal { val } => sess.error_snippet(
                "expression value is unknown",
                vec![sess.error_ann("has an unknown value", val.span())]
                    .into_iter()
                    .chain(state_annotations)
                    .collect(),
                vec![
                    sess.footer_note("output data must be written before use"),
                    sess.footer_help(&format!(
                        "consider putting `write {};` before the usage of `{}`",
                        quote_hir(val.as_ref()),
                        quote_hir(val.as_ref())
                    )),
                ],
            ),
            RError::InputSource { def, cause } => sess.error_snippet(
                "invalid input",
                vec![
                    sess.error_ann("invalid input here", def.span()),
                    sess.error_ann(&cause.message(&def.ty), def.span()),
                ]
                .into_iter()
                .chain(state_annotations)
                .collect(),
                vec![],
            ),
            RError::OutputSource { atom, cause } => sess.error_snippet(
                "invalid output",
                vec![
                    sess.error_ann("invalid output here", atom.span()),
                    sess.error_ann(&cause.message(&atom.ty), atom.span()),
                ]
                .into_iter()
                .chain(state_annotations)
                .collect(),
                vec![],
            ),
        }
    }
}
