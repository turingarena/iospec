use crate::spec::hir_quote::quote_hir;
use crate::spec::hir_span::HasSpan;
use crate::spec::sess::Sess;

use super::err::*;

impl RError {
    pub fn diagnostic_message(self: &Self, sess: &Sess) -> String {
        match self {
            RError::UnresolvedVal { val } => sess.error_snippet(
                "expression value is unknown",
                vec![sess.error_ann("has an unknown value", val.span())],
                vec![
                    sess.footer_note("output data must be written before use"),
                    sess.footer_help(&format!(
                        "consider putting `write {};` before the usage of `{}`",
                        quote_hir(val.as_ref()),
                        quote_hir(val.as_ref())
                    )),
                ],
            ),
            RError::InputSource { .. } => todo!(),
            RError::OutputSource { .. } => todo!(),
        }
    }
}
