//! Relation operators

use syn::export::fmt::{Display, Error};
use syn::export::Formatter;

#[derive(Debug, Copy, Clone)]
pub enum RelOp {
    Eq(syn::token::Eq),
    Ne(syn::token::Ne),
    Lt(syn::token::Lt),
    Le(syn::token::Le),
    Gt(syn::token::Gt),
    Ge(syn::token::Ge),
}

impl RelOp {
    pub fn apply(self: &Self, left: i64, right: i64) -> bool {
        match self {
            RelOp::Eq(_) => left == right,
            RelOp::Ne(_) => left != right,
            RelOp::Lt(_) => left < right,
            RelOp::Le(_) => left <= right,
            RelOp::Gt(_) => left > right,
            RelOp::Ge(_) => left >= right,
        }
    }
}

impl Display for RelOp {
    fn fmt(self: &Self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            RelOp::Eq(_) => f.write_str("=="),
            RelOp::Ne(_) => f.write_str("!="),
            RelOp::Lt(_) => f.write_str("<"),
            RelOp::Le(_) => f.write_str("<="),
            RelOp::Gt(_) => f.write_str(">"),
            RelOp::Ge(_) => f.write_str(">="),
        }
    }
}
