use proc_macro2::Span;

use crate::hir::*;

pub trait HasSpan {
    fn span(self: &Self) -> Span;
}

impl HasSpan for HIdent {
    fn span(self: &Self) -> Span {
        self.token.span()
    }
}

impl HasSpan for HVal {
    fn span(self: &Self) -> Span {
        self.expr.span()
    }
}

impl HasSpan for HValExpr {
    fn span(self: &Self) -> Span {
        match self {
            HValExpr::Var { ident, .. } => ident.token.span(),
            HValExpr::Subscript { array, bracket, .. } => array.span().join(bracket.span).unwrap(),
        }
    }
}

impl HasSpan for HRange {
    fn span(self: &Self) -> Span {
        self.index.span().join(self.bound.val.span()).unwrap()
    }
}
