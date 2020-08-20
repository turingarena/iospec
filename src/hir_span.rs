use proc_macro2::Span;

use crate::hir::*;

pub trait HasSpan {
    fn span(self: &Self) -> Span;
}

impl HasSpan for HName {
    fn span(self: &Self) -> Span {
        self.ident.span()
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
            HValExpr::Var { name, .. } => name.ident.span(),
            HValExpr::Subscript { array, bracket, .. } => array.span().join(bracket.span).unwrap(),
        }
    }
}

impl HasSpan for HRange {
    fn span(self: &Self) -> Span {
        self.index.span().join(self.bound.val.span()).unwrap()
    }
}
