use proc_macro2::Span;

use crate::spec::hir_span::HasSpan;

use super::ast::*;

impl HasSpan for AExpr {
    fn span(self: &Self) -> Span {
        match self {
            AExpr::IntLit { token } => token.span(),
            AExpr::Ref { ident } => ident.token.span(),
            AExpr::Subscript { array, bracket, .. } => array.span().join(bracket.span).unwrap(),
            AExpr::Paren { paren, .. } => paren.span,
            AExpr::Mul { factors } => factors
                .first()
                .unwrap()
                .span()
                .join(factors.last().unwrap().span())
                .unwrap(),
            AExpr::Sum { first_sign, terms } => first_sign
                .as_ref()
                .map(|s| s.span())
                .unwrap_or(terms.first().unwrap().span())
                .join(terms.last().unwrap().span())
                .unwrap(),
            AExpr::RelChain { chain } => chain
                .first()
                .unwrap()
                .span()
                .join(chain.last().unwrap().span())
                .unwrap(),
        }
    }
}

impl HasSpan for ASign {
    fn span(self: &Self) -> Span {
        match self {
            Self::Plus(op) => op.span,
            Self::Minus(op) => op.span,
        }
    }
}
