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
        }
    }
}
