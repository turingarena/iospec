use proc_macro2::Span;
use syn::spanned::Spanned;

use crate::spec::hir::*;

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
            HValExpr::Lit { token, .. } => token.span(),
            HValExpr::Paren { paren, .. } => paren.span,
            HValExpr::Mul { factors, .. } => factors
                .first()
                .unwrap()
                .span()
                .join(factors.last().unwrap().span())
                .unwrap(),
            HValExpr::Err => panic!(),
            HValExpr::Sum { terms, .. } => {
                let extrema: Vec<_> = [terms.first(), terms.last()]
                    .iter()
                    .map(|t| t.unwrap())
                    .map(|(sign, term)| {
                        let sign_span = match sign {
                            HSign::Plus(Some(op)) => Some(op.span()),
                            HSign::Minus(op) => Some(op.span()),
                            HSign::Plus(None) => None,
                        };
                        if let Some(span) = sign_span {
                            term.span().join(span).unwrap()
                        } else {
                            term.span()
                        }
                    })
                    .collect();
                extrema[0].join(extrema[1]).unwrap()
            }
            HValExpr::RelChain { rels } => rels
                .first()
                .unwrap()
                .0
                .span()
                .join(rels.last().unwrap().2.span())
                .unwrap(),
        }
    }
}

impl HasSpan for HRange {
    fn span(self: &Self) -> Span {
        self.index.span().join(self.bound.val.span()).unwrap()
    }
}

impl HasSpan for HAtomTy {
    fn span(self: &Self) -> Span {
        match &self.expr {
            HAtomTyExpr::Name { name } => name.span(),
            HAtomTyExpr::Lit { token } => token.span(),
            // FIXME: duplicate code
            HAtomTyExpr::Rel { rels } => rels
                .first()
                .unwrap()
                .0
                .span()
                .join(rels.last().unwrap().2.span())
                .unwrap(),
            HAtomTyExpr::Err => panic!(),
        }
    }
}

impl HasSpan for HAtom {
    fn span(self: &Self) -> Span {
        self.val.span()
    }
}

impl HasSpan for HAtomDef {
    fn span(self: &Self) -> Span {
        self.node.span().join(self.ty.span()).unwrap()
    }
}

impl HasSpan for HNodeDef {
    fn span(self: &Self) -> Span {
        match &self.expr {
            HNodeDefExpr::Var { var } => var.span(),
            HNodeDefExpr::Subscript { array, bracket, .. } => {
                array.span().join(bracket.span).unwrap()
            }
            HNodeDefExpr::Err => panic!(),
        }
    }
}

impl HasSpan for HVarDef {
    fn span(self: &Self) -> Span {
        match &self.expr {
            HVarDefExpr::Name { name } => name.span(),
            HVarDefExpr::Err => panic!(),
        }
    }
}
