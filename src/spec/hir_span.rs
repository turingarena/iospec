use proc_macro2::Span;

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
            _ => panic!(),
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
        self.node.span()
    }
}

impl HasSpan for HNodeDef {
    fn span(self: &Self) -> Span {
        match &self.expr {
            HNodeDefExpr::Var { var } => var.span(),
            HNodeDefExpr::Subscript { array, index, .. } => {
                array.span().join(index.span()).unwrap()
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
