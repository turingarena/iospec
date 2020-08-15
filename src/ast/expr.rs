use super::*;

#[derive(Debug, Clone)]
pub enum ParsedExpr {
    Ref {
        ident: ParsedIdent,
    },
    Subscript {
        array: Box<ParsedExpr>,
        bracket: syn::token::Bracket,
        index: Box<ParsedExpr>,
    },
}
