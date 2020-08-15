use super::*;

#[derive(Debug, Clone)]
pub struct ParsedDef {
    pub expr: ParsedExpr,
    pub colon: syn::Token![:],
    pub ty: ParsedScalarTypeExpr,
}
