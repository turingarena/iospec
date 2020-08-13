use super::*;

#[derive(Debug, Clone)]
pub struct ParsedDef {
    pub expr: ParsedExpr,
    pub colon: syn::Token![:],
    pub ty: ParsedScalarTypeExpr,
}

impl Parse for ParsedDef {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            expr: input.parse()?,
            colon: input.parse()?,
            ty: input.parse()?,
        })
    }
}
