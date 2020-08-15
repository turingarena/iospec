use super::*;

#[derive(Debug, Clone)]
pub struct ParsedScalarTypeExpr {
    pub ident: ParsedIdent,
}

impl Parse for ParsedScalarTypeExpr {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(Self {
            ident: input.parse()?,
        })
    }
}
