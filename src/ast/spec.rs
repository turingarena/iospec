pub use super::*;

#[derive(Debug, Clone)]
pub struct ParsedSpec {
    pub main: ParsedBlock,
}

impl Parse for ParsedSpec {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        Ok(ParsedSpec {
            main: input.parse()?,
        })
    }
}
