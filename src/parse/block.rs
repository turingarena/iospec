use super::*;

#[derive(Debug, Clone)]
pub struct ParsedBlock {
    pub stmts: Vec<ParsedStmt>,
}

impl Parse for ParsedBlock {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let mut stmts = vec![];
        while !input.is_empty() {
            stmts.push(input.parse()?);
        }
        Ok(ParsedBlock { stmts })
    }
}
