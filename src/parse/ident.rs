use super::*;

#[derive(Debug, Clone)]
pub struct ParsedIdent {
    pub sym: String,
}

impl Parse for ParsedIdent {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        // Parsing TokenTree instead of Indent to ignore Rust keywords
        let token_tree: proc_macro2::TokenTree = input.parse()?;
        match token_tree {
            proc_macro2::TokenTree::Ident(x) => Ok(ParsedIdent { sym: x.to_string() }),
            _ => Err(Error::new(token_tree.span(), "expected identifier")),
        }
    }
}
