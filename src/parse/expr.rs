use super::*;

#[derive(Debug, Clone)]
pub enum ParsedExpr {
    Var {
        ident: ParsedIdent,
    },
    Index {
        array: Box<ParsedExpr>,
        bracket: syn::token::Bracket,
        index: Box<ParsedExpr>,
    },
}

impl Parse for ParsedExpr {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        let mut current = ParsedExpr::Var {
            ident: input.parse()?,
        };

        while input.peek(syn::token::Bracket) {
            let index_input;
            current = ParsedExpr::Index {
                array: Box::new(current),
                bracket: syn::bracketed!(index_input in input),
                index: index_input.parse()?,
            };
        }
        Ok(current)
    }
}

