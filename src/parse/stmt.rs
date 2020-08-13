use super::*;

#[derive(Debug, Clone)]
pub enum ParsedStmt {
    Write {
        inst: kw::write,
        args: Punctuated<ParsedExpr, syn::Token![,]>,
        semi: syn::Token![;],
    },
    Read {
        inst: kw::read,
        args: Punctuated<ParsedDecl, syn::Token![,]>,
        semi: syn::Token![;],
    },
    Call {
        inst: kw::call,
        name: ParsedIdent,
        args_paren: syn::token::Paren,
        args: Punctuated<ParsedExpr, syn::Token![,]>,
        return_value: Option<(syn::Token![->], ParsedDecl)>,
        semi: syn::Token![;],
    },
    For {
        for_token: syn::Token![for],
        index_name: ParsedIdent,
        upto: kw::upto,
        bound: ParsedExpr,
        body_brace: syn::token::Brace,
        body: ParsedBlock,
    },
}

impl Parse for ParsedStmt {
    fn parse(input: &ParseBuffer) -> Result<Self, Error> {
        if input.peek(kw::read) {
            Ok(ParsedStmt::Read {
                inst: input.parse()?,
                args: Punctuated::parse_separated_nonempty(input)?,
                semi: input.parse()?,
            })
        } else if input.peek(kw::write) {
            Ok(ParsedStmt::Write {
                inst: input.parse()?,
                args: Punctuated::parse_separated_nonempty(input)?,
                semi: input.parse()?,
            })
        } else if input.peek(kw::call) {
            let args_input;

            Ok(ParsedStmt::Call {
                inst: input.parse()?,
                name: input.parse()?,
                args_paren: syn::parenthesized!(args_input in input),
                args: Punctuated::parse_terminated(&args_input)?,
                return_value: if input.peek(syn::Token![->]) {
                    Some((input.parse()?, input.parse()?))
                } else {
                    None
                },
                semi: input.parse()?,
            })
        } else if input.peek(syn::Token![for]) {
            let body_input;
            Ok(ParsedStmt::For {
                for_token: input.parse()?,
                index_name: input.parse()?,
                upto: input.parse()?,
                bound: input.parse()?,
                body_brace: syn::braced!(body_input in input),
                body: body_input.parse()?,
            })
        } else {
            Err(Error::new(input.span(), "statement expected"))
        }
    }
}
