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
        args: Punctuated<ParsedDef, syn::Token![,]>,
        semi: syn::Token![;],
    },
    Call {
        inst: kw::call,
        name: ParsedIdent,
        args_paren: syn::token::Paren,
        args: Punctuated<ParsedExpr, syn::Token![,]>,
        return_value: Option<(syn::Token![->], ParsedDef)>,
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
