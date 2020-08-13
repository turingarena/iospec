extern crate proc_macro2;
extern crate syn;

use syn::Error;
use syn::parse::Parse;
use syn::parse::ParseBuffer;
use syn::punctuated::Punctuated;

pub use block::*;
pub use decl::*;
pub use expr::*;
pub use ident::*;
pub use spec::*;
pub use stmt::*;
pub use ty::*;

mod kw {
    syn::custom_keyword!(read);
    syn::custom_keyword!(write);
    syn::custom_keyword!(call);
    syn::custom_keyword!(upto);
}

mod ident;
mod ty;
mod expr;
mod decl;
mod stmt;
mod block;
mod spec;
