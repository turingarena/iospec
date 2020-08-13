extern crate proc_macro2;
extern crate syn;

use syn::parse::Parse;
use syn::parse::ParseBuffer;
use syn::punctuated::Punctuated;
use syn::Error;

pub use block::*;
pub use def::*;
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

mod block;
mod def;
mod expr;
mod ident;
mod spec;
mod stmt;
mod ty;
