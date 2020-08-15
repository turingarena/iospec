extern crate proc_macro2;
extern crate syn;

use syn::punctuated::Punctuated;

pub use block::*;
pub use def::*;
pub use expr::*;
pub use ident::*;
pub use spec::*;
pub use stmt::*;
pub use ty::*;

pub mod kw {
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
