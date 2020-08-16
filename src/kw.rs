//! Definitions of custom keywords.

extern crate syn;

pub mod kw {
    syn::custom_keyword!(read);
    syn::custom_keyword!(write);
    syn::custom_keyword!(call);
    syn::custom_keyword!(upto);
}
