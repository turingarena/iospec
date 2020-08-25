use genco::prelude::*;

use crate::spec::Spec;

use super::lir::*;
use super::lir_build::build_lir;
use super::lir_code::Code;

pub fn code_gen<L: CodeLang>(spec: &Spec, lang: &L) -> String
where
    Lir<L, LSpec<L>>: Code<L>,
{
    let lir = &build_lir(&spec.hir, lang);
    let tokens: Tokens = quote!(#lir);

    tokens.to_file_string().unwrap()
}
