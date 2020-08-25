use genco::prelude::*;

use crate::spec::Spec;

use super::lir::*;
use super::lir_build::build_lir;
use super::lir_code::Code;

pub fn code_gen<C: LirConfig>(spec: &Spec, config: &C) -> String
where
    Lir<C, LSpec<C>>: Code,
{
    let lir = &build_lir(&spec.hir, config);
    let tokens: Tokens = quote!(#lir);

    tokens.to_file_string().unwrap()
}
