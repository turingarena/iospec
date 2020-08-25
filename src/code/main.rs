use genco::prelude::*;

use crate::spec::Spec;

use super::lir::*;
use super::lir_build::build_lir;
use super::lir_code::Code;

pub fn code_gen<F: LirFlavor>(spec: &Spec) -> String
where
    Lir<F, LSpec<F>>: Code,
{
    let lir = &build_lir(&spec.hir);
    let tokens: Tokens = quote!(#lir);

    tokens.to_file_string().unwrap()
}
