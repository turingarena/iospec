use super::lang::cpp::gen_file;
use super::lir_build::build_lir;
use crate::spec::*;

pub fn code_gen(spec: &Spec) -> String {
    let lir = build_lir(&spec.hir);

    gen_file(&lir)
}
