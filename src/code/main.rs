use super::lang::cpp::gen_file;
use super::lir_build::build_lir;
use crate::spec::hir::*;

pub fn code_gen(spec: &Rc<HSpec>) -> String {
    let lir = build_lir(spec);

    gen_file(&lir)
}
