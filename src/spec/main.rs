use crate::spec::hir::*;
use crate::spec::sess::Sess;

pub struct Spec {
    pub sess: Sess,
    pub hir: Rc<HSpec>,
}
