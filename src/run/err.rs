use std::error::Error;

use crate::spec::hir::*;

pub enum RError {
    UnresolvedVal {
        val: Rc<HVal>,
    },
    InvalidInputAtom {
        def: Rc<HAtomDef>,
        cause: Box<dyn Error>,
    },
    InvalidOutputAtom {
        atom: Rc<HAtom>,
        cause: Box<dyn Error>,
    },
}
