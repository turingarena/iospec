use std::error::Error;

use crate::spec::hir::*;

pub enum RError {
    UnresolvedVal {
        val: Rc<HVal>,
    },
    InputSource {
        def: Rc<HAtomDef>,
        cause: AtomSourceError,
    },
    OutputSource {
        atom: Rc<HAtom>,
        cause: AtomSourceError,
    },
}

pub enum AtomSourceError {
    Parse(Box<dyn Error>),
    Type,
    Value,
    End,
}
