use std::error::Error;

use crate::spec::hir::*;

#[derive(Debug)]
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

#[derive(Debug)]
pub enum AtomSourceError {
    Parse(Box<dyn Error>),
    Type(i64),
    Value(AtomValueError),
    End,
}

#[derive(Debug)]
pub struct AtomValueError {
    pub expected: i64,
    pub actual: i64,
}
