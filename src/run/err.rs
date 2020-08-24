use std::error::Error;

use crate::atom::*;
use crate::spec::hir::*;

#[derive(Debug)]
pub enum RError {
    UnresolvedVal {
        val: Rc<HVal>,
    },
    Overflow {
        val: Rc<HVal>,
        ty: Rc<HAtomTy>,
    },
    InputSource {
        def: Rc<HAtomDef>,
        cause: AtomSourceError,
    },
    OutputSource {
        atom: Rc<HAtom>,
        cause: AtomSourceError,
    },
    AssumptionViolated {
        cond: Rc<HAtom>,
    },
}

#[derive(Debug)]
pub enum AtomSourceError {
    Parse(Box<dyn Error>),
    Type(AtomTypeError),
    Value(AtomValueError),
    End,
}

#[derive(Debug)]
pub struct AtomValueError {
    pub expected: i64,
    pub actual: i64,
}

impl From<AtomTypeError> for AtomSourceError {
    fn from(e: AtomTypeError) -> Self {
        AtomSourceError::Type(e)
    }
}

impl From<AtomValueError> for AtomSourceError {
    fn from(e: AtomValueError) -> Self {
        AtomSourceError::Value(e)
    }
}
