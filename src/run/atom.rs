use crate::spec::ty::*;

use super::err::*;

#[derive(Debug, Clone, Copy)]
pub struct RAtom {
    ty: AtomTy,
    value: i64,
}

impl RAtom {
    pub fn new(ty: AtomTy, value: i64) -> RAtom {
        Self::try_new(ty, value).unwrap()
    }

    pub fn try_new(ty: AtomTy, value: i64) -> Result<RAtom, AtomTypeError> {
        let ok = match ty {
            AtomTy::Bool => value == 0 || value == 1,
            AtomTy::Nat { size } => 0 <= value && value <= size.max_safe_value(),
            AtomTy::Int { size } => {
                -size.max_safe_value() <= value && value <= size.max_safe_value()
            }
            AtomTy::Err => unreachable!(),
        };

        if ok {
            Ok(RAtom { ty, value })
        } else {
            Err(AtomTypeError { ty, actual: value })
        }
    }

    pub fn value_i64(self: &Self) -> i64 {
        self.value
    }
}
