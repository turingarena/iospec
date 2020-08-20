//! Construct HIR nodes representing compilation errors

use crate::hir::*;
use crate::ty::*;

pub trait HErr {
    fn err() -> Self;
}

impl<T: HErr> HErr for Rc<T> {
    fn err() -> Self {
        Rc::new(T::err())
    }
}

impl HErr for HAtomTy {
    fn err() -> Self {
        HAtomTy {
            expr: HErr::err(),
            sem: AtomTy::Err,
        }
    }
}

impl HErr for HValTy {
    fn err() -> Self {
        HValTy::Err
    }
}

impl HErr for HVarExpr {
    fn err() -> Self {
        HVarExpr::Err
    }
}

impl HErr for HAtomTyExpr {
    fn err() -> Self {
        HAtomTyExpr::Err
    }
}

impl HErr for HNodeDefExpr {
    fn err() -> Self {
        HNodeDefExpr::Err
    }
}

impl HErr for HNodeDef {
    fn err() -> Self {
        HNodeDef {
            expr: HErr::err(),
            root_var: HErr::err(),
            ty: HErr::err(),
        }
    }
}

impl HErr for HVarDef {
    fn err() -> Self {
        HVarDef {
            expr: HErr::err(),
            ty: HErr::err(),
        }
    }
}

impl HErr for HVarDefExpr {
    fn err() -> Self {
        HVarDefExpr::Err
    }
}

impl HErr for HArgExpr {
    fn err() -> Self {
        HArgExpr::Err
    }
}
