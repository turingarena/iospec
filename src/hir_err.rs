//! Construct HIR nodes representing compilation errors

use crate::hir::*;
use crate::ty::*;

pub trait HirCreateErr {
    fn create_err() -> Self;
}

pub trait HErr {
    fn err() -> Rc<Self>;
}

impl<T: HirCreateErr> HErr for T {
    fn err() -> Rc<Self> {
        Rc::new(Self::create_err())
    }
}

impl HirCreateErr for HAtomTy {
    fn create_err() -> Self {
        HAtomTy {
            expr: HErr::err(),
            sem: AtomTy::Err,
        }
    }
}

impl HirCreateErr for HValTy {
    fn create_err() -> Self {
        HValTy::Err
    }
}

impl HirCreateErr for HVarKind {
    fn create_err() -> Self {
        HVarKind::Err
    }
}

impl HirCreateErr for HAtomTyExpr {
    fn create_err() -> Self {
        HAtomTyExpr::Err
    }
}

impl HirCreateErr for HDataExpr {
    fn create_err() -> Self {
        HDataExpr::Err
    }
}

impl HirCreateErr for HDataNode {
    fn create_err() -> Self {
        HDataNode {
            expr: HErr::err(),
            root: HErr::err(),
            ty: HErr::err(),
        }
    }
}

impl HirCreateErr for HDataVar {
    fn create_err() -> Self {
        HDataVar {
            expr: HErr::err(),
            ty: HErr::err(),
        }
    }
}

impl HirCreateErr for HDataVarExpr {
    fn create_err() -> Self {
        HDataVarExpr::Err
    }
}

impl HirCreateErr for HArgExpr {
    fn create_err() -> Self {
        HArgExpr::Err
    }
}
