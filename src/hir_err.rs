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

impl HirCreateErr for HVarExpr {
    fn create_err() -> Self {
        HVarExpr::Err
    }
}

impl HirCreateErr for HAtomTyExpr {
    fn create_err() -> Self {
        HAtomTyExpr::Err
    }
}

impl HirCreateErr for HNodeDefExpr {
    fn create_err() -> Self {
        HNodeDefExpr::Err
    }
}

impl HirCreateErr for HNodeDef {
    fn create_err() -> Self {
        HNodeDef {
            expr: HErr::err(),
            root_var: HErr::err(),
            ty: HErr::err(),
            var: None,
        }
    }
}

impl HirCreateErr for HVarDef {
    fn create_err() -> Self {
        HVarDef {
            expr: HErr::err(),
            ty: HErr::err(),
        }
    }
}

impl HirCreateErr for HVarDefExpr {
    fn create_err() -> Self {
        HVarDefExpr::Err
    }
}

impl HirCreateErr for HArgExpr {
    fn create_err() -> Self {
        HArgExpr::Err
    }
}
