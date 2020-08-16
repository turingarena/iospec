use crate::hir::*;

impl HBlock {
    pub fn funs(self: &Self) -> impl Iterator<Item = &Rc<HFun>> {
        self.stmts.iter().flat_map(|s| s.funs.iter())
    }

    pub fn vars(self: &Self) -> impl Iterator<Item = &Rc<HVar>> {
        self.stmts.iter().flat_map(|s| s.vars.iter())
    }

    pub fn defs(self: &Self) -> impl Iterator<Item = &Rc<HDefExpr>> {
        self.stmts.iter().flat_map(|s| s.defs.iter())
    }
}
