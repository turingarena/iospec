use std::ops::Deref;

use crate::hir::*;

impl HSpec {
    pub fn funs(self: &Self) -> Vec<Rc<HFun>> {
        self.main.funs()
    }
}

impl HStmt {
    pub fn funs(self: &Self) -> Vec<Rc<HFun>> {
        match self {
            HStmt::Block { stmts } => stmts.iter().flat_map(|s| s.funs()).collect(),
            HStmt::Call { fun, .. } => vec![fun.clone()],
            HStmt::For { body, .. } => body.funs(),
            _ => Vec::new(),
        }
    }

    pub fn vars(self: &Self) -> Vec<Rc<HVar>> {
        match self {
            HStmt::Block { stmts } => stmts.iter().flat_map(|s| s.vars()).collect(),
            HStmt::Read { args, .. } => args.iter().map(hir_def_var).map(Rc::new).collect(),
            HStmt::Call { fun, .. } => fun.ret.iter().map(hir_def_var).map(Rc::new).collect(),
            HStmt::For { body, .. } => body.vars(),
            _ => Vec::new(),
        }
    }

    pub fn allocs(self: &Self) -> Vec<Rc<HDefExpr>> {
        match self {
            HStmt::Block { stmts } => stmts.iter().flat_map(|s| s.allocs()).collect(),
            HStmt::Read { args, .. } => args.iter().map(|d| d.expr.clone()).collect(),
            HStmt::Call { fun, .. } => fun.ret.iter().map(|d| d.expr.clone()).collect(),
            HStmt::For { body, .. } => body
                .allocs()
                .into_iter()
                .flat_map(|expr| match &expr.kind {
                    // TODO: check index somewhere?
                    HDefExprKind::Subscript { array, .. } => Some(array.clone()),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        }
    }
}

impl HDefExpr {
    pub fn var_name_and_ty(self: &Self) -> (Rc<HIdent>, Rc<HExprTy>) {
        match &self.kind {
            HDefExprKind::Var { ident, .. } => (ident.clone(), self.ty()),
            HDefExprKind::Subscript { array, .. } => array.var_name_and_ty(),
        }
    }

    pub fn ty(self: &Self) -> Rc<HExprTy> {
        hir_def_ctx_ty(
            Rc::new(HExprTy::Atom {
                atom: self.atom_ty.clone(),
            }),
            self.ctx.clone(),
            self.loc.clone(),
        )
    }
}

pub fn hir_def_var(def: &Rc<HDef>) -> HVar {
    let (ident, ty) = def.expr.var_name_and_ty();
    HVar {
        ident,
        ty,
        kind: HVarKind::Data { def: def.clone() },
    }
}

pub fn hir_index_var(range: &Rc<HRange>) -> HVar {
    HVar {
        ident: range.index.clone(),
        ty: Rc::new(HExprTy::Index {
            range: range.clone(),
        }),
        kind: HVarKind::Index {
            range: range.clone(),
        },
    }
}

fn hir_def_ctx_ty(ty: Rc<HExprTy>, ctx: Rc<HDefExprCtx>, loc: Rc<HDefLoc>) -> Rc<HExprTy> {
    match ctx.deref() {
        HDefExprCtx::Atom => ty,
        HDefExprCtx::Subscript { item, .. } => match loc.deref() {
            HDefLoc::For { range, parent } => {
                // TODO: check index matches parent
                hir_def_ctx_ty(
                    Rc::new(HExprTy::Array {
                        item: ty.clone(),
                        range: range.clone(),
                    }),
                    item.clone(),
                    parent.clone(),
                )
            }
            _ => todo!("recover from wrong index in def expr subscript"),
        },
    }
}
