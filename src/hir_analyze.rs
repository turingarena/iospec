use std::ops::Deref;

use crate::hir::*;

impl HSpec {
    pub fn funs(self: &Self) -> Vec<Rc<HFun>> {
        self.main.funs()
    }
}

impl HBlock {
    pub fn funs(self: &Self) -> Vec<Rc<HFun>> {
        self.stmts.iter().flat_map(|s| s.funs()).collect()
    }

    pub fn vars(self: &Self) -> Vec<Rc<HVar>> {
        self.stmts.iter().flat_map(|s| s.vars()).collect()
    }

    pub fn defs(self: &Self) -> Vec<Rc<HDefExpr>> {
        self.stmts.iter().flat_map(|s| s.defs()).collect()
    }
}

impl HStmt {
    pub fn funs(self: &Self) -> Vec<Rc<HFun>> {
        match &self.kind {
            HStmtKind::Call { fun, .. } => vec![fun.clone()],
            HStmtKind::For { body, .. } => body.funs(),
            _ => Vec::new(),
        }
    }

    pub fn vars(self: &Self) -> Vec<Rc<HVar>> {
        match &self.kind {
            HStmtKind::Read { args, .. } => args.iter().map(hir_def_var).map(Rc::new).collect(),
            HStmtKind::Call { fun, .. } => fun.ret.iter().map(hir_def_var).map(Rc::new).collect(),
            HStmtKind::For { body, .. } => body.vars(),
            _ => Vec::new(),
        }
    }

    pub fn defs(self: &Self) -> Vec<Rc<HDefExpr>> {
        match &self.kind {
            HStmtKind::Read { args, .. } => args.iter().map(|d| d.expr.clone()).collect(),
            HStmtKind::Call { fun, .. } => fun.ret.iter().map(|d| d.expr.clone()).collect(),
            HStmtKind::For { body, .. } => body
                .defs()
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

impl HValExpr {
    pub fn ty(self: &Self) -> Rc<HExprTy> {
        match &self.kind {
            HValExprKind::Var { var, .. } => var.ty.clone(),
            HValExprKind::Subscript { array, .. } => match array.ty().as_ref() {
                // TODO: check index type as well
                HExprTy::Array { item, .. } => item.clone(),
                _ => todo!("recover from invalid array type"),
            },
        }
    }

    pub fn param_name(self: &Self) -> Rc<HIdent> {
        match &self.kind {
            HValExprKind::Var { var, .. } => var.ident.clone(),
            _ => todo!("recover from invalid expr in call args"),
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
        make_def_expr_ty(
            Rc::new(HExprTy::Atom {
                atom: self.atom_ty.clone(),
            }),
            self.ctx.clone(),
            self.loc.clone(),
        )
    }
}

impl HDef {
    pub fn var_name_and_ty(self: &Self) -> (Rc<HIdent>, Rc<HExprTy>) {
        self.expr.var_name_and_ty()
    }
}

pub fn hir_def_var(def: &Rc<HDef>) -> HVar {
    let (ident, ty) = def.var_name_and_ty();
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

fn make_def_expr_ty(ty: Rc<HExprTy>, ctx: Rc<HDefExprCtx>, loc: Rc<HDefLoc>) -> Rc<HExprTy> {
    match ctx.deref() {
        HDefExprCtx::Atom => ty,
        HDefExprCtx::Subscript { item, .. } => match loc.deref() {
            HDefLoc::For { range, parent } => {
                // TODO: check index matches parent
                make_def_expr_ty(
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
