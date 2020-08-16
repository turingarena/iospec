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
            HStmtKind::For { body, .. } => body.defs().into_iter()
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

impl HVar {
    pub fn ty(self: &Self) -> Rc<HExprTy> {
        match &self.kind {
            HVarKind::Data { def, .. } => def.var_ty.clone(),
            HVarKind::Index { range, .. } => Rc::new(HExprTy::Index { range: range.clone() }),
        }
    }
}

impl HValExpr {
    pub fn ty(self: &Self) -> Rc<HExprTy> {
        match &self.kind {
            HValExprKind::Var { var, .. } => var.ty(),
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

fn hir_def_var(def: &Rc<HDef>) -> HVar {
    HVar {
        ident: def.ident.clone(),
        kind: HVarKind::Data { def: def.clone() },
    }
}

