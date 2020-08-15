use crate::hir::*;
use crate::mir::*;

impl HirNode<HirBlock> {
    fn build_mir(self: &Self) -> Vec<MirInst> {
        self.stmts.iter().flat_map(|s| s.build_mir()).collect()
    }
}

impl HirNode<HirStmt> {
    fn build_mir(self: &Self) -> Vec<MirInst> {
        match &self.kind {
            HirStmtKind::Read { args, .. } => args
                .iter()
                .flat_map(|def| vec![
                    def.build_decl_mir(),
                    MirInst::Read {
                        ty: def.ty.build_mir(),
                        arg: def.expr.build_mir(),
                    },
                ])
                .collect(),
            HirStmtKind::Write { args, .. } => args.iter().map(|expr| MirInst::Write {
                arg: expr.build_mir(),
            }).collect(),
            HirStmtKind::Call { name, args, return_value, .. } => {
                let call = MirInst::Call {
                    name: name.token.to_string(),
                    args: args.iter().map(|a| a.build_mir()).collect(),
                    ret: return_value.as_ref().map(|r| r.expr.build_mir()),
                };

                if let Some(return_value) = return_value {
                    vec![
                        return_value.build_decl_mir(),
                        call,
                    ]
                } else {
                    vec![
                        call,
                    ]
                }
            }
            HirStmtKind::For { range, body, .. } => vec![
                MirInst::For {
                    index_name: range.index_name.token.to_string(),
                    bound: range.bound.build_mir(),
                    body: Box::new(body.build_mir()),
                }
            ],
        }
    }
}

impl HirNode<HirDef> {
    fn build_decl_mir(self: &Self) -> MirInst {
        MirInst::Decl {
            name: self.expr.ident.token.to_string(),
            ty: MirConsTy::Scalar {
                def: self.ty.build_mir(),
            },
        }
    }
}

impl HirNode<HirScalarTypeExpr> {
    fn build_mir(self: &Self) -> MirDefTy {
        todo!()
    }
}

impl HirNode<HirExpr> {
    fn build_mir(self: &Self) -> MirExpr {
        match &self.kind {
            HirExprKind::Ref {
                ident, ..
            } => MirExpr::Var {
                name: ident.token.to_string(),
            },
            HirExprKind::Subscript { array, index, .. } => MirExpr::Subscript {
                array: Box::new(array.build_mir()),
                index: Box::new(index.build_mir()),
            }
        }
    }
}

impl HirNode<HirDefExpr> {
    fn build_mir(self: &Self) -> MirExpr {
        match &self.kind {
            HirDefExprKind::Var {
                ident, ..
            } => MirExpr::Var {
                name: ident.token.to_string(),
            },
            HirDefExprKind::Subscript { array, index, .. } => MirExpr::Subscript {
                array: Box::new(array.build_mir()),
                index: Box::new(index.build_mir()),
            }
        }
    }
}