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
                ty: expr.build_def_ty_mir(),
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
        // TODO: exploit interning of identifiers
        match self.ident.token.to_string().as_str() {
            "n32" => MirDefTy::N32,
            "i32" => MirDefTy::I32,
            "n64" => MirDefTy::N64,
            "i64" => MirDefTy::I64,
            _ => unreachable!(), // TODO: recover
        }
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

    fn build_ty_mir(self: &Self) -> MirConsTy {
        match &self.kind {
            HirExprKind::Ref {
                target: Some(target),
                ..
            } => match &target.kind {
                HirRefKind::Var { def, .. } => todo!(),
                HirRefKind::Index { .. } => MirConsTy::Scalar { def: MirDefTy::N32 },
                _ => todo!("recover"),
            },
            HirExprKind::Subscript { array, .. } => match array.build_ty_mir() {
                MirConsTy::Array { item, .. } => *item,
                _ => todo!("recover"),
            },
            _ => todo!("recover"),
        }
    }

    fn build_def_ty_mir(self: &Self) -> MirDefTy {
        match self.build_ty_mir() {
            MirConsTy::Scalar { def } => def,
            _ => todo!("recover"),
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

    fn build_ty_mir(self: &Self, def: &MirDefTy) -> MirConsTy {
        match &self.kind {
            HirDefExprKind::Var { .. } => MirConsTy::Scalar {
                def: def.clone(),
            },
            HirDefExprKind::Subscript { array, .. } => MirConsTy::Array {
                item: Box::new(array.build_ty_mir(def)),
            }
        }
    }
}

pub fn build_mir(spec: &HirSpec) -> MirSpec {
    MirSpec {
        main: spec.main.build_mir(),
    }
}