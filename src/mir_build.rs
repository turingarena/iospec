use std::ops::Deref;

use crate::hir::*;
use crate::mir::*;

fn mir_block(hir: &HN<HBlock>) -> Vec<MInst> {
    hir.stmts.iter().flat_map(mir_stmt_insts).collect()
}

fn mir_stmt_insts(hir: &HN<HStmt>) -> Vec<MInst> {
    let mut insts = vec![];

    insts.extend(hir.conses.iter().flat_map(|c| match (*c).deref() {
        HCons::Scalar { def } => vec![mir_inst_decl(def)],
        _ => vec![],
    }));

    let stmt_insts = match &hir.kind {
        HStmtKind::Read { args, .. } => args
            .iter()
            .map(Deref::deref)
            .map(|HDef { ty, expr, .. }| MInst::Read {
                ty: mir_def_ty(ty),
                arg: mir_def_expr(expr),
            })
            .collect(),
        HStmtKind::Write { args, .. } => args
            .iter()
            .map(|expr| MInst::Write {
                ty: mir_expr_def_ty(expr),
                arg: mir_expr(expr),
            })
            .collect(),
        HStmtKind::Call {
            name, args, ret, ..
        } => vec![MInst::Call {
            name: name.token.to_string(),
            args: args.iter().map(mir_expr).collect(),
            ret: ret
                .as_ref()
                .map(Deref::deref)
                .map(|HDef { expr, .. }| mir_def_expr(expr)),
        }],
        HStmtKind::For { range, body, .. } => vec![MInst::For {
            index_name: range.index_name.token.to_string(),
            bound: mir_expr(&range.bound),
            body: Box::new(mir_block(body)),
        }],
    };

    insts.extend(stmt_insts);

    insts
}

fn mir_inst_decl(hir: &HN<HDef>) -> MInst {
    MInst::Decl {
        name: hir.expr.ident.token.to_string(),
        ty: MConsTy::Atom {
            atom: mir_def_ty(&hir.ty),
        },
    }
}

fn mir_cons_ty(hir: &HN<HDef>) -> MConsTy {
    mir_def_expr_cons_ty(&hir.expr, &mir_def_ty(&hir.ty))
}

fn mir_def_ty(hir: &HN<HAtomTy>) -> MAtomTy {
    // TODO: exploit interning of identifiers
    match hir.ident.token.to_string().as_str() {
        "n32" => MAtomTy::N32,
        "i32" => MAtomTy::I32,
        "n64" => MAtomTy::N64,
        "i64" => MAtomTy::I64,
        _ => unreachable!(), // TODO: recover
    }
}

fn mir_expr(hir: &HN<HValExpr>) -> MExpr {
    match &hir.kind {
        HValExprKind::Ref { ident, .. } => MExpr::Var {
            name: ident.token.to_string(),
        },
        HValExprKind::Subscript { array, index, .. } => MExpr::Subscript {
            array: Box::new(mir_expr(array)),
            index: Box::new(mir_expr(index)),
        },
    }
}

fn mir_expr_ty(hir: &HN<HValExpr>) -> MConsTy {
    match &hir.kind {
        HValExprKind::Ref {
            target: Some(target),
            ..
        } => match &target.kind {
            HRefKind::Var { def, .. } => mir_cons_ty(def),
            HRefKind::Index { .. } => MConsTy::Atom { def: MAtomTy::N32 },
            _ => todo!("recover"),
        },
        HValExprKind::Subscript { array, .. } => match mir_expr_ty(array) {
            MConsTy::Array { item, .. } => *item,
            _ => todo!("recover"),
        },
        _ => todo!("recover"),
    }
}

fn mir_expr_def_ty(hir: &HN<HValExpr>) -> MAtomTy {
    match mir_expr_ty(hir) {
        MConsTy::Atom { def } => def,
        _ => todo!("recover"),
    }
}

fn mir_def_expr(hir: &HN<HDefExpr>) -> MExpr {
    match &hir.kind {
        HDefExprKind::Var { ident, .. } => MExpr::Var {
            name: ident.token.to_string(),
        },
        HDefExprKind::Subscript { array, index, .. } => MExpr::Subscript {
            array: Box::new(mir_def_expr(array)),
            index: Box::new(mir_expr(index)),
        },
    }
}

fn mir_def_expr_cons_ty(hir: &HN<HDefExpr>, def: &MAtomTy) -> MConsTy {
    match &hir.kind {
        HDefExprKind::Var { .. } => MConsTy::Atom { def: def.clone() },
        HDefExprKind::Subscript { array, .. } => MConsTy::Array {
            item: Box::new(mir_def_expr_cons_ty(array, def)),
        },
    }
}

pub fn build_mir(spec: &HSpec) -> MSpec {
    MSpec {
        main: mir_block(&spec.main),
    }
}
