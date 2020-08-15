use std::ops::Deref;

use crate::hir::*;
use crate::mir::*;

pub fn build_mir(spec: &HSpec) -> MSpec {
    MSpec {
        main: mir_block(&spec.main),
    }
}

fn mir_block(hir: &HN<HBlock>) -> Vec<MInst> {
    let mut insts = Vec::new();

    insts.extend(hir.stmts.iter().flat_map(mir_stmt_decls));
    insts.extend(hir.stmts.iter().flat_map(mir_stmt_insts));

    insts
}

fn mir_stmt_decls(hir: &HN<HStmt>) -> Vec<MInst> {
    hir.defs
        .iter()
        .flat_map(|c| match &c.kind {
            HDefExprKind::Var { ident } => Some(MInst::Decl {
                name: ident.token.to_string(),
                ty: mir_expr_ty(&c.var_ty),
            }),
            _ => None,
        })
        .collect()
}

fn mir_stmt_insts(hir: &HN<HStmt>) -> Vec<MInst> {
    match &hir.kind {
        HStmtKind::Read { args, .. } => args
            .iter()
            .map(Deref::deref)
            .map(|HDef { atom_ty, expr, .. }| MInst::Read {
                ty: mir_atom_ty(atom_ty),
                arg: mir_def_expr(expr),
            })
            .collect(),
        HStmtKind::Write { args, .. } => args
            .iter()
            .map(|expr| MInst::Write {
                ty: mir_atom_ty(match expr.ty.deref() {
                    HExprTy::Atom { atom } => atom,
                    _ => todo!("recover"),
                }),
                arg: mir_val_expr(expr),
            })
            .collect(),
        HStmtKind::Call {
            name, args, ret, ..
        } => vec![MInst::Call {
            name: name.token.to_string(),
            args: args.iter().map(mir_val_expr).collect(),
            ret: ret
                .as_ref()
                .map(Deref::deref)
                .map(|HDef { expr, .. }| mir_def_expr(expr)),
        }],
        HStmtKind::For { range, body, .. } => vec![MInst::For {
            index_name: range.index_name.token.to_string(),
            bound: mir_val_expr(&range.bound),
            body: Box::new(mir_block(body)),
        }],
    }
}

fn mir_def_expr(hir: &HN<HDefExpr>) -> MExpr {
    match &hir.kind {
        HDefExprKind::Var { ident, .. } => MExpr::Var {
            name: ident.token.to_string(),
        },
        HDefExprKind::Subscript { array, index, .. } => MExpr::Subscript {
            array: Box::new(mir_def_expr(array)),
            index: Box::new(mir_val_expr(index)),
        },
    }
}

fn mir_val_expr(hir: &HN<HValExpr>) -> MExpr {
    match &hir.kind {
        HValExprKind::Ref { ident, .. } => MExpr::Var {
            name: ident.token.to_string(),
        },
        HValExprKind::Subscript { array, index, .. } => MExpr::Subscript {
            array: Box::new(mir_val_expr(array)),
            index: Box::new(mir_val_expr(index)),
        },
    }
}

fn mir_atom_ty(hir: &HN<HAtomTy>) -> MAtomTy {
    // TODO: exploit interning of identifiers
    match hir.ident.token.to_string().as_str() {
        "n32" => MAtomTy::N32,
        "i32" => MAtomTy::I32,
        "n64" => MAtomTy::N64,
        "i64" => MAtomTy::I64,
        _ => unreachable!(), // TODO: recover
    }
}

fn mir_expr_ty(hir: &HN<HExprTy>) -> MConsTy {
    let hir: &HExprTy = hir.deref();

    match hir {
        HExprTy::Atom { atom } => MConsTy::Atom {
            atom: mir_atom_ty(atom),
        },
        HExprTy::Array { item, .. } => MConsTy::Array {
            item: Box::new(mir_expr_ty(item)),
        },
        HExprTy::Index { .. } => MConsTy::Atom { atom: MAtomTy::N32 },
    }
}
