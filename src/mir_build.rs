//! Build MIR from HIR.

use std::ops::Deref;

use crate::hir::*;
use crate::mir::*;
use crate::ty::*;

pub fn build_mir(spec: &HSpec) -> MSpec {
    MSpec {
        funs: spec.funs.iter().map(mir_fun).collect(),
        main: mir_exec_insts(&spec.main),
    }
}

fn mir_insts(hir: &Rc<HStmt>) -> Vec<MInst> {
    let mut insts = Vec::new();
    insts.extend(mir_decl_insts(hir).into_iter());
    insts.extend(mir_alloc_insts(hir).into_iter());
    insts.extend(mir_exec_insts(hir).into_iter());
    insts
}

fn mir_decl_insts(hir: &Rc<HStmt>) -> Vec<MInst> {
    hir.allocs()
        .iter()
        .flat_map(|node| match node.expr.deref() {
            HDataExpr::Var { var } => match var.expr.deref() {
                HDataVarExpr::Name { name } => Some(MInst::Decl {
                    name: name.to_string(),
                    ty: mir_expr_ty(&var.ty),
                }),
                _ => unreachable!(),
            },
            _ => None,
        })
        .collect()
}

fn mir_alloc_insts(hir: &Rc<HStmt>) -> Vec<MInst> {
    match hir.deref() {
        HStmt::For { body, .. } => body
            .allocs()
            .into_iter()
            .flat_map(|node| match node.expr.deref() {
                HDataExpr::Subscript { array, index, .. } => Some(MInst::Alloc {
                    array: mir_node_expr(array),
                    size: mir_val_expr(&index.range.bound.val),
                    ty: mir_expr_ty(&node.ty),
                }),
                _ => None,
            })
            .collect(),
        _ => vec![],
    }
}

fn mir_exec_insts(hir: &Rc<HStmt>) -> Vec<MInst> {
    match hir.deref() {
        HStmt::Block { stmts } => stmts.iter().flat_map(mir_insts).collect(),
        HStmt::Read { args, .. } => args
            .iter()
            .map(Deref::deref)
            .map(|HDataAtom { ty, node, .. }| MInst::Read {
                ty: mir_atom_ty(ty),
                arg: mir_node_expr(node),
            })
            .collect(),
        HStmt::Write { args, .. } => args
            .iter()
            .map(|atom| MInst::Write {
                ty: mir_atom_ty(&atom.ty),
                arg: mir_val_expr(&atom.val),
            })
            .collect(),
        HStmt::Call { fun, .. } => vec![MInst::Call {
            name: fun.name.to_string(),
            args: fun.args.iter().map(|a| &a.val).map(mir_val_expr).collect(),
            ret: fun
                .ret
                .as_ref()
                .map(Deref::deref)
                .map(|HDataAtom { node, .. }| mir_node_expr(node)),
        }],
        HStmt::For { range, body, .. } => vec![MInst::For {
            index_name: range.index.to_string(),
            bound: mir_val_expr(&range.bound.val),
            body: Box::new(mir_exec_insts(body)),
        }],
    }
}

fn mir_node_expr(hir: &Rc<HDataNode>) -> MExpr {
    match hir.expr.deref() {
        HDataExpr::Var { var, .. } => MExpr::Var {
            name: match var.expr.deref() {
                HDataVarExpr::Name { name } => name.to_string(),
                _ => unreachable!(),
            },
        },
        HDataExpr::Subscript { array, index, .. } => MExpr::Subscript {
            array: Box::new(mir_node_expr(array)),
            index: Box::new(mir_index_expr(index)),
        },
        HDataExpr::Err => unreachable!(),
    }
}

fn mir_val_expr(hir: &Rc<HVal>) -> MExpr {
    match hir.expr.deref() {
        HValExpr::Var { ident, .. } => MExpr::Var {
            name: ident.to_string(),
        },
        HValExpr::Subscript { array, index, .. } => MExpr::Subscript {
            array: Box::new(mir_val_expr(array)),
            index: Box::new(mir_val_expr(index)),
        },
    }
}

fn mir_index_expr(hir: &Rc<HIndex>) -> MExpr {
    MExpr::Var {
        name: hir.name.to_string(),
    }
}

fn mir_atom_ty(hir: &Rc<HAtomTy>) -> AtomTy {
    hir.sem
}

fn mir_expr_ty(hir: &Rc<HValTy>) -> MExprTy {
    let hir: &HValTy = hir.deref();

    match hir {
        HValTy::Atom { atom_ty } => MExprTy::Atom {
            atom: mir_atom_ty(atom_ty),
        },
        HValTy::Array { item, .. } => MExprTy::Array {
            item: Box::new(mir_expr_ty(item)),
        },
        HValTy::Err => unreachable!(),
    }
}

fn mir_fun(hir: &Rc<HFun>) -> MFun {
    MFun {
        name: hir.name.to_string(),
        params: hir.args.iter().map(mir_param).collect(),
        ret: hir.ret.as_ref().map(|r| mir_atom_ty(&r.ty)),
    }
}

fn mir_param(hir: &Rc<HArg>) -> MParam {
    MParam {
        name: hir.name.to_string(),
        ty: mir_expr_ty(&hir.ty),
    }
}
