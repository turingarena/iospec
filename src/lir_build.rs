//! Build LIR from HIR.

use std::ops::Deref;

use crate::hir::*;
use crate::hir_sem::*;
use crate::lir::*;
use crate::ty::*;

pub fn build_lir(spec: Rc<HSpec>) -> LSpec {
    LSpec {
        funs: spec.main.funs.iter().map(lir_fun).collect(),
        main: lir_block(&spec.main),
    }
}

fn lir_block(step: &Rc<HStep>) -> LBlock {
    LBlock {
        stmts: lir_stmts(step),
    }
}

fn lir_stmts(step: &Rc<HStep>) -> Vec<LStmt> {
    match &step.expr {
        HStepExpr::Seq { steps } => steps.iter().flat_map(lir_stmts).collect(),
        HStepExpr::Read { args, .. } => args
            .iter()
            .flat_map(|atom| {
                std::iter::empty()
                    .chain(node_decl(&atom.node).map(lir_decl))
                    .chain(node_alloc(&atom.node).map(lir_alloc))
                    .chain(std::iter::once(LStmt::Read {
                        ty: lir_atom_ty(&atom.ty),
                        arg: lir_def_expr(&atom.node),
                    }))
            })
            .collect(),
        HStepExpr::Write { args, .. } => args
            .iter()
            .cloned()
            .map(|atom| LStmt::Write {
                ty: lir_atom_ty(&atom.ty),
                arg: lir_val_expr(&atom.val),
            })
            .collect(),
        HStepExpr::Call { fun, .. } => fun
            .ret
            .iter()
            .flat_map(|ret| node_decl(&ret.node).map(lir_decl))
            .chain(std::iter::once(LStmt::Call {
                name: fun.name.to_string(),
                args: fun.args.iter().map(|a| &a.val).map(lir_val_expr).collect(),
                ret: fun
                    .ret
                    .as_ref()
                    .map(Deref::deref)
                    .map(|a| lir_def_expr(&a.node)),
            }))
            .collect(),
        HStepExpr::For { range, body, .. } => std::iter::empty()
            .chain(step.nodes.iter().flat_map(|node| {
                std::iter::empty()
                    .chain(node_decl(&node).map(lir_decl))
                    .chain(node_alloc(&node).map(lir_alloc))
            }))
            .chain(std::iter::once(LStmt::For {
                index_name: range.index.to_string(),
                bound: lir_val_expr(&range.bound.val),
                body: lir_block(body),
            }))
            .collect(),
    }
}

fn lir_alloc(alloc: HAlloc) -> LStmt {
    LStmt::Alloc {
        array: lir_def_expr(&alloc.array),
        item_ty: lir_expr_ty(&alloc.item_ty),
        size: lir_val_expr(&alloc.size),
    }
}

fn lir_decl(var: Rc<HVarDef>) -> LStmt {
    LStmt::Decl {
        name: match &var.expr {
            HVarDefExpr::Name { name } => name.to_string(),
            _ => unreachable!(),
        },
        ty: lir_expr_ty(&var.ty),
    }
}

fn lir_def_expr(hir: &Rc<HNodeDef>) -> LExpr {
    match &hir.expr {
        HNodeDefExpr::Var { var, .. } => LExpr::Var {
            name: match &var.expr {
                HVarDefExpr::Name { name } => name.to_string(),
                _ => unreachable!(),
            },
        },
        HNodeDefExpr::Subscript { array, index, .. } => LExpr::Subscript {
            array: Box::new(lir_def_expr(array)),
            index: Box::new(lir_val_expr(index)),
        },
        HNodeDefExpr::Err => unreachable!(),
    }
}

fn lir_val_expr(hir: &Rc<HVal>) -> LExpr {
    match &hir.expr {
        HValExpr::Var { name, .. } => LExpr::Var {
            name: name.to_string(),
        },
        HValExpr::Subscript { array, index, .. } => LExpr::Subscript {
            array: Box::new(lir_val_expr(array)),
            index: Box::new(lir_val_expr(index)),
        },
    }
}

fn lir_atom_ty(hir: &Rc<HAtomTy>) -> AtomTy {
    hir.sem
}

fn lir_expr_ty(hir: &Rc<HValTy>) -> LTy {
    let hir: &HValTy = hir.deref();

    match hir {
        HValTy::Atom { atom_ty } => LTy::Atom {
            atom: lir_atom_ty(atom_ty),
        },
        HValTy::Array { item, .. } => LTy::Array {
            item: Box::new(lir_expr_ty(item)),
        },
        HValTy::Err => unreachable!(),
    }
}

fn lir_fun(hir: &Rc<HFun>) -> LFun {
    LFun {
        name: hir.name.to_string(),
        params: hir.args.iter().map(lir_param).collect(),
        ret: hir.ret.as_ref().map(|r| lir_atom_ty(&r.ty)),
    }
}

fn lir_param(hir: &Rc<HArg>) -> LParam {
    LParam {
        name: match &hir.expr {
            HArgExpr::Name { name } => name.to_string(),
            HArgExpr::Err => unreachable!(),
        },
        ty: lir_expr_ty(&hir.val.ty),
    }
}
