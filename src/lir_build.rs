//! Build MIR from HIR.

use std::ops::Deref;

use crate::hir::*;
use crate::lir::*;
use crate::mir::*;
use crate::ty::*;

pub fn build_lir(spec: MSpec) -> LSpec {
    LSpec {
        funs: spec.hir.funs.iter().map(lir_fun).collect(),
        main: lir_block(spec.main),
    }
}

fn lir_block(insts: Vec<MInst>) -> LBlock {
    LBlock {
        stmts: insts.into_iter().map(lir_stmt).collect(),
    }
}

fn lir_stmt(inst: MInst) -> LStmt {
    match inst {
        MInst::Decl(var) => match var.expr.deref() {
            HVarDefExpr::Name { name } => LStmt::Decl {
                name: name.to_string(),
                ty: lir_expr_ty(&var.ty),
            },
            _ => unreachable!(),
        },
        MInst::Alloc { array, size, ty } => LStmt::Alloc {
            array: lir_data_node_expr(&array),
            size: lir_val_expr(&size),
            ty: lir_expr_ty(&ty),
        },
        MInst::Read(atom) => LStmt::Read {
            ty: lir_atom_ty(&atom.ty),
            arg: lir_data_node_expr(&atom.node),
        },
        MInst::Write(atom) => LStmt::Write {
            ty: lir_atom_ty(&atom.ty),
            arg: lir_val_expr(&atom.val),
        },
        MInst::Call(fun) => LStmt::Call {
            name: fun.name.to_string(),
            args: fun.args.iter().map(|a| &a.val).map(lir_val_expr).collect(),
            ret: fun
                .ret
                .as_ref()
                .map(Deref::deref)
                .map(|a| lir_data_node_expr(&a.node)),
        },
        MInst::For { range, body } => LStmt::For {
            index_name: range.index.to_string(),
            bound: lir_val_expr(&range.bound.val),
            body: lir_block(body),
        },
    }
}

fn lir_data_node_expr(hir: &Rc<HNodeDef>) -> LExpr {
    match hir.expr.deref() {
        HNodeDefExpr::Var { var, .. } => LExpr::Var {
            name: match var.expr.deref() {
                HVarDefExpr::Name { name } => name.to_string(),
                _ => unreachable!(),
            },
        },
        HNodeDefExpr::Subscript { array, index, .. } => LExpr::Subscript {
            array: Box::new(lir_data_node_expr(array)),
            index: Box::new(lir_val_expr(index)),
        },
        HNodeDefExpr::Err => unreachable!(),
    }
}

fn lir_val_expr(hir: &Rc<HVal>) -> LExpr {
    match hir.expr.deref() {
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
        name: match hir.expr.deref() {
            HArgExpr::Name { name } => name.to_string(),
            HArgExpr::Err => unreachable!(),
        },
        ty: lir_expr_ty(&hir.val.ty),
    }
}
