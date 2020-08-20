//! Build MIR from HIR.

use std::ops::Deref;

use crate::hir::*;
use crate::lir::*;
use crate::mir::*;
use crate::ty::*;

pub fn build_lir(spec: MSpec) -> LSpec {
    LSpec {
        funs: spec.hir.funs.iter().map(mir_fun).collect(),
        main: lir_block(spec.main),
    }
}

fn lir_block(insts: Vec<MInst>) -> Vec<LStmt> {
    insts.into_iter().map(lir_inst).collect()
}

fn lir_inst(inst: MInst) -> LStmt {
    match inst {
        MInst::Decl(var) => match var.expr.deref() {
            HDataVarExpr::Name { name } => LStmt::Decl {
                name: name.to_string(),
                ty: mir_expr_ty(&var.ty),
            },
            _ => unreachable!(),
        },
        MInst::Alloc { array, size, ty } => LStmt::Alloc {
            array: mir_node_expr(&array),
            size: mir_val_expr(&size),
            ty: mir_expr_ty(&ty),
        },
        MInst::Read(atom) => LStmt::Read {
            ty: mir_atom_ty(&atom.ty),
            arg: mir_node_expr(&atom.node),
        },
        MInst::Write(atom) => LStmt::Write {
            ty: mir_atom_ty(&atom.ty),
            arg: mir_val_expr(&atom.val),
        },
        MInst::Call(fun) => LStmt::Call {
            name: fun.name.to_string(),
            args: fun.args.iter().map(|a| &a.val).map(mir_val_expr).collect(),
            ret: fun
                .ret
                .as_ref()
                .map(Deref::deref)
                .map(|a| mir_node_expr(&a.node)),
        },
        MInst::For { range, body } => LStmt::For {
            index_name: range.index.to_string(),
            bound: mir_val_expr(&range.bound.val),
            body: lir_block(body),
        },
    }
}

fn mir_node_expr(hir: &Rc<HDataNode>) -> LExpr {
    match hir.expr.deref() {
        HDataExpr::Var { var, .. } => LExpr::Var {
            name: match var.expr.deref() {
                HDataVarExpr::Name { name } => name.to_string(),
                _ => unreachable!(),
            },
        },
        HDataExpr::Subscript { array, index, .. } => LExpr::Subscript {
            array: Box::new(mir_node_expr(array)),
            index: Box::new(mir_index_expr(index)),
        },
        HDataExpr::Err => unreachable!(),
    }
}

fn mir_val_expr(hir: &Rc<HVal>) -> LExpr {
    match hir.expr.deref() {
        HValExpr::Var { ident, .. } => LExpr::Var {
            name: ident.to_string(),
        },
        HValExpr::Subscript { array, index, .. } => LExpr::Subscript {
            array: Box::new(mir_val_expr(array)),
            index: Box::new(mir_val_expr(index)),
        },
    }
}

fn mir_index_expr(hir: &Rc<HIndex>) -> LExpr {
    LExpr::Var {
        name: hir.name.to_string(),
    }
}

fn mir_atom_ty(hir: &Rc<HAtomTy>) -> AtomTy {
    hir.sem
}

fn mir_expr_ty(hir: &Rc<HValTy>) -> LTy {
    let hir: &HValTy = hir.deref();

    match hir {
        HValTy::Atom { atom_ty } => LTy::Atom {
            atom: mir_atom_ty(atom_ty),
        },
        HValTy::Array { item, .. } => LTy::Array {
            item: Box::new(mir_expr_ty(item)),
        },
        HValTy::Err => unreachable!(),
    }
}

fn mir_fun(hir: &Rc<HFun>) -> LFun {
    LFun {
        name: hir.name.to_string(),
        params: hir.args.iter().map(mir_param).collect(),
        ret: hir.ret.as_ref().map(|r| mir_atom_ty(&r.ty)),
    }
}

fn mir_param(hir: &Rc<HArg>) -> LParam {
    LParam {
        name: match hir.expr.deref() {
            HArgExpr::Name { name } => name.to_string(),
            HArgExpr::Err => unreachable!(),
        },
        ty: mir_expr_ty(&hir.ty),
    }
}
