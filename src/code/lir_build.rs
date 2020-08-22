//! Build LIR from HIR.

use std::ops::Deref;

use crate::code::lir::*;
use crate::spec::hir::*;
use crate::spec::hir_sem::*;
use crate::spec::ty::*;

trait LirFrom<T> {
    fn lir_from(hir: &T) -> Self;
}

impl<T, U: LirFrom<T>> LirFrom<Rc<T>> for U {
    fn lir_from(hir: &Rc<T>) -> Self {
        U::lir_from(hir.deref())
    }
}

trait LirInto<U> {
    fn lir(self: &Self) -> U;
}

impl<T, U: LirFrom<T>> LirInto<U> for T {
    fn lir(self: &Self) -> U {
        U::lir_from(self)
    }
}

impl LirFrom<HSpec> for LSpec {
    fn lir_from(hir: &HSpec) -> Self {
        LSpec {
            funs: hir.main.funs.iter().map(|f| f.lir()).collect(),
            main: hir.main.lir(),
        }
    }
}

impl LirFrom<HStep> for LBlock {
    fn lir_from(hir: &HStep) -> Self {
        LBlock { stmts: hir.lir() }
    }
}

impl LirFrom<HStep> for Vec<LStmt> {
    fn lir_from(hir: &HStep) -> Self {
        match &hir.expr {
            HStepExpr::Seq { steps } => steps
                .iter()
                .flat_map::<Vec<LStmt>, _>(LirInto::lir)
                .collect(),
            HStepExpr::Read { args, .. } => vec![LStmt::Read {
                args: args
                    .iter()
                    .map(|atom| LReadArg {
                        decl: node_decl(&atom.node).map(|h| h.lir()),
                        expr: atom.node.lir(),
                        ty: atom.ty.lir(),
                    })
                    .collect(),
            }],
            HStepExpr::Write { args, .. } => vec![LStmt::Write {
                args: args
                    .iter()
                    .cloned()
                    .map(|atom| LWriteArg {
                        ty: atom.ty.lir(),
                        expr: atom.val.lir(),
                    })
                    .collect(),
            }],
            HStepExpr::Call { fun, .. } => vec![LStmt::Call {
                decl: fun
                    .ret
                    .as_ref()
                    .and_then(|ret| node_decl(&ret.node).map(|h| h.lir())),
                name: fun.name.to_string(),
                args: fun.args.iter().map(|a| a.val.lir()).collect(),
                ret: fun.ret.as_ref().map(|a| a.node.lir()),
            }],
            HStepExpr::For { range, body, .. } => vec![LStmt::For {
                allocs: hir
                    .nodes
                    .iter()
                    .flat_map(node_alloc)
                    .map(|h| h.lir())
                    .collect(),
                index: LDecl {
                    name: range.index.to_string(),
                    ty: range.bound.val.ty.lir(),
                },
                index_ty: range.bound.ty.lir(),
                bound: range.bound.val.lir(),
                body: body.lir(),
            }],
        }
    }
}

impl LirFrom<HVarDef> for LDecl {
    fn lir_from(var: &HVarDef) -> Self {
        LDecl {
            name: match &var.expr {
                HVarDefExpr::Name { name } => name.to_string(),
                _ => unreachable!(),
            },
            ty: var.ty.lir(),
        }
    }
}

impl LirFrom<HAlloc> for LAlloc {
    fn lir_from(alloc: &HAlloc) -> Self {
        LAlloc {
            decl: node_decl(&alloc.array).map(|h| h.lir()),
            array: alloc.array.lir(),
            item_ty: alloc.item_ty.lir(),
            size: alloc.size.lir(),
        }
    }
}

impl LirFrom<HNodeDef> for LExpr {
    fn lir_from(node: &HNodeDef) -> Self {
        match &node.expr {
            HNodeDefExpr::Var { var, .. } => LExpr::Var {
                name: match &var.expr {
                    HVarDefExpr::Name { name } => name.to_string(),
                    _ => unreachable!(),
                },
            },
            HNodeDefExpr::Subscript { array, index, .. } => LExpr::Subscript {
                array: Box::new(array.lir()),
                index: Box::new(index.lir()),
            },
            HNodeDefExpr::Err => unreachable!(),
        }
    }
}

impl LirFrom<HVal> for LExpr {
    fn lir_from(val: &HVal) -> Self {
        match &val.expr {
            HValExpr::Var { name, .. } => LExpr::Var {
                name: name.to_string(),
            },
            HValExpr::Subscript { array, index, .. } => LExpr::Subscript {
                array: Box::new(array.lir()),
                index: Box::new(index.lir()),
            },
        }
    }
}

impl LirFrom<HAtomTy> for AtomTy {
    fn lir_from(ty: &HAtomTy) -> Self {
        ty.sem
    }
}

impl LirFrom<HValTy> for LTy {
    fn lir_from(ty: &HValTy) -> Self {
        match ty {
            HValTy::Atom { atom_ty } => LTy::Atom {
                atom: atom_ty.lir(),
            },
            HValTy::Array { item, .. } => LTy::Array {
                item: Box::new(item.lir()),
            },
            HValTy::Err => unreachable!(),
        }
    }
}

impl LirFrom<HFun> for LFun {
    fn lir_from(fun: &HFun) -> Self {
        LFun {
            name: fun.name.to_string(),
            params: fun.args.iter().map(|p| p.lir()).collect(),
            ret: fun.ret.as_ref().map(|r| r.ty.lir()),
        }
    }
}

impl LirFrom<HArg> for LParam {
    fn lir_from(arg: &HArg) -> Self {
        LParam {
            name: match &arg.expr {
                HArgExpr::Name { name } => name.to_string(),
                HArgExpr::Err => unreachable!(),
            },
            ty: arg.val.ty.lir(),
        }
    }
}

pub fn build_lir(spec: &Rc<HSpec>) -> LSpec {
    spec.lir()
}
