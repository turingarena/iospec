//! Build LIR from HIR.

use std::marker::PhantomData;

use crate::atom::*;
use crate::code::lir::*;
use crate::spec::hir::*;
use crate::spec::hir_sem::*;

trait LirFrom<T> {
    fn lir_from(hir: &T) -> Self;
}

trait LirInto<U, F: LirFlavor> {
    fn lir(self: &Self) -> Lir<F, U>;
}

impl<T, U: LirFrom<T>, F: LirFlavor> LirInto<U, F> for T {
    fn lir(self: &Self) -> Lir<F, U> {
        U::lir_from(self).into()
    }
}

trait LirFromRc<T> {
    fn lir_from(hir: &T) -> Self;
}

impl<T, U: LirFromRc<T>> LirFrom<Rc<T>> for U {
    fn lir_from(hir: &Rc<T>) -> Self {
        <U as LirFromRc<T>>::lir_from(hir.as_ref())
    }
}

impl<F: LirFlavor> LirFromRc<HSpec> for LSpec<F> {
    fn lir_from(hir: &HSpec) -> Self {
        LSpec {
            funs: hir.main.funs.iter().map(|f| f.lir()).collect(),
            main: hir.main.lir(),
        }
    }
}

impl<F: LirFlavor> LirFromRc<HStep> for LBlock<F> {
    fn lir_from(hir: &HStep) -> Self {
        LBlock {
            stmts: match &hir.expr {
                HStepExpr::Seq { steps } => steps.iter().map(LirInto::lir).collect(),
                _ => unreachable!(),
            },
        }
    }
}

impl<F: LirFlavor> LirFromRc<HStep> for LStmt<F> {
    fn lir_from(hir: &HStep) -> Self {
        match &hir.expr {
            HStepExpr::Seq { .. } => unreachable!(),
            HStepExpr::Read { args, .. } => LStmt::Read {
                args: args.iter().map(LirInto::lir).collect(),
            },
            HStepExpr::Write { args, .. } => LStmt::Write {
                args: args.iter().map(LirInto::lir).collect(),
            },
            HStepExpr::Call { fun, .. } => LStmt::Call {
                decl: fun
                    .ret
                    .as_ref()
                    .and_then(|ret| node_decl(&ret.node).map(|h| h.lir())),
                name: fun.name.to_string(),
                args: fun.args.iter().map(|a| a.val.lir()).collect(),
                ret: fun.ret.as_ref().map(|a| a.node.lir()),
            },
            HStepExpr::For { range, body, .. } => LStmt::For {
                allocs: hir
                    .nodes
                    .iter()
                    .flat_map(node_alloc)
                    .map(|h| h.lir())
                    .collect(),
                index: range.lir(),
                index_ty: range.bound.ty.lir(),
                bound: range.bound.val.lir(),
                body: body.lir(),
            },
            HStepExpr::Assume { cond, .. } => LStmt::Assume {
                cond: cond.val.lir(),
            },
        }
    }
}

impl<F: LirFlavor> LirFromRc<HAtomDef> for LReadArg<F> {
    fn lir_from(atom: &HAtomDef) -> Self {
        LReadArg {
            decl: node_decl(&atom.node).map(|h| h.lir()),
            expr: atom.node.lir(),
            ty: atom.ty.lir(),
        }
    }
}

impl<F: LirFlavor> LirFromRc<HAtom> for LWriteArg<F> {
    fn lir_from(atom: &HAtom) -> Self {
        LWriteArg {
            ty: atom.ty.lir(),
            expr: atom.val.lir(),
        }
    }
}

impl<F: LirFlavor> LirFromRc<HVarDef> for LDecl<F> {
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

impl<F: LirFlavor> LirFromRc<HRange> for LDecl<F> {
    fn lir_from(range: &HRange) -> Self {
        LDecl {
            name: range.index.to_string(),
            ty: range.bound.val.ty.lir(),
        }
    }
}

impl<F: LirFlavor> LirFrom<HAlloc> for LAlloc<F> {
    fn lir_from(alloc: &HAlloc) -> Self {
        LAlloc {
            decl: node_decl(&alloc.array).map(|h| h.lir()),
            array: alloc.array.lir(),
            item_ty: alloc.item_ty.lir(),
            size: alloc.size.lir(),
        }
    }
}

impl<F: LirFlavor> LirFromRc<HNodeDef> for LExpr<F> {
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

impl<F: LirFlavor> LirFromRc<HVal> for LExpr<F> {
    fn lir_from(val: &HVal) -> Self {
        match &val.expr {
            HValExpr::Var { name, .. } => LExpr::Var {
                name: name.to_string(),
            },
            HValExpr::Subscript { array, index, .. } => LExpr::Subscript {
                array: Box::new(array.lir()),
                index: Box::new(index.lir()),
            },
            HValExpr::Lit { value, .. } => LExpr::Lit {
                value: value.value_i64(),
            },
            HValExpr::Paren { inner, .. } => LExpr::Paren {
                inner: Box::new(inner.lir()),
            },
            HValExpr::Mul { factors, .. } => LExpr::Mul {
                factors: factors.iter().map(|f| f.val.lir()).collect(),
            },
            HValExpr::Sum { terms, .. } => LExpr::Sum {
                terms: terms
                    .iter()
                    .map(|(sign, t)| (sign.lir(), t.val.lir()))
                    .collect(),
            },
            HValExpr::RelChain { rels } => LExpr::And {
                clauses: rels.iter().map(LirInto::lir).collect(),
            },
            HValExpr::Err => unreachable!(),
        }
    }
}

impl<F: LirFlavor> LirFrom<HSign> for Option<LSign<F>> {
    fn lir_from(sign: &HSign) -> Self {
        match sign {
            HSign::Plus(None) => None,
            HSign::Plus(Some(_)) => Some(LSign::Plus(PhantomData)),
            HSign::Minus(_) => Some(LSign::Minus(PhantomData)),
        }
    }
}

impl LirFromRc<HAtomTy> for AtomTy {
    fn lir_from(ty: &HAtomTy) -> Self {
        ty.sem.unwrap()
    }
}

impl<F: LirFlavor> LirFromRc<HValTy> for LTy<F> {
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

impl<F: LirFlavor> LirFromRc<HFun> for LFun<F> {
    fn lir_from(fun: &HFun) -> Self {
        LFun {
            name: fun.name.to_string(),
            params: fun.args.iter().map(|p| p.lir()).collect(),
            ret: fun.ret.as_ref().map(|r| r.ty.lir()),
        }
    }
}

impl<F: LirFlavor> LirFrom<HRel> for LExpr<F> {
    fn lir_from(rel: &HRel) -> Self {
        let (left, op, right) = rel;
        LExpr::Rel {
            left: Box::new(left.val.lir()),
            op: Lir::from(op.clone()),
            right: Box::new(right.val.lir()),
        }
    }
}

impl<F: LirFlavor> LirFromRc<HArg> for LParam<F> {
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

pub fn build_lir<F: LirFlavor>(spec: &Rc<HSpec>) -> Lir<F, LSpec<F>> {
    spec.lir()
}
