//! Build LIR from HIR.

use std::marker::PhantomData;

use crate::atom::*;
use crate::code::lir::*;
use crate::spec::hir::*;
use crate::spec::hir_sem::*;
use crate::spec::rel::RelOp;

trait LirFrom<T, C: LirConfig> {
    fn lir_from(hir: &T, config: &C) -> Self;
}

trait LirInto<U, C: LirConfig> {
    fn lir(self: &Self, config: &C) -> Lir<C, U>;
}

impl<T, U: LirFrom<T, C>, C: LirConfig> LirInto<U, C> for T {
    fn lir(self: &Self, config: &C) -> Lir<C, U> {
        Lir {
            config: config.clone(),
            code: Rc::new(U::lir_from(self, config)),
        }
    }
}

trait LirFromRc<T, C: LirConfig> {
    fn lir_from(hir: &T, config: &C) -> Self;
}

impl<T, U: LirFromRc<T, C>, C: LirConfig> LirFrom<Rc<T>, C> for U {
    fn lir_from(hir: &Rc<T>, config: &C) -> Self {
        <U as LirFromRc<T, C>>::lir_from(hir.as_ref(), config)
    }
}

impl<C: LirConfig> LirFromRc<HSpec, C> for LSpec<C> {
    fn lir_from(hir: &HSpec, config: &C) -> Self {
        LSpec {
            funs: hir.main.funs.iter().map(|f| f.lir(config)).collect(),
            main: hir.main.lir(config),
        }
    }
}

impl<C: LirConfig> LirFromRc<HStep, C> for LBlock<C> {
    fn lir_from(hir: &HStep, config: &C) -> Self {
        LBlock {
            stmts: match &hir.expr {
                HStepExpr::Seq { steps } => steps.iter().map(|x| x.lir(config)).collect(),
                _ => unreachable!(),
            },
        }
    }
}

impl<C: LirConfig> LirFromRc<HStep, C> for LStmt<C> {
    fn lir_from(hir: &HStep, config: &C) -> Self {
        match &hir.expr {
            HStepExpr::Seq { .. } => unreachable!(),
            HStepExpr::Read { args, .. } => LStmt::Read {
                args: args.iter().map(|x| x.lir(config)).collect(),
            },
            HStepExpr::Write { args, .. } => LStmt::Write {
                args: args.iter().map(|x| x.lir(config)).collect(),
            },
            HStepExpr::Call { fun, .. } => LStmt::Call {
                decl: fun
                    .ret
                    .as_ref()
                    .and_then(|ret| node_decl(&ret.node).map(|h| h.lir(config))),
                name: fun.name.to_string(),
                args: fun.args.iter().map(|a| a.val.lir(config)).collect(),
                ret: fun.ret.as_ref().map(|a| a.node.lir(config)),
            },
            HStepExpr::For { range, body, .. } => LStmt::For {
                allocs: hir
                    .nodes
                    .iter()
                    .flat_map(node_alloc)
                    .map(|h| h.lir(config))
                    .collect(),
                index: range.lir(config),
                index_ty: range.bound.ty.lir(config),
                bound: range.bound.val.lir(config),
                body: body.lir(config),
            },
            HStepExpr::Assume { cond, .. } => LStmt::Assume {
                cond: cond.val.lir(config),
            },
        }
    }
}

impl<C: LirConfig> LirFromRc<HAtomDef, C> for LReadArg<C> {
    fn lir_from(atom: &HAtomDef, config: &C) -> Self {
        LReadArg {
            decl: node_decl(&atom.node).map(|h| h.lir(config)),
            expr: atom.node.lir(config),
            ty: atom.ty.lir(config),
        }
    }
}

impl<C: LirConfig> LirFromRc<HAtom, C> for LWriteArg<C> {
    fn lir_from(atom: &HAtom, config: &C) -> Self {
        LWriteArg {
            ty: atom.ty.lir(config),
            expr: atom.val.lir(config),
        }
    }
}

impl<C: LirConfig> LirFromRc<HVarDef, C> for LDecl<C> {
    fn lir_from(var: &HVarDef, config: &C) -> Self {
        LDecl {
            name: match &var.expr {
                HVarDefExpr::Name { name } => name.to_string(),
                _ => unreachable!(),
            },
            ty: var.ty.lir(config),
        }
    }
}

impl<C: LirConfig> LirFromRc<HRange, C> for LDecl<C> {
    fn lir_from(range: &HRange, config: &C) -> Self {
        LDecl {
            name: range.index.to_string(),
            ty: range.bound.val.ty.lir(config),
        }
    }
}

impl<C: LirConfig> LirFrom<HAlloc, C> for LAlloc<C> {
    fn lir_from(alloc: &HAlloc, config: &C) -> Self {
        LAlloc {
            decl: node_decl(&alloc.array).map(|h| h.lir(config)),
            array: alloc.array.lir(config),
            item_ty: alloc.item_ty.lir(config),
            size: alloc.size.lir(config),
        }
    }
}

impl<C: LirConfig> LirFromRc<HNodeDef, C> for LExpr<C> {
    fn lir_from(node: &HNodeDef, config: &C) -> Self {
        match &node.expr {
            HNodeDefExpr::Var { var, .. } => LExpr::Var {
                name: match &var.expr {
                    HVarDefExpr::Name { name } => name.to_string(),
                    _ => unreachable!(),
                },
            },
            HNodeDefExpr::Subscript { array, index, .. } => LExpr::Subscript {
                array: Box::new(array.lir(config)),
                index: Box::new(index.lir(config)),
            },
            HNodeDefExpr::Err => unreachable!(),
        }
    }
}

impl<C: LirConfig> LirFromRc<HVal, C> for LExpr<C> {
    fn lir_from(val: &HVal, config: &C) -> Self {
        match &val.expr {
            HValExpr::Var { name, .. } => LExpr::Var {
                name: name.to_string(),
            },
            HValExpr::Subscript { array, index, .. } => LExpr::Subscript {
                array: Box::new(array.lir(config)),
                index: Box::new(index.lir(config)),
            },
            HValExpr::Lit { value, .. } => LExpr::Lit {
                value: value.value_i64(),
            },
            HValExpr::Paren { inner, .. } => LExpr::Paren {
                inner: Box::new(inner.lir(config)),
            },
            HValExpr::Mul { factors, .. } => LExpr::Mul {
                factors: factors.iter().map(|f| f.val.lir(config)).collect(),
            },
            HValExpr::Sum { terms, .. } => LExpr::Sum {
                terms: terms
                    .iter()
                    .map(|(sign, t)| (sign.lir(config), t.val.lir(config)))
                    .collect(),
            },
            HValExpr::RelChain { rels } => LExpr::And {
                clauses: rels.iter().map(|x| x.lir(config)).collect(),
            },
            HValExpr::Err => unreachable!(),
        }
    }
}

impl<C: LirConfig> LirFrom<HSign, C> for Option<LSign<C>> {
    fn lir_from(sign: &HSign, _config: &C) -> Self {
        match sign {
            HSign::Plus(None) => None,
            HSign::Plus(Some(_)) => Some(LSign::Plus(PhantomData)),
            HSign::Minus(_) => Some(LSign::Minus(PhantomData)),
        }
    }
}

impl<C: LirConfig> LirFromRc<HAtomTy, C> for AtomTy {
    fn lir_from(ty: &HAtomTy, _config: &C) -> Self {
        ty.sem.unwrap()
    }
}

impl<C: LirConfig> LirFromRc<HValTy, C> for LTy<C> {
    fn lir_from(ty: &HValTy, config: &C) -> Self {
        match ty {
            HValTy::Atom { atom_ty } => LTy::Atom {
                atom: atom_ty.lir(config),
            },
            HValTy::Array { item, .. } => LTy::Array {
                item: Box::new(item.lir(config)),
            },
            HValTy::Err => unreachable!(),
        }
    }
}

impl<C: LirConfig> LirFromRc<HFun, C> for LFun<C> {
    fn lir_from(fun: &HFun, config: &C) -> Self {
        LFun {
            name: fun.name.to_string(),
            params: fun.args.iter().map(|p| p.lir(config)).collect(),
            ret: fun.ret.as_ref().map(|r| r.ty.lir(config)),
        }
    }
}

impl<C: LirConfig> LirFrom<HRel, C> for LExpr<C> {
    fn lir_from(rel: &HRel, config: &C) -> Self {
        let (left, op, right) = rel;
        LExpr::Rel {
            left: Box::new(left.val.lir(config)),
            op: op.lir(config),
            right: Box::new(right.val.lir(config)),
        }
    }
}

impl<C: LirConfig> LirFrom<RelOp, C> for RelOp {
    fn lir_from(op: &RelOp, _config: &C) -> Self {
        op.clone()
    }
}

impl<C: LirConfig> LirFromRc<HArg, C> for LParam<C> {
    fn lir_from(arg: &HArg, config: &C) -> Self {
        LParam {
            name: match &arg.expr {
                HArgExpr::Name { name } => name.to_string(),
                HArgExpr::Err => unreachable!(),
            },
            ty: arg.val.ty.lir(config),
        }
    }
}

pub fn build_lir<C: LirConfig>(spec: &Rc<HSpec>, config: &C) -> Lir<C, LSpec<C>> {
    spec.lir(config)
}
