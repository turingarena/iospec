//! Build LIR from HIR.

use crate::atom::*;
use crate::code::lir::*;
use crate::spec::hir::*;
use crate::spec::hir_sem::*;
use crate::spec::rel::RelOp;

trait LirFrom<T, L: CodeLang> {
    fn lir_from(hir: &T, lang: &L) -> Self;
}

trait LirInto<U, L: CodeLang> {
    fn lir(self: &Self, lang: &L) -> Lir<L, U>;
}

impl<T, U: LirFrom<T, L>, L: CodeLang> LirInto<U, L> for T {
    fn lir(self: &Self, lang: &L) -> Lir<L, U> {
        Lir {
            lang: lang.clone(),
            code: Rc::new(U::lir_from(self, lang)),
        }
    }
}

trait LirFromRc<T, L: CodeLang> {
    fn lir_from(hir: &T, lang: &L) -> Self;
}

impl<T, U: LirFromRc<T, L>, L: CodeLang> LirFrom<Rc<T>, L> for U {
    fn lir_from(hir: &Rc<T>, lang: &L) -> Self {
        <U as LirFromRc<T, L>>::lir_from(hir.as_ref(), lang)
    }
}

impl<L: CodeLang> LirFromRc<HSpec, L> for LSpec<L> {
    fn lir_from(hir: &HSpec, lang: &L) -> Self {
        LSpec {
            funs: hir.main.funs.iter().map(|f| f.lir(lang)).collect(),
            main: hir.main.lir(lang),
        }
    }
}

impl<L: CodeLang> LirFromRc<HStep, L> for LBlock<L> {
    fn lir_from(hir: &HStep, lang: &L) -> Self {
        LBlock {
            stmts: match &hir.expr {
                HStepExpr::Seq { steps } => steps.iter().map(|x| x.lir(lang)).collect(),
                _ => unreachable!(),
            },
        }
    }
}

impl<L: CodeLang> LirFromRc<HStep, L> for LStmt<L> {
    fn lir_from(hir: &HStep, lang: &L) -> Self {
        match &hir.expr {
            HStepExpr::Seq { .. } => unreachable!(),
            HStepExpr::Read { args, .. } => LStmt::Read {
                args: args.iter().map(|x| x.lir(lang)).collect(),
            },
            HStepExpr::Write { args, .. } => LStmt::Write {
                args: args.iter().map(|x| x.lir(lang)).collect(),
            },
            HStepExpr::Call { fun, .. } => LStmt::Call {
                decl: fun
                    .ret
                    .as_ref()
                    .and_then(|ret| node_decl(&ret.node).map(|h| h.lir(lang))),
                name: fun.name.to_string(),
                args: fun.args.iter().map(|a| a.val.lir(lang)).collect(),
                ret: fun.ret.as_ref().map(|a| a.node.lir(lang)),
            },
            HStepExpr::For { range, body, .. } => LStmt::For {
                allocs: hir
                    .nodes
                    .iter()
                    .flat_map(node_alloc)
                    .map(|h| h.lir(lang))
                    .collect(),
                index: range.lir(lang),
                index_ty: range.bound.ty.lir(lang),
                bound: range.bound.val.lir(lang),
                body: body.lir(lang),
            },
            HStepExpr::Assume { cond, .. } => LStmt::Assume {
                cond: cond.val.lir(lang),
            },
        }
    }
}

impl<L: CodeLang> LirFromRc<HAtomDef, L> for LReadArg<L> {
    fn lir_from(atom: &HAtomDef, lang: &L) -> Self {
        LReadArg {
            decl: node_decl(&atom.node).map(|h| h.lir(lang)),
            expr: atom.node.lir(lang),
            ty: atom.ty.lir(lang),
        }
    }
}

impl<L: CodeLang> LirFromRc<HAtom, L> for LWriteArg<L> {
    fn lir_from(atom: &HAtom, lang: &L) -> Self {
        LWriteArg {
            ty: atom.ty.lir(lang),
            expr: atom.val.lir(lang),
        }
    }
}

impl<L: CodeLang> LirFromRc<HVarDef, L> for LDecl<L> {
    fn lir_from(var: &HVarDef, lang: &L) -> Self {
        LDecl {
            name: match &var.expr {
                HVarDefExpr::Name { name } => name.to_string(),
                _ => unreachable!(),
            },
            ty: var.ty.lir(lang),
        }
    }
}

impl<L: CodeLang> LirFromRc<HRange, L> for LDecl<L> {
    fn lir_from(range: &HRange, lang: &L) -> Self {
        LDecl {
            name: range.index.to_string(),
            ty: range.bound.val.ty.lir(lang),
        }
    }
}

impl<L: CodeLang> LirFrom<HAlloc, L> for LAlloc<L> {
    fn lir_from(alloc: &HAlloc, lang: &L) -> Self {
        LAlloc {
            decl: node_decl(&alloc.array).map(|h| h.lir(lang)),
            array: alloc.array.lir(lang),
            item_ty: alloc.item_ty.lir(lang),
            size: alloc.size.lir(lang),
        }
    }
}

impl<L: CodeLang> LirFromRc<HNodeDef, L> for LExpr<L> {
    fn lir_from(node: &HNodeDef, lang: &L) -> Self {
        match &node.expr {
            HNodeDefExpr::Var { var, .. } => LExpr::Var {
                name: match &var.expr {
                    HVarDefExpr::Name { name } => name.to_string(),
                    _ => unreachable!(),
                },
            },
            HNodeDefExpr::Subscript { array, index, .. } => LExpr::Subscript {
                array: Box::new(array.lir(lang)),
                index: Box::new(index.lir(lang)),
            },
            HNodeDefExpr::Err => unreachable!(),
        }
    }
}

impl<L: CodeLang> LirFromRc<HVal, L> for LExpr<L> {
    fn lir_from(val: &HVal, lang: &L) -> Self {
        match &val.expr {
            HValExpr::Var { name, .. } => LExpr::Var {
                name: name.to_string(),
            },
            HValExpr::Subscript { array, index, .. } => LExpr::Subscript {
                array: Box::new(array.lir(lang)),
                index: Box::new(index.lir(lang)),
            },
            HValExpr::Lit { value, .. } => LExpr::Lit {
                value: value.value_i64(),
            },
            HValExpr::Paren { inner, .. } => LExpr::Paren {
                inner: Box::new(inner.lir(lang)),
            },
            HValExpr::Mul { factors, .. } => LExpr::Mul {
                factors: factors.iter().map(|f| f.val.lir(lang)).collect(),
            },
            HValExpr::Sum { terms, .. } => LExpr::Sum {
                terms: terms
                    .iter()
                    .map(|(sign, t)| (sign.lir(lang), t.val.lir(lang)))
                    .collect(),
            },
            HValExpr::RelChain { rels } => LExpr::And {
                clauses: rels.iter().map(|x| x.lir(lang)).collect(),
            },
            HValExpr::Err => unreachable!(),
        }
    }
}

impl<L: CodeLang> LirFrom<HSign, L> for Option<LSign> {
    fn lir_from(sign: &HSign, _lang: &L) -> Self {
        match sign {
            HSign::Plus(None) => None,
            HSign::Plus(Some(_)) => Some(LSign::Plus),
            HSign::Minus(_) => Some(LSign::Minus),
        }
    }
}

impl<L: CodeLang> LirFromRc<HAtomTy, L> for AtomTy {
    fn lir_from(ty: &HAtomTy, _lang: &L) -> Self {
        ty.sem.unwrap()
    }
}

impl<L: CodeLang> LirFromRc<HValTy, L> for LTy<L> {
    fn lir_from(ty: &HValTy, lang: &L) -> Self {
        match ty {
            HValTy::Atom { atom_ty } => LTy::Atom {
                atom: atom_ty.lir(lang),
            },
            HValTy::Array { item, .. } => LTy::Array {
                item: Box::new(item.lir(lang)),
            },
            HValTy::Err => unreachable!(),
        }
    }
}

impl<L: CodeLang> LirFromRc<HFun, L> for LFun<L> {
    fn lir_from(fun: &HFun, lang: &L) -> Self {
        LFun {
            name: fun.name.to_string(),
            params: fun.args.iter().map(|p| p.lir(lang)).collect(),
            ret: fun.ret.as_ref().map(|r| r.ty.lir(lang)),
        }
    }
}

impl<L: CodeLang> LirFrom<HRel, L> for LExpr<L> {
    fn lir_from(rel: &HRel, lang: &L) -> Self {
        let (left, op, right) = rel;
        LExpr::Rel {
            left: Box::new(left.val.lir(lang)),
            op: op.lir(lang),
            right: Box::new(right.val.lir(lang)),
        }
    }
}

impl<L: CodeLang> LirFrom<RelOp, L> for RelOp {
    fn lir_from(op: &RelOp, _lang: &L) -> Self {
        op.clone()
    }
}

impl<L: CodeLang> LirFromRc<HArg, L> for LParam<L> {
    fn lir_from(arg: &HArg, lang: &L) -> Self {
        LParam {
            name: match &arg.expr {
                HArgExpr::Name { name } => name.to_string(),
                HArgExpr::Err => unreachable!(),
            },
            ty: arg.val.ty.lir(lang),
        }
    }
}

pub fn build_lir<L: CodeLang>(spec: &Rc<HSpec>, lang: &L) -> Lir<L, LSpec<L>> {
    spec.lir(lang)
}
