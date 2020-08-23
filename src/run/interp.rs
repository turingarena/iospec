use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::ops::Deref;

use crate::spec::hir::*;
use crate::spec::hir_quote::quote_hir;
use crate::spec::hir_sem::*;

use super::atom::*;
use super::ctx::*;
use super::io::*;
use super::val::*;

#[derive(Debug, Default)]
pub struct RState {
    env: HashMap<*const HVarDef, RNode>,
    indexes: HashMap<*const HRange, usize>,
}

impl HSpec {
    pub fn run<C: RunContext>(
        self: &Self,
        state: &mut RState,
        ctx: &mut C,
    ) -> Result<(), Box<dyn Error>> {
        self.main.run(state, ctx)
    }
}

impl HStep {
    fn run<C: RunContext>(
        self: &Self,
        state: &mut RState,
        ctx: &mut C,
    ) -> Result<(), Box<dyn Error>> {
        match &self.expr {
            HStepExpr::Seq { steps, .. } => {
                for step in steps {
                    step.run(state, ctx)?
                }
            }
            HStepExpr::Read { args, .. } => {
                for arg in args {
                    if let Some(var) = node_decl(&arg.node) {
                        decl(&var, state);
                    }

                    let val = ctx.input_source().next_atom(&arg.ty.sem)?;

                    eprintln!("READ  {} <- {}", quote_hir(arg.as_ref()), val);

                    arg.eval(state).set(Some(val));
                }
            }
            HStepExpr::Write { args, .. } => {
                for arg in args {
                    let val = ctx.output_source().next_atom(&arg.ty.sem)?;

                    eprintln!("WRITE {} <- {}", quote_hir(arg.as_ref()), val);

                    arg.val.eval_mut_atom(state).set(Some(val));
                }
            }
            HStepExpr::Call { fun, .. } => {
                if let Some(ret) = &fun.ret {
                    if let Some(var) = node_decl(&ret.node) {
                        decl(&var, state);
                    }
                }
            }
            HStepExpr::For { range, body, .. } => {
                for node in self.nodes.iter() {
                    if let Some(var) = node_decl(&node) {
                        decl(&var, state);
                    }

                    if let Some(alloc) = node_alloc(&node) {
                        alloc.run(state);
                    }
                }

                let bound = range.bound.val.eval_index(state);
                for i in 0..bound {
                    state.indexes.insert(Rc::as_ptr(&range), i);
                    body.run(state, ctx)?;
                }
                state.indexes.remove(&Rc::as_ptr(&range));
            }
        }
        Ok(())
    }
}

impl HAtomDef {
    fn eval<'a>(self: &Self, state: &'a mut RState) -> &'a mut dyn RAtomCell {
        self.node.eval_atom(state)
    }
}

impl HNodeDef {
    fn eval_atom<'a>(self: &Self, state: &'a mut RState) -> &'a mut dyn RAtomCell {
        match &self.expr {
            HNodeDefExpr::Var { var } => match state.env.get_mut(&Rc::as_ptr(&var)).unwrap() {
                RNode::Atom(val) => &mut **val,
                _ => unreachable!(),
            },
            HNodeDefExpr::Subscript { array, index, .. } => {
                let index = index.eval_index(state);
                match array.eval_aggr(state) {
                    RAggr::AtomArray(array) => array.at_mut(index),
                    _ => unreachable!(),
                }
            }
            HNodeDefExpr::Err => unreachable!(),
        }
    }

    fn eval_aggr<'a>(self: &Self, state: &'a mut RState) -> &'a mut RAggr {
        match &self.expr {
            HNodeDefExpr::Var { var } => match state.env.get_mut(&Rc::as_ptr(&var)).unwrap() {
                RNode::Aggr(aggr) => aggr,
                _ => unreachable!(),
            },
            HNodeDefExpr::Subscript { array, index, .. } => {
                // TODO: should be inverted, but the borrow checker is not happy about that
                let index = index.eval_index(state);
                let array = array.eval_aggr(state);

                match array {
                    RAggr::AggrArray(array) => &mut array[index],
                    _ => unreachable!(),
                }
            }
            HNodeDefExpr::Err => unreachable!(),
        }
    }
}

impl HVal {
    fn eval<'a>(self: &Self, state: &'a RState) -> RVal<'a> {
        match &self.expr {
            HValExpr::Var { var, .. } => match &var.expr {
                HVarExpr::Data { def } => match state.env.get(&Rc::as_ptr(def)).unwrap() {
                    RNode::Atom(cell) => RVal::Atom(cell.get().unwrap()),
                    RNode::Aggr(ref aggr) => RVal::Aggr(aggr),
                },
                HVarExpr::Index { range } => {
                    RVal::Atom(*state.indexes.get(&Rc::as_ptr(range)).unwrap() as i64)
                }
                HVarExpr::Err => unreachable!(),
            },
            HValExpr::Subscript { array, index, .. } => {
                let index = match index.eval(state) {
                    RVal::Atom(val) => val as usize,
                    _ => unreachable!(),
                };
                match array.eval(state) {
                    RVal::Aggr(RAggr::AtomArray(array)) => {
                        RVal::Atom(array.at(index).get().unwrap())
                    }
                    RVal::Aggr(RAggr::AggrArray(vec)) => RVal::Aggr(&vec[index]),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn eval_atom(self: &Self, state: &RState) -> i64 {
        let val = match self.eval(state) {
            RVal::Atom(val) => val,
            _ => unreachable!(),
        };
        assert_ne!(val, i64::MIN);
        val
    }

    fn eval_index(self: &Self, state: &RState) -> usize {
        self.eval_atom(state) as usize
    }

    fn eval_mut<'a>(self: &Self, state: &'a mut RState) -> RValMut<'a> {
        match &self.expr {
            HValExpr::Var { var, .. } => match &var.expr {
                HVarExpr::Data { def } => match state.env.get_mut(&Rc::as_ptr(def)).unwrap() {
                    RNode::Atom(ref mut cell) => RValMut::Atom(&mut **cell),
                    RNode::Aggr(ref mut aggr) => RValMut::Aggr(aggr),
                },
                _ => unreachable!(),
            },
            HValExpr::Subscript { array, index, .. } => {
                let index = match index.eval(state) {
                    RVal::Atom(val) => val as usize,
                    _ => unreachable!(),
                };
                match array.eval_mut(state) {
                    RValMut::Aggr(RAggr::AtomArray(array)) => RValMut::Atom(array.at_mut(index)),
                    RValMut::Aggr(RAggr::AggrArray(vec)) => RValMut::Aggr(&mut vec[index]),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn eval_mut_atom<'a>(self: &Self, state: &'a mut RState) -> &'a mut dyn RAtomCell {
        match self.eval_mut(state) {
            RValMut::Atom(val) => val,
            _ => unreachable!(),
        }
    }
}

impl HValTy {
    fn decl(self: &Self, _state: &RState) -> RNode {
        match self {
            HValTy::Atom { atom_ty } => RNode::Atom(atom_ty.sem.cell()),
            _ => RNode::Aggr(RAggr::Uninit),
        }
    }

    fn alloc(self: &Self, state: &RState) -> RAggr {
        match self {
            HValTy::Array { item, range } => {
                let len = range.bound.val.eval_atom(state) as usize;

                match item.deref() {
                    HValTy::Atom { atom_ty } => RAggr::AtomArray(atom_ty.sem.array(len)),
                    _ => RAggr::AggrArray({
                        let mut vec = Vec::with_capacity(len);
                        for _ in 0..len {
                            vec.push(RAggr::Uninit)
                        }
                        vec
                    }),
                }
            }
            _ => unreachable!(),
        }
    }
}

fn decl(var: &Rc<HVarDef>, state: &mut RState) {
    state.env.insert(Rc::as_ptr(&var), var.ty.decl(state));
}

impl HAlloc {
    fn run(self: &Self, state: &mut RState) {
        *self.array.eval_aggr(state) = self.array.ty.alloc(state)
    }
}
