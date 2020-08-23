use std::ops::Deref;

use crate::spec::hir::*;
use crate::spec::hir_quote::quote_hir;
use crate::spec::hir_sem::*;

use super::atom::*;
use super::ctx::*;
use super::err::*;
use super::io::*;
use super::state::*;
use super::val::*;

impl HSpec {
    pub fn run<C: RunContext>(self: &Self, state: &mut RState, ctx: &mut C) -> Result<(), RError> {
        self.main.run(state, ctx)
    }
}

impl HStep {
    fn run<C: RunContext>(self: &Self, state: &mut RState, ctx: &mut C) -> Result<(), RError> {
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

                    let val = ctx
                        .input_source()
                        .next_atom(&arg.ty.sem)
                        .map_err(|e| RError::InputSource {
                            def: arg.clone(),
                            cause: AtomSourceError::Parse(e),
                        })?
                        .ok_or_else(|| RError::InputSource {
                            def: arg.clone(),
                            cause: AtomSourceError::End,
                        })?;

                    eprintln!("READ  {} <- {}", quote_hir(arg.as_ref()), val);

                    arg.eval_mut(state)?.set(Some(val));
                }
            }
            HStepExpr::Write { args, .. } => {
                for arg in args {
                    let val = ctx
                        .output_source()
                        .next_atom(&arg.ty.sem)
                        .map_err(|e| RError::OutputSource {
                            atom: arg.clone(),
                            cause: AtomSourceError::Parse(e),
                        })?
                        .ok_or_else(|| RError::OutputSource {
                            atom: arg.clone(),
                            cause: AtomSourceError::End,
                        })?;

                    eprintln!("WRITE {} <- {}", quote_hir(arg.as_ref()), val);

                    if let Some(atom) = HVal::eval_atom_mut(&arg.val, state)? {
                        atom.set(Some(val));
                    }
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
                let bound = HVal::eval_index(&range.bound.val, state)?;

                for node in self.nodes.iter() {
                    if let Some(var) = node_decl(&node) {
                        decl(&var, state);
                    }

                    if let Some(alloc) = node_alloc(&node) {
                        *alloc.array.eval_aggr_mut(state)? = alloc.array.ty.alloc(bound, state);
                    }
                }

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
    fn eval_mut<'a>(self: &Self, state: &'a mut RState) -> Result<&'a mut dyn RAtomCell, RError> {
        self.node.eval_atom_mut(state)
    }
}

impl HNodeDef {
    fn eval_atom_mut<'a>(
        self: &Self,
        state: &'a mut RState,
    ) -> Result<&'a mut dyn RAtomCell, RError> {
        Ok(match &self.expr {
            HNodeDefExpr::Var { var } => match state.env.get_mut(&Rc::as_ptr(&var)).unwrap() {
                RNode::Atom(val) => &mut **val,
                _ => unreachable!(),
            },
            HNodeDefExpr::Subscript { array, index, .. } => {
                let index = HVal::eval_index(&index, state)?;
                match array.eval_aggr_mut(state)? {
                    RAggr::AtomArray(array) => array.at_mut(index),
                    _ => unreachable!(),
                }
            }
            HNodeDefExpr::Err => unreachable!(),
        })
    }

    fn eval_aggr_mut<'a>(self: &Self, state: &'a mut RState) -> Result<&'a mut RAggr, RError> {
        Ok(match &self.expr {
            HNodeDefExpr::Var { var } => match state.env.get_mut(&Rc::as_ptr(&var)).unwrap() {
                RNode::Aggr(aggr) => aggr,
                _ => unreachable!(),
            },
            HNodeDefExpr::Subscript { array, index, .. } => {
                // TODO: should be inverted, but the borrow checker is not happy about that
                let index = HVal::eval_index(index, state)?;
                let array = Self::eval_aggr_mut(array, state)?;

                match array {
                    RAggr::AggrArray(array) => &mut array[index],
                    _ => unreachable!(),
                }
            }
            HNodeDefExpr::Err => unreachable!(),
        })
    }
}

impl HVal {
    fn eval<'a>(val: &Rc<Self>, state: &'a RState) -> Result<RVal<'a>, RError> {
        Ok(match &val.expr {
            HValExpr::Var { var, .. } => match &var.expr {
                HVarExpr::Data { def } => match state.env.get(&Rc::as_ptr(def)).unwrap() {
                    RNode::Atom(cell) => RVal::Atom(
                        cell.get()
                            .ok_or_else(|| RError::UnresolvedVal { val: val.clone() })?,
                    ),
                    RNode::Aggr(ref aggr) => RVal::Aggr(aggr),
                },
                HVarExpr::Index { range } => {
                    RVal::Atom(*state.indexes.get(&Rc::as_ptr(range)).unwrap() as i64)
                }
                HVarExpr::Err => unreachable!(),
            },
            HValExpr::Subscript { array, index, .. } => {
                let index = match HVal::eval(index, state)? {
                    RVal::Atom(val) => val as usize,
                    _ => unreachable!(),
                };
                match HVal::eval(array, state)? {
                    RVal::Aggr(RAggr::AtomArray(array)) => RVal::Atom(
                        array
                            .at(index)
                            .get()
                            .ok_or_else(|| RError::UnresolvedVal { val: val.clone() })?,
                    ),
                    RVal::Aggr(RAggr::AggrArray(vec)) => RVal::Aggr(&vec[index]),
                    _ => unreachable!(),
                }
            }
        })
    }

    fn eval_atom(val: &Rc<Self>, state: &RState) -> Result<i64, RError> {
        let val = match Self::eval(val, state)? {
            RVal::Atom(val) => val,
            _ => unreachable!(),
        };
        Ok(val)
    }

    fn eval_index(val: &Rc<Self>, state: &RState) -> Result<usize, RError> {
        Ok(Self::eval_atom(val, state)? as usize)
    }

    fn eval_mut<'a>(val: &Rc<Self>, state: &'a mut RState) -> Result<RValMut<'a>, RError> {
        Ok(match &val.expr {
            HValExpr::Var { var, .. } => match &var.expr {
                HVarExpr::Data { def } => match state.env.get_mut(&Rc::as_ptr(def)).unwrap() {
                    RNode::Atom(ref mut cell) => RValMut::Atom(&mut **cell),
                    RNode::Aggr(ref mut aggr) => RValMut::Aggr(aggr),
                },
                HVarExpr::Index { .. } => RValMut::NotMut,
                _ => unreachable!(),
            },
            HValExpr::Subscript { array, index, .. } => {
                let index = match HVal::eval(index, state)? {
                    RVal::Atom(val) => val as usize,
                    _ => unreachable!(),
                };
                match HVal::eval_mut(array, state)? {
                    RValMut::Aggr(RAggr::AtomArray(array)) => RValMut::Atom(array.at_mut(index)),
                    RValMut::Aggr(RAggr::AggrArray(vec)) => RValMut::Aggr(&mut vec[index]),
                    _ => unreachable!(),
                }
            }
        })
    }

    fn eval_atom_mut<'a>(
        val: &Rc<Self>,
        state: &'a mut RState,
    ) -> Result<Option<&'a mut dyn RAtomCell>, RError> {
        Ok(match Self::eval_mut(val, state)? {
            RValMut::Atom(val) => Some(val),
            RValMut::NotMut => None,
            _ => unreachable!(),
        })
    }
}

impl HValTy {
    fn decl(self: &Self, _state: &RState) -> RNode {
        match self {
            HValTy::Atom { atom_ty } => RNode::Atom(atom_ty.sem.cell()),
            _ => RNode::Aggr(RAggr::Uninit),
        }
    }

    fn alloc(self: &Self, len: usize, state: &RState) -> RAggr {
        match self {
            HValTy::Array { item, range } => match item.deref() {
                HValTy::Atom { atom_ty } => RAggr::AtomArray(atom_ty.sem.array(len)),
                _ => RAggr::AggrArray({
                    let mut vec = Vec::with_capacity(len);
                    for _ in 0..len {
                        vec.push(RAggr::Uninit)
                    }
                    vec
                }),
            },
            _ => unreachable!(),
        }
    }
}

fn decl(var: &Rc<HVarDef>, state: &mut RState) {
    state.env.insert(Rc::as_ptr(&var), var.ty.decl(state));
}
