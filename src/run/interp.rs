use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::fs::File;
use std::ops::Deref;

use num_traits::{Bounded, Num, NumCast};

use crate::spec::hir::*;
use crate::spec::hir_quote::quote_hir;
use crate::spec::hir_sem::*;
use crate::spec::ty::*;

use super::io::*;

#[derive(Debug, Clone, Copy, Default)]
struct RAtomError;

impl std::fmt::Display for RAtomError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.write_str("Cannot parse atom")
    }
}

/// Compact representation of an atom, to use in array cells
trait RAtom: Clone + Copy + Debug + Num + Bounded + NumCast {}

impl RAtom for i8 {}

impl RAtom for i16 {}

impl RAtom for i32 {}

impl RAtom for i64 {}

trait RAtomCell: Debug {
    fn get(self: &Self) -> i64;
    fn set(self: &mut Self, value: i64);
}

impl<T: RAtom> RAtomCell for T {
    fn get(self: &Self) -> i64 {
        if *self == Self::min_value() {
            i64::MIN
        } else {
            (*self).to_i64().unwrap()
        }
    }

    fn set(self: &mut Self, value: i64) {
        assert_ne!(value, i64::MIN);
        *self = if value == i64::MIN {
            Self::min_value()
        } else {
            <T as NumCast>::from(value).unwrap()
        }
    }
}

trait RAtomArray: Debug {
    fn at(self: &Self, index: usize) -> &dyn RAtomCell;
    fn at_mut(self: &mut Self, index: usize) -> &mut dyn RAtomCell;
}

impl<T: RAtom> RAtomArray for Vec<T> {
    fn at(self: &Self, index: usize) -> &dyn RAtomCell {
        &self[index]
    }

    fn at_mut(self: &mut Self, index: usize) -> &mut dyn RAtomCell {
        &mut self[index]
    }
}

#[derive(Debug)]
enum RNode {
    Atom(Box<dyn RAtomCell>),
    Aggr(RAggr),
}

#[derive(Debug)]
enum RAggr {
    AtomArray(Box<dyn RAtomArray>),
    AggrArray(Vec<RAggr>),
    Uninit,
}

#[derive(Clone, Copy, Debug)]
enum RVal<'a> {
    Atom(i64),
    Aggr(&'a RAggr),
}

#[derive(Debug)]
enum RValMut<'a> {
    Atom(&'a mut dyn RAtomCell),
    Aggr(&'a mut RAggr),
}

#[derive(Debug, Default)]
pub struct RState {
    env: HashMap<*const HVarDef, RNode>,
    indexes: HashMap<*const HRange, usize>,
}

pub trait RunContext {
    type InputStream: AtomStream;
    type OutputStream: AtomStream;

    fn input_parser(self: &mut Self) -> &mut Self::InputStream;
    fn output_parser(self: &mut Self) -> &mut Self::OutputStream;
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

                    let val = ctx.input_parser().next_atom(&arg.ty.sem)?;

                    eprintln!("READ  {} <- {}", quote_hir(arg.as_ref()), val);

                    arg.eval(state).set(val);
                }
            }
            HStepExpr::Write { args, .. } => {
                for arg in args {
                    let val = ctx.output_parser().next_atom(&arg.ty.sem)?;

                    eprintln!("WRITE {} <- {}", quote_hir(arg.as_ref()), val);

                    arg.val.eval_mut_atom(state).set(val);
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

impl AtomTy {
    fn cell(self: &Self) -> Box<dyn RAtomCell> {
        match self {
            AtomTy::Bool => Box::new(i8::MIN),
            AtomTy::Nat { size } | AtomTy::Int { size } => match size {
                BitSize::S8 => Box::new(i8::MIN),
                BitSize::S16 => Box::new(i16::MIN),
                BitSize::S32 => Box::new(i32::MIN),
                BitSize::S64 => Box::new(i64::MIN),
            },
            AtomTy::Err => unreachable!(),
        }
    }

    fn array(self: &Self, len: usize) -> Box<dyn RAtomArray> {
        match self {
            AtomTy::Bool => Box::new(vec![i8::MIN; len]),
            AtomTy::Nat { size } | AtomTy::Int { size } => match size {
                BitSize::S8 => Box::new(vec![i8::MIN; len]),
                BitSize::S16 => Box::new(vec![i16::MIN; len]),
                BitSize::S32 => Box::new(vec![i32::MIN; len]),
                BitSize::S64 => Box::new(vec![i64::MIN; len]),
            },
            AtomTy::Err => unreachable!(),
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

impl HVal {
    fn eval<'a>(self: &Self, state: &'a RState) -> RVal<'a> {
        match &self.expr {
            HValExpr::Var { var, .. } => match &var.expr {
                HVarExpr::Data { def } => match state.env.get(&Rc::as_ptr(def)).unwrap() {
                    RNode::Atom(cell) => RVal::Atom(cell.get()),
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
                    RVal::Aggr(RAggr::AtomArray(array)) => RVal::Atom(array.at(index).get()),
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

fn decl(var: &Rc<HVarDef>, state: &mut RState) {
    state.env.insert(Rc::as_ptr(&var), var.ty.decl(state));
}

impl HAlloc {
    fn run(self: &Self, state: &mut RState) {
        *self.array.eval_aggr(state) = self.array.ty.alloc(state)
    }
}

pub struct Runner {
    pub input_parser: TextAtomStream<File>,
    pub output_parser: TextAtomStream<File>,
}

impl RunContext for Runner {
    type InputStream = TextAtomStream<File>;
    type OutputStream = TextAtomStream<File>;

    fn input_parser(self: &mut Self) -> &mut Self::InputStream {
        &mut self.input_parser
    }

    fn output_parser(self: &mut Self) -> &mut Self::OutputStream {
        &mut self.output_parser
    }
}
