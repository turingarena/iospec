//! Low-level Intermediate Representation (LIR), used for code generation.
//!
//! LIR is meant to be directly translated into parser code.
//! It is a tree without references to higher level nodes (HIR).
//! Thus, it can be easily traversed structurally and translated into code.

use std::marker::PhantomData;
use std::rc::Rc;

use crate::atom::*;
use crate::spec::rel::RelOp;
use std::ops::Deref;

pub trait LirConfig: Clone {}

#[derive(Debug, Clone)]
pub struct Lir<C: LirConfig, T> {
    pub config: C,
    pub code: Rc<T>,
}

impl<C: LirConfig, T> Lir<C, T> {
    pub fn with_config<FF: LirConfig>(self: &Self, config: FF) -> Lir<FF, T> {
        Lir {
            code: self.code.clone(),
            config,
        }
    }
}

impl<C: LirConfig, T> AsRef<T> for Lir<C, T> {
    fn as_ref(self: &Self) -> &T {
        self.code.as_ref()
    }
}

impl<C: LirConfig, T> Deref for Lir<C, T> {
    type Target = T;

    fn deref(self: &Self) -> &Self::Target {
        self.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct LSpec<C: LirConfig> {
    pub funs: Vec<Lir<C, LFun<C>>>,
    pub main: Lir<C, LBlock<C>>,
}

#[derive(Debug, Clone)]
pub struct LFun<C: LirConfig> {
    pub name: String,
    pub params: Vec<Lir<C, LParam<C>>>,
    pub ret: Option<Lir<C, AtomTy>>,
}

#[derive(Debug, Clone)]
pub struct LParam<C: LirConfig> {
    pub name: String,
    pub ty: Lir<C, LTy<C>>,
}

#[derive(Debug, Clone)]
pub struct LBlock<C: LirConfig> {
    pub stmts: Vec<Lir<C, LStmt<C>>>,
}

#[derive(Debug, Clone)]
pub enum LStmt<C: LirConfig> {
    Read {
        args: Vec<Lir<C, LReadArg<C>>>,
    },
    Write {
        args: Vec<Lir<C, LWriteArg<C>>>,
    },
    Call {
        decl: Option<Lir<C, LDecl<C>>>,
        name: String,
        args: Vec<Lir<C, LExpr<C>>>,
        ret: Option<Lir<C, LExpr<C>>>,
    },
    For {
        allocs: Vec<Lir<C, LAlloc<C>>>,
        index: Lir<C, LDecl<C>>,
        index_ty: Lir<C, AtomTy>,
        bound: Lir<C, LExpr<C>>,
        body: Lir<C, LBlock<C>>,
    },
    Assume {
        cond: Lir<C, LExpr<C>>,
    },
}

#[derive(Debug, Clone)]
pub struct LDecl<C: LirConfig> {
    pub ty: Lir<C, LTy<C>>,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct LReadArg<C: LirConfig> {
    pub decl: Option<Lir<C, LDecl<C>>>,
    pub expr: Lir<C, LExpr<C>>,
    pub ty: Lir<C, AtomTy>,
}

#[derive(Debug, Clone)]
pub struct LWriteArg<C: LirConfig> {
    pub expr: Lir<C, LExpr<C>>,
    pub ty: Lir<C, AtomTy>,
}

#[derive(Debug, Clone)]
pub struct LAlloc<C: LirConfig> {
    pub decl: Option<Lir<C, LDecl<C>>>,
    pub array: Lir<C, LExpr<C>>,
    pub item_ty: Lir<C, LTy<C>>,
    pub size: Lir<C, LExpr<C>>,
}

#[derive(Debug, Clone)]
pub enum LExpr<C: LirConfig> {
    Var {
        name: String,
    },
    Subscript {
        array: Box<Lir<C, LExpr<C>>>,
        index: Box<Lir<C, LExpr<C>>>,
    },
    Lit {
        value: i64,
    },
    Paren {
        inner: Box<Lir<C, LExpr<C>>>,
    },
    Mul {
        factors: Vec<Lir<C, LExpr<C>>>,
    },
    Sum {
        terms: Vec<(Lir<C, Option<LSign<C>>>, Lir<C, LExpr<C>>)>,
    },
    Rel {
        left: Box<Lir<C, LExpr<C>>>,
        op: Lir<C, RelOp>,
        right: Box<Lir<C, LExpr<C>>>,
    },
    And {
        clauses: Vec<Lir<C, LExpr<C>>>,
    },
}

#[derive(Debug, Clone)]
pub enum LSign<C: LirConfig> {
    Plus(PhantomData<C>),
    Minus(PhantomData<C>),
}

#[derive(Debug, Clone)]
pub enum LTy<C: LirConfig> {
    Atom { atom: Lir<C, AtomTy> },
    Array { item: Box<Lir<C, LTy<C>>> },
}
