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

pub trait LirFlavor {}

#[derive(Debug, Clone)]
pub struct Lir<F: LirFlavor, T> {
    pub flavor: PhantomData<F>,
    pub code: Rc<T>,
}

impl<F: LirFlavor, T> From<T> for Lir<F, T> {
    fn from(code: T) -> Self {
        Self {
            code: Rc::new(code),
            flavor: PhantomData,
        }
    }
}

impl<F: LirFlavor, T> Lir<F, T> {
    pub fn in_flavor<FF: LirFlavor>(self: &Self) -> Lir<FF, T> {
        Lir {
            code: self.code.clone(),
            flavor: PhantomData,
        }
    }
}

impl<F: LirFlavor, T> AsRef<T> for Lir<F, T> {
    fn as_ref(self: &Self) -> &T {
        self.code.as_ref()
    }
}

impl<F: LirFlavor, T> Deref for Lir<F, T> {
    type Target = T;

    fn deref(self: &Self) -> &Self::Target {
        self.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct LSpec<F: LirFlavor> {
    pub funs: Vec<Lir<F, LFun<F>>>,
    pub main: Lir<F, LBlock<F>>,
}

#[derive(Debug, Clone)]
pub struct LFun<F: LirFlavor> {
    pub name: String,
    pub params: Vec<Lir<F, LParam<F>>>,
    pub ret: Option<Lir<F, AtomTy>>,
}

#[derive(Debug, Clone)]
pub struct LParam<F: LirFlavor> {
    pub name: String,
    pub ty: Lir<F, LTy<F>>,
}

#[derive(Debug, Clone)]
pub struct LBlock<F: LirFlavor> {
    pub stmts: Vec<Lir<F, LStmt<F>>>,
}

#[derive(Debug, Clone)]
pub enum LStmt<F: LirFlavor> {
    Read {
        args: Vec<Lir<F, LReadArg<F>>>,
    },
    Write {
        args: Vec<Lir<F, LWriteArg<F>>>,
    },
    Call {
        decl: Option<Lir<F, LDecl<F>>>,
        name: String,
        args: Vec<Lir<F, LExpr<F>>>,
        ret: Option<Lir<F, LExpr<F>>>,
    },
    For {
        allocs: Vec<Lir<F, LAlloc<F>>>,
        index: Lir<F, LDecl<F>>,
        index_ty: Lir<F, AtomTy>,
        bound: Lir<F, LExpr<F>>,
        body: Lir<F, LBlock<F>>,
    },
    Assume {
        cond: Lir<F, LExpr<F>>,
    },
}

#[derive(Debug, Clone)]
pub struct LDecl<F: LirFlavor> {
    pub ty: Lir<F, LTy<F>>,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct LReadArg<F: LirFlavor> {
    pub decl: Option<Lir<F, LDecl<F>>>,
    pub expr: Lir<F, LExpr<F>>,
    pub ty: Lir<F, AtomTy>,
}

#[derive(Debug, Clone)]
pub struct LWriteArg<F: LirFlavor> {
    pub expr: Lir<F, LExpr<F>>,
    pub ty: Lir<F, AtomTy>,
}

#[derive(Debug, Clone)]
pub struct LAlloc<F: LirFlavor> {
    pub decl: Option<Lir<F, LDecl<F>>>,
    pub array: Lir<F, LExpr<F>>,
    pub item_ty: Lir<F, LTy<F>>,
    pub size: Lir<F, LExpr<F>>,
}

#[derive(Debug, Clone)]
pub enum LExpr<F: LirFlavor> {
    Var {
        name: String,
    },
    Subscript {
        array: Box<Lir<F, LExpr<F>>>,
        index: Box<Lir<F, LExpr<F>>>,
    },
    Lit {
        value: i64,
    },
    Paren {
        inner: Box<Lir<F, LExpr<F>>>,
    },
    Mul {
        factors: Vec<Lir<F, LExpr<F>>>,
    },
    Sum {
        terms: Vec<(Lir<F, Option<LSign<F>>>, Lir<F, LExpr<F>>)>,
    },
    Rel {
        left: Box<Lir<F, LExpr<F>>>,
        op: Lir<F, RelOp>,
        right: Box<Lir<F, LExpr<F>>>,
    },
    And {
        clauses: Vec<Lir<F, LExpr<F>>>,
    },
}

#[derive(Debug, Clone)]
pub enum LSign<F: LirFlavor> {
    Plus(PhantomData<F>),
    Minus(PhantomData<F>),
}

#[derive(Debug, Clone)]
pub enum LTy<F: LirFlavor> {
    Atom { atom: Lir<F, AtomTy> },
    Array { item: Box<Lir<F, LTy<F>>> },
}
